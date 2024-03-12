# Program:  beta.model.R
# Location:  s:/novick/R libraries/Hill Model/funs
# Version:  1
# Author:   Steven Novick
# Date:     March 24, 2021
# Purpose:  Beta (hook-effect) Model, gradient, and backsolve algorithms
#       Can be used with "Optim Model" (Novick) library

##  e0 = theta[1], ydelta = theta[2], log(delta1) = theta[3], log(delta2) = theta[4], log(delta3) = theta[5]
    
.beta.func = function(delta1, delta2, log=TRUE)
{
  ## Both delta1, delta2 > 0
  out = (delta1+delta2)*log(delta1+delta2) - delta1*log(delta1) - delta2*log(delta2)
  if ( !log )
    out = exp(out)
  return(out)
}
beta_model = function(theta, x)
{
  maxX = attr(theta, "maxX")
  if ( is.null(maxX) )
    maxX = max(x)
  
  delta1 = exp(theta[3])
  delta2 = exp(theta[4])
  sc = maxX+exp(theta[5])
  
  mu = theta[1] + theta[2]*exp(.beta.func(delta1, delta2, log=TRUE) + delta1*log(x) + delta2*log(sc-x) - (delta1+delta2)*log(sc) )
  return(mu)
}
attr(beta_model, "backsolve") = function(theta, y, log=FALSE)
{

  maxX = attr(theta, "maxX")
  if ( is.null(maxX) )
    stop("Must set attr(theta, 'maxX') = max(x)")

  delta1 = exp(theta[3])
  delta2 = exp(theta[4])
  sc = maxX+exp(theta[5])

  ## Find the x that yields a maximum
  x0 = delta1*sc/(delta1+delta2)

  ## Find the first "x" that provides a response of y
  f.ssq = function(x, theta, y) {
        (y - beta_model(theta, x))^2
    }
  out = optimize(f.ssq, interval = c(0, x0), theta = theta, y = y)$minimum
  
  if ( log )
    out = log(out)
  return(out)
}
attr(beta_model, "gradient") = function(theta, x)
{

  maxX = attr(theta, "maxX")
  if ( is.null(maxX) )
    maxX = max(x)
    
  delta1 = exp(theta[3])
  delta2 = exp(theta[4])
  sc = maxX+exp(theta[5])
  
  jac = matrix(0, length(x), 5)
  jac[,1] = 1

  index = x>0
  temp = exp(.beta.func(delta1, delta2, log=TRUE) + delta1*log(x[index]) + delta2*log(sc-x[index]) - (delta1+delta2)*log(sc) )

  jac[index,2] = temp

  jac[index,3] = theta[2]*temp*delta1*( log(delta1+delta2) - theta[3] + log(x[index]) - log(sc) )
  jac[index,4] = theta[2]*temp*delta2*( log(delta1+delta2) - theta[4] + log(sc - x[index]) - log(sc) )
  jac[index,5] = theta[2]*temp*exp(theta[5])*( delta2/(sc-x[index]) - (delta1+delta2)/sc )

  return(jac)
}
attr(beta_model, "start") = function(x, y)
{
    beta0 = rep(0, 5)
    names(beta0) = c("emin", "emax", "ldelta1", "ldelta2", "ldelta3")

    beta0[1] = mean(y[x==min(x)])
    lx = log(x+ifelse(any(x==0), min(x[x>0])/10, 0))
    fit0 = smooth.spline(lx, y)
    ypred = predict(fit0, lx)$y
    ssto = sum( (y-mean(y))^2 )
    sse = sum( (y-ypred)^2 )
    r2 = 1 - sse/ssto

    if ( r2 > 0.9 ){
      ii = which.max( abs(ypred-beta0[1]) )
      beta0[2] = ypred[ii] - beta0[1]
    }else{
      mu = tapply(y, x, mean)
      ii = which.max( abs(mu-beta0[1]) )
      beta0[2] = mu[ii] - beta0[1]
    }
    attr(beta0, "maxX") = max(x)

    return(beta0)

}
