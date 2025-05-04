# Program:  hill.switchpoint.model.R
# Location:  s:/novick/R libraries/Hill Model/funs
# Version:  1
# Author:   Steven Novick
# Date:     July 3, 2003
# Purpose:  Hill Model, gradient, and backsolve algorithms
#       Can be used with "Optim Model" (Novick) library

##  emin = theta[1], emax = theta[2], log(ec50) = theta[3], m = theta[4]
    
hill_switchpoint_model = function(theta, x)
{
  sp = exp(theta[5])   ## switchpoint coefficient
  f.s = (sp-x)/(sp+x)

  mu = theta[1] + (theta[2] - theta[1])/(1 + exp(f.s*theta[4] * (log(x) - theta[3])))
  return(mu)
}
attr(hill_switchpoint_model, "backsolve") = function(theta, y, log=FALSE)
{
  rhs = (1/theta[4])*log( (theta[2]-y)/(y-theta[1]) )
  sp = exp(theta[5])   ## switchpoint coefficient
  fsq.opt = function(lx){  ((sp-exp(lx))/(sp+exp(lx))*( lx - theta[3] ) - rhs)^2 }
  
  lx.seq = seq(-100, 100, length=10000)
  fsq.seq = fsq.opt(lx.seq)
  o = order(fsq.seq)
  lx.5 = lx.seq[o[1:5]]
  fitx = list()
  for ( i in 1:5 )
    fitx[[i]] = optim(c(lx=lx.5[i]), fsq.opt, method="BFGS")
  fitx = fitx[sapply(fitx, function(f){ f$convergence==0 })]
  out = min(sapply(fitx, function(f){ f$par }))
  if ( !log )
    out = exp(out)
  return(out)
}
attr(hill_switchpoint_model, "gradient") = function(theta, x)
{

    sp = exp(theta[5])   ## switchpoint coefficient
    f.s = (sp-x)/(sp+x)

    index = x>0
    exp.term = exp(f.s[index]*theta[4] * (log(x[index]) - theta[3]))
    temp = 1/( 1 + exp.term )
    jac = matrix(0, length(x), 5)

    jac[index,1] = 1 - temp
    jac[index,2] = temp
    jac[index,3] = temp*temp*(theta[2]-theta[1])*exp.term
    jac[index,4] = jac[index,3] * f.s[index] * ( theta[3] - log(x[index]) )
    jac[index,5] = jac[index,3] * theta[4] * ( theta[3] - log(x[index]) )*2*x[index]*sp/(sp+x[index])^2    
    jac[index,3] = jac[index,3] * theta[4] * f.s[index]


    if ( any(!index) )
    {
        if ( theta[4] < 0 )
            jac[!index,] = matrix(rep( c(1, 0, 0, 0, 0), sum(!index) ), ncol=5, byrow=TRUE)
        else
            jac[!index,] = matrix(rep( c(0, 1, 0, 0, 0), sum(!index) ), ncol=5, byrow=TRUE)
    }

    return(jac)
}

attr(hill_switchpoint_model, "start") = function(x, y)
{
    beta0 = rep(NA, 5)

    beta0[1:4] = attr(hill_model, "start")(x, y)
    beta0[5] = 0
    beta0[1] = mean( y[ x==min(x)] )
    beta0[2] = mean( y[ x==max(x)] )
    beta0[1:2] = sort(beta0[1:2])


    names(beta0) = c("emin", "emax", "ec50", "m", "lsp")
    return(beta0)

}
