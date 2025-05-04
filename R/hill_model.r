# Program:  hill.model.R
# Location:  s:/novick/R libraries/Hill Model/funs
# Version:  1
# Author:   Steven Novick
# Date:     July 3, 2003
# Purpose:  Hill Model, gradient, and backsolve algorithms
#       Can be used with "Optim Model" (Novick) library

##  emin = theta[1], emax = theta[2], log(ec50) = theta[3], m = theta[4]
    
hill_model = function(theta, x){ theta[1] + (theta[2]-theta[1])/( 1 + exp( theta[4]*log(x) - theta[4]*theta[3]) ) }
attr(hill_model, "backsolve") = function(theta, y, log=FALSE)
{
  out = theta[3] + (1/theta[4])*log( (theta[2]-y)/(y-theta[1]) )
  if ( !log )
    out = exp(out)
  return(out)
}
attr(hill_model, "gradient") = function(theta, x)
{

    index = x>0
    exp.term = exp( theta[4]*log(x[index]) - theta[4]*theta[3] )
    temp = 1/( 1 + exp.term )
    jac = matrix(0, length(x), 4)

    jac[index,1] = 1 - temp
    jac[index,2] = temp
    jac[index,3] = temp*temp*(theta[2]-theta[1])*exp.term
    jac[index,4] = jac[index,3] * ( theta[3] - log(x[index]) )
    jac[index,3] = jac[index,3] * theta[4]

    if ( any(!index) )
    {
        if ( theta[4] < 0 )
            jac[!index,] = matrix(rep( c(1, 0, 0, 0), sum(!index) ), ncol=4, byrow=TRUE)
        else
            jac[!index,] = matrix(rep( c(0, 1, 0, 0), sum(!index) ), ncol=4, byrow=TRUE)
    }

    return(jac)
}

attr(hill_model, "start") = function(x, y)
{
    beta0 = rep(NA, 4)
    names(beta0) = c("emin", "emax", "lec50", "m")

    beta0[1] = mean( y[ x==min(x)] )
    beta0[2] = mean( y[ x==max(x)] )
    beta0[4] = ifelse( beta0[1] > beta0[2], 1, -1 )

    ymid = mean( beta0[1:2] )
    y.diff = abs(y-ymid)
    min.diff = min(y.diff)
    ymid = ( y[y.diff==min.diff] )[1]
    xmid = (x[y.diff==min.diff])[1]

    beta0[3] = log(  xmid/((beta0[1]-beta0[2])/(ymid-beta0[2])-1) )

    if ( is.infinite(beta0[3]) | is.na(beta0[3]) )
        beta0[3] = log( x[ which.min(abs(y-mean(beta0[1:2]))) ][1] )
    if ( is.infinite(beta0[3]) | is.na(beta0[3]) )
        beta0[3] = log( median(x[x>0]) )
    beta0[1:2] = sort( beta0[1:2] )

    return(beta0)

}
