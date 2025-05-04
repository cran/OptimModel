exp_decay = function(theta, x)
{
  K = exp(theta[3])
  pred = theta[1] + theta[2]*exp(-K*x)

  return(pred)
}
attr(exp_decay, "backsolve") = function(theta, y, log=FALSE)
{
  out = -(1/exp(theta[3]))*log( (y-theta[1])/theta[2] )
  
  if ( log )
    out = log(out)
  
  return(out)
}
attr(exp_decay, "gradient") = function(theta, x)
{
  K = exp(theta[3])
  grad = matrix(NA, length(x), 3)
  grad[,1] = 1
  grad[,2] = exp(-K*x)
  grad[,3] = -theta[2]*exp(-K*x)*K*x

  return(grad)
}
attr(exp_decay, "start") = function(x, y)
{
  theta0 = rep(NA, 3)
  names(theta0) = c("A", "B", "k")

  A = min(y)
  delta = min( abs(A) )*.01
  logY = log( y-A+delta )
  linear.coef = as.vector(coef(lm( logY~x )))
  theta0["B"] = exp(linear.coef[1])
  theta0["k"] = -linear.coef[2]
  theta0["A"] = median(y-theta0["B"]*exp(-theta0["k"]*x))
  theta0["k"] = log( abs(theta0["k"]) )

  return(theta0)
}
