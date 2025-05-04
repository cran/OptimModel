exp_decay_pl = function(theta, x)
{
  x0 = exp(theta[1])
  K = exp(theta[4])
  pred = ifelse( x <= x0, theta[2], theta[3] + (theta[2]-theta[3])*exp(-K*(x-x0)) )

  return(pred)
}
attr(exp_decay_pl, "backsolve") = function(theta, y, log=FALSE)
{
  x0 = exp(theta[1])
  K = exp(theta[4])
  out = ifelse( y >= theta[2], x0, x0-(1/K)*log( (y-theta[3])/(theta[2]-theta[3]) ) )
  
  if ( log )
    out = log(out)
    
  return(out)
}
attr(exp_decay_pl, "gradient") = function(theta, x)
{
  x0 = exp(theta[1])
  K = exp(theta[4])
  grad = matrix(0, length(x), 4)

  index = x > x0
  if ( any(!index) )
    grad[!index,2] = 1
  if ( any(index) )
  {
    t1 = exp(-K*(x[index]-x0))
    t2 = (theta[2]-theta[3])*t1*K
    grad[index,1] = t2*x0
    grad[index,2] = t1
    grad[index,3] = 1 - t1
    grad[index,4] = -t2*(x[index]-x0)
  }
  return(grad)
}
attr(exp_decay_pl, "start") = function(x, y)
{
  theta0 = rep(NA, 4)
  names(theta0) = c("x0", "yMax", "yMin", "k")

  yMin = min(y)
  yMax = max(y)

  index = which( y < yMax - .05*(yMax-yMin) )
  Ymax = median( y[1:index[1]] )
  x0 = index[1]

  delta = min( abs(yMin) )*.0001
  logY = log( (y-yMin)/(yMax-yMin)+delta )
  slope.coef = as.vector(coef(lm( logY~I(x0-x)-1, subset=x>x0 )))
  
  theta0["x0"] = log(x0)
  theta0["yMax"] = yMax+delta
  theta0["yMin"] = yMin-delta
  theta0["k"] = slope.coef

  return(theta0)
}

