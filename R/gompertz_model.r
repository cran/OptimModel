gompertz_model = function(theta, x)
{

  pred = theta[1] + (theta[2]-theta[1])*exp(-exp( theta[3]*(x-theta[4]) ))


  return(pred)
}
attr(gompertz_model, "backsolve") = function(theta, y, log=FALSE)
{
  out = theta[4] + (1/theta[3])*log( -log((y-theta[1])/(theta[2]-theta[1])) )
  if ( log )
    out = log(out)
    
  return(out)
}
attr(gompertz_model, "gradient") = function(theta, x)
{
  grad = matrix(NA, length(x), 4)
  t1 = exp(theta[3]*(x-theta[4]))
  t2 = exp(-t1)

  grad[,1] = 1 - t2
  grad[,2] = t2
  grad[,3] = -(theta[2]-theta[1])*t1*t2*(x-theta[4])
  grad[,4] = (theta[2]-theta[1])*t1*t2*theta[3]
  
  return(grad)
}
attr(gompertz_model, "start") = function(x, y)
{
  theta0 = rep(NA, 4)
  names(theta0) = c("A", "B", "m", "offset")
  yMin = .99*min(y)
  yMax = 1.01*max(y)
  yLL = log( -log( (y-yMin)/(yMax-yMin) ) )
  fit = lm( yLL~x )
  
  theta0[1] = yMin
  theta0[2] = yMax
  theta0[3] = as.vector(coef(fit)[2])
  theta0[4] = -as.vector(coef(fit)[1])/theta0[3]


  return(theta0)
}
