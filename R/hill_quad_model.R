hill_quad_model = function(theta, x)
{
  z = log(x)
  xbeta = theta[3] + theta[4]*z + theta[5]*z^2
  pred = theta[1]+(theta[2]-theta[1])/(1+exp(-xbeta))

  return(pred)
}
attr(hill_quad_model, "gradient") = function(theta, x)
{

  z = log(x)
  jac = matrix( 0, length(x), 5 )


  xbeta = theta[3] + theta[4]*z + theta[5]*z^2
  L =  1/(1+exp(-xbeta))

  index = z > -Inf
  jac[,1] = 1 - L
  jac[,2] = L
  jac[index,3] = (theta[2]-theta[1])*L[index]*L[index]*exp(-xbeta[index])
  jac[index,4] = jac[index,3]*z[index]
  jac[index,5] = jac[index,3]*z[index]^2

  return(jac)
}
attr(hill_quad_model, "backsolve") = function(theta, y, log=FALSE)
{
  ## Solve for positive root of quadratic formula.
  ## If two positive roots, take the smaller.
  ## Solving for x=exp(z) in:   theta[5]*z^2 + theta[4]*z + ( theta[3] - log(y-theta[1])/(theta[2]-y) ) = 0

  Const = theta[3] - log( (y-theta[1])/(theta[2]-y) )
  Discrim = theta[4]^2 - 4*theta[5]*Const

  if ( Discrim < 0 )
    out = rep(NA, 2)
  else
   out = sort( ( -theta[4] + c(-1, 1)*sqrt(Discrim) )/(2*theta[5]) )
   
  if ( !log )
    out = exp(out)

  return(out)
}
attr(hill_quad_model, "start") = function(x, y)
{
  ## Y = A + (B-A)/( 1 + exp(-(a+b*x+c*x^2)^2) )
  beta0 = rep(0, 5)
  names(beta0) = c("A", "B", "a", "b", "c")

  z = log(x)

  index = z > -Inf
  z = z[index]
  y = y[index]

  ymean = tapply(y, z, mean)
  unique.x = as.numeric(names(ymean))
  y.range = range(ymean)
    ## Push out (A, B) so that y.range[2]-y.range[1] = 0.9*(B-A)
  beta0[1] = (19*y.range[1] - y.range[2])/18
  beta0[2] = (19*y.range[2] - y.range[1])/18

  
  y.st = (ymean-beta0[1])/(beta0[2]-beta0[1])
  y.stL = log( y.st/(1-y.st) )

  ## Given asymptotes (A, B), solve for (a, b, c)
  beta0[3:5] = as.vector( coef( lm(y.stL~poly(unique.x, 2, raw=TRUE)) ) )
  
  ## Given (a, b, c), update A and B
  ii = c( which.min(ymean), which.max(ymean) )
  xbeta = beta0[3] + beta0[4]*unique.x[ii] + beta0[5]*unique.x[ii]^2
  L =  1/(1+exp(-xbeta))
  x1.ii = 1-L
  x2.ii = L
  beta0[1:2] = as.vector( coef(lm( ymean[ii]~x1.ii+x2.ii-1 )) )

  ## Give (A, B), update a, b, and c
  y.st = (ymean-beta0[1])/(beta0[2]-beta0[1])
  y.stL = log( y.st/(1-y.st) )
  beta0[3:5] = as.vector( coef( lm(y.stL~poly(unique.x, 2, raw=TRUE)) ) )

  return(beta0)
}

