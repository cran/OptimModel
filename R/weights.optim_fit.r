weights_varIdent = function(phi, mu){ rep(1, length(mu)) }
weights_varExp = function(phi, mu){ exp(phi*mu) }
weights_varPower = function(phi, mu){ abs(mu)^(phi) }
weights_varConstPower = function(phi, mu){ abs( phi[1] + abs(mu)^phi[2] ) }
weights_tukey_bw = function(phi = 4.685, resid)
{
  ## phi[1] = standard deviation, phi[2] = tuning constant (B=4.685), resid = model residuals
  sig = mad(resid, center=0)

  r = abs(resid)/sig
  wts = ifelse(r<=phi, (1-(r/phi)^2)^2, 0)
  return(wts)
}
weights_huber = function(phi=1.345, resid)
{
  ## phi = huber tuning constant, resid = model residuals
  sig = mad(resid, center=0)
  r = abs(resid)/sig
  wts = pmin(1, phi/r)
  
  return(wts)
}



