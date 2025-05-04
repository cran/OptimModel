rout_outlier_test = function(fit, Q=0.01)
{
  ## ROUT outlier test method
  ## Q = test size

  if ( class(fit)[1] != "rout_fit" )
    stop("The 'fit' object must be of class 'rout_fit' from the function 'rout.fitter'.")

  ## Calculate p-value for value of standardized residuals
  N = length(fit$residuals) ## Number of observations
  p = length(fit$par) - 1   ## Number of model parameters, excluding the parameter for log(sigma)
  degFree = N-p
  pval = 2*pt( -abs(fit$sresiduals), df=degFree )
  pval.adj = p.adjust( pval, method="BH" )  ## Adjust p-values for multiplicity via BH method (=fdr)

  
  ## Determine outliers.  If p-value <= Q, point may be an outlier
  fit$outlier = pval <= Q
  fit$outlier.adj = pval.adj <= Q
  attr(fit, "Q") = Q

  return(fit)
}
