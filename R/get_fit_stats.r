.get.fit.stats = function(fit, x, y, conf.level)
{
  ## Change 'par' to 'coef'
  names(fit)[ names(fit)=="par" ] = "coefficients"
  theta.hat = coef(fit)
  if ( fit$fit.method=="mle" )
  {
    fit$sigma = exp(theta.hat[length(theta.hat)])
    fit$varBeta = try( solve(fit$hessian), FALSE )
    phi.fixed = is.null(fit$call$phi.fixed) || fit$call$phi.fixed
    N.param = length(theta.hat) - 1 - ifelse(phi.fixed, 0, length(attr(fit, "var.param")))
    theta.hat = theta.hat[1:N.param]
  }
  f.model = eval(fit$call$f.model)
  fit$fitted = f.model(theta.hat, x)

  wts = attr(fit, "weights")
  fit$residuals = (y-fitted(fit))*sqrt(wts)
  fit$df = ifelse(fit$fit.method=="mle", Inf, sum(wts>0) - length(coef(fit)))
  fit$dims = list(p=length(coef(fit)), N=length(x))

  if ( fit$fit.method != "mle" )
  {
    fit$sigma = sqrt( sum( wts*(y-f.model(theta.hat, x))^2 )/fit$df )
  
    jacobian = attr(f.model, "gradient")
    if ( fit$var.method !="hessian" )
    {
      X.mat = if ( is.null(jacobian) )
                f2djac(f.model, coef(fit), x=x)*sqrt(wts)
              else
                jacobian(coef(fit), x)*sqrt(wts)

      xTx.inv = try( solve( t(X.mat)%*%X.mat ), TRUE )
      if ( !is.numeric(xTx.inv) )
      {
        fit$var.method = "hessian"
        warning("Switching var.method to 'hessian'")
      }
    }
    fit$varBeta = try(switch( fit$var.method,
            "robust"={ xTx.inv%*%t(X.mat*sqrt(wts)*fit$residuals)%*%(X.mat*sqrt(wts)*fit$residuals)%*%xTx.inv },
            "normal"={fit$sigma^2*xTx.inv },
            "hessian"={fit$sigma^2*solve(.5*fit$hessian)}), FALSE )
  }            

  if ( !is.numeric(fit$varBeta) )
  {  
    warning("Invalid variance-covariance matrix.")
    fit$varBeta = matrix(NA, length(coef(fit)), length(coef(fit)))
  }

  beta = data.frame( par = coef(fit), standard.error=sqrt(diag(fit$varBeta)), row.names=names(coef(fit)) )

  ## Estimate 100*conf.level% CI for parameter estimates
  beta$lower = beta$par - qt(.5*(1+conf.level), fit$df)*beta$standard.error
  beta$upper = beta$par + qt(.5*(1+conf.level), fit$df)*beta$standard.error
  attr(beta, "conf.level") = conf.level

  fit$beta = beta
  
  ## Compute r-squared
  ssto = sum( (y - mean(y))^2*wts )
#  sse = fit$sigma^2*fit$df
  sse = sum( wts*(y-f.model(theta.hat, x))^2 )
  fit$r.squared = as.vector( (ssto - sse)/ssto )

  ## Compute BIC (smaller is better)
  nparm = length(coef(fit))

  if ( attr(attr(fit, "w.func"), "label") %in% c("user.defined", "weights_tukey_bw", "weights_huber") )
    sig = fit$sigma/sqrt(attr(fit, "weights"))
  else
    sig = fit$sigma*attr(fit, "w.func")( attr(fit, "var.param"), fitted(fit) )

    ## log-likelihood as if fitted via MLE
  sig = sig*ifelse( fit$fit.method=="mle", 1, sqrt(fit$df/length(y)) )
  log.l = sum( dnorm(y, mean=fitted(fit), sd=sig, log=TRUE) )
  fit$bic = -2*log.l + log(length(y))*(nparm+1)   ## Number of parameters includes the standard deviation
  
  return(fit)
}

