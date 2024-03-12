.optim.fit.mle = function(ols.fit, f.model, phi0, phi.fixed, x, y, w.func, optim.list)
{

  ## Identify the weights function.  Give starting values to phi if not supplied by phi0.
  if ( !inherits(w.func,"function" ))
    stop("MLE algorithm requires that 'wts' in optim.fit() be a function.")
    
  wt.type = attr(w.func, "label")
  if ( missing(phi0) && wt.type=="user.defined" )
    stop("Cannot proceed.  Supply optim.fit() with a value for 'phi0'.")
  phi.start = phi0

  if ( is.null(phi.start) )
    phi.start = switch(attr(w.func, "label"), "weights_varExp"={c(phi=.1)},
                  "weights_varPower"={c(phi=.5)}, "weights_varConstPower"={c(c=0, phi=.5)})
  theta0 = ols.fit$par
  p.th = length(theta0)
  txt <- deparse(.loglik)
  txt1 <- capture.output(dput(x))
  txt1 <- do.call('paste0',as.list(txt1))
  txt <- gsub('x = NA',paste0('x = ',txt1),txt)
  txt1 <- capture.output(dput(y))
  txt1 <- do.call('paste0',as.list(txt1))
  txt <- gsub('y = NA',paste0('y = ',txt1),txt)
  txt <- process_low_level_txt(txt,'f.model = NA',f.model)
  txt <- process_low_level_txt(txt,'w.func = NA',w.func)
  .loglik=eval(parse(text=txt))
  
  txt <- deparse(.f.mle)
  txt <- gsub('p.th = NA',paste0('p.th = ',p.th),txt)
  txt <- gsub('phi.start = NA',paste0('phi.start = ',phi.start),txt)
  txt <- gsub('phi.fixed = NA',paste0('phi.fixed = ',phi.fixed),txt)
  .f.mle=eval(parse(text=txt))

  ## Estimate starting values for log(sigma)
  mu = f.model(ols.fit$par, x)
  wts = w.func(phi.start, mu)^(-2)
  lsigma.start = log( sqrt( sum( (y-mu)^2*wts )/(length(y)-length(ols.fit$par)) ) )

  ## Starting values for MLE algorithm
  mle.start = as.vector(c( ols.fit$par, ifelse(phi.fixed, NA, phi.start), lsigma.start))
  mle.start = mle.start[!is.na(mle.start)]

  if ( optim.list$method=="L-BFGS-B" & length(optim.list$lower) > 1 )
  {
    optim.list$lower = c(optim.list$lower, rep(-Inf, length(mle.start)-length(theta0)))
    optim.list$upper = c(optim.list$upper, rep(Inf, length(mle.start)-length(theta0)))
  }

  fit = optim(mle.start, .f.mle, method=optim.list$method,
          lower=optim.list$lower, upper=optim.list$upper, hessian=TRUE,
          control=optim.list$control.list)

  if ( fit$converge > 0 )
    warning("MLE:  optim failed to converge.")
  if ( is.null(names(theta0)) )
    names(theta0) = paste("par.", 1:length(theta0), sep="")
  names(fit$par)[c(1:p.th, length(fit$par))] = c(names(theta0), "lsigma")

  phi = ifelse(phi.fixed, ifelse(is.null(phi.start), NA, phi.start), fit$par[(p.th+1):(length(fit$par)-1)])
  if ( !phi.fixed )
    names(fit$par)[(p.th+1):(length(fit$par)-1)] = paste("v", 1:length(phi), sep="")

  names(phi) = names(phi.start)
  attr(fit, "weights") = w.func(phi, f.model(fit$par, x))^(-2)
  attr(fit, "var.param") = phi
  attr(fit, "converge.mle") = fit$converge==0

  return(fit)
}

