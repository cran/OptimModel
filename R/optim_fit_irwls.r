.optim.fit.irwls = function(ols.fit, f.model, gr.model, phi0, phi.fixed, x, y, w.func, optim.list)
{

  ## Identify the weights function.  Give starting values to phi if not supplied by phi0.
  wt.type = attr(w.func, "label")
  sig <- 1 ## Todo: ask about this?
  if ( wt.type=="weights_tukey_bw" )
  {
    phi.fixed=TRUE
    if ( is.null(phi0) )
    {
#      sig = try(1.25*sqrt( IQR(sapply(split(y, x), var)) ))    
#      if ( class(sig)[1]=="try-error" )
#        stop("Cannot proceed without an estimate of sigma.  Supply optim.fit() with 'phi0=c(sigma, B)'.")
#      phi0 = c(sig, 4.685)
       phi0 = 4.685  ## Tukey tuning constant
    }
  }
  if ( wt.type=="weights_huber" )
  {
    phi.fixed=TRUE
    if ( is.null(phi0) )
      phi0 = 1.345  ## Huber tuning constant
  }  
  if ( missing(phi0) && wt.type=="user.defined" )
    stop("Cannot proceed.  Supply optim.fit() with a value for 'phi0'.")

  if ( !is.null(phi0) )
    phi.start = phi0
  else
    phi.start = switch(attr(w.func, "label"), "weights_varExp"={c(phi=.1)},
                  "weights_varPower"={c(phi=.5)}, "weights_varConstPower"={c(c=0, phi=.5)},
                  "weights_tukey_bw"={c(sig=sig, B=4.685)}, "weights_huber"={c(B=1.345)}, {NULL})

  theta0 = ols.fit$par
  p.th = length(theta0)

  txt <- deparse(.f.ssq)
  txt <- process_low_level_txt(txt,'f.model = NA',f.model)
  .f.ssq=eval(parse(text=txt))
  if ( is.null(gr.model) ){
	.gr.ssq = NULL
  }else{
	  txt <- deparse(.gr.ssq)
	  txt <- process_low_level_txt(txt,'f.model = NA',f.model)
	  txt <- process_low_level_txt(txt,'gr.model = NA',gr.model)
	  .gr.ssq=eval(parse(text=txt))
  }
  
  phi = phi.start
  converge.pls = ifelse(phi.fixed, TRUE, FALSE)
  fit = ols.fit
  .trkfunc_original <- deparse(.trkfunc)
  for ( k in 1:10 )
  {
    mu = f.model(fit$par, x)    ## Used in estimation of wts  (estimation of phi)
    if ( !phi.fixed )
    {
      phi.lag = phi
      resid = as.vector(y - mu)       ## Used in trkfunc  (estimation of phi)      
	  txt <- .trkfunc_original
	  txt1 <- capture.output(dput(mu))
	  txt1 <- do.call('paste0',as.list(txt1))
	  txt <- gsub('mu = NA',paste0('mu = ',txt1),txt)
	  txt1 <- capture.output(dput(resid))
	  txt1 <- do.call('paste0',as.list(txt1))
	  txt <- gsub('resid = NA',paste0('resid = ',txt1),txt)
	  txt <- process_low_level_txt(txt,'w.func = NA',w.func)
	  .trkfunc=eval(parse(text=txt))

      phi.fit = nlm(.trkfunc, phi.start, hessian = FALSE, typsize=rep(1, length(phi)),
                  fscale=optim.list$tol)
      if ( phi.fit$code > 3 )
        warning(paste("IRWLS Iteration ", k, " failed to converge.\n", sep=""))

      phi = phi.fit$estimate
    }

    wts = if ( wt.type%in%c("weights_huber", "weights_tukey_bw") )
            w.func(phi, as.vector(y-mu))
          else
            w.func(phi, mu)^(-2)
    ## Scale the weights            
#    wts = 100*wts/( max(wts[is.finite(wts)]) - min(wts[is.finite(wts)]) )
#    wts[is.infinite(wts)] = 10000
    fit = optim(theta0, fn=.f.ssq, gr=.gr.ssq, method=optim.list$method,
          lower=optim.list$lower, upper=optim.list$upper, hessian=TRUE,
          x=x, y=y, w=wts, control=optim.list$control.list)

    if ( !phi.fixed && max( abs((phi-phi.lag)/phi.lag) ) < optim.list$tol )
    {
      converge.pls=TRUE
      break
    }
  }
  if ( !converge.pls )
    warning("IRWLS Method failed to converge.")

  names(phi) = names(phi.start)
  mu= f.model(fit$par, x)
  attr(fit, "weights") =  if ( wt.type%in%c("weights_huber", "weights_tukey_bw") )
                            w.func(phi, as.vector(y-mu))
                          else
                            w.func(phi, mu)^(-2)
  attr(fit, "var.param") = phi
  attr(fit, "converge.pls") = converge.pls

  return(fit)

}

