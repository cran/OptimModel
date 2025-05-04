robust_fit = function(theta0, f.model, gr.model=NULL, x, y, wts=c("huber", "tukey"),
            var.method=c("hessian", "normal", "robust"),
            conf.level=.95, tol=1e-3, ...)
{
  wts = match.arg(wts)                    ## huber, tukey

  ## Get weights
  if ( wts=="huber" )
    w.func = weights_huber
  else if ( wts=="tukey" )
    w.func = weights_tukey_bw
  else
    stop("Weights ('wts') must be given as either 'huber' or 'tukey'. For more advanced options, use optim_fit().")


  txt = paste("optim_fit(theta0=theta0, f.model=", match.call()$f.model,
              ", gr.model=", match.call()$gr.model, ", x=x, y=y",
              ", wts=", ifelse(wts=="huber", "weights_huber", "weights_tukey_bw"),
              ", fit.method='irwls', var.method=var.method, phi0=NULL, phi.fixed=TRUE",
              ", conf.level=conf.level, tol=tol, ...)")

  fit = eval(parse(text=txt))


  return(fit)
}


