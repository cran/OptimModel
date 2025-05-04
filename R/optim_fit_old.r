
##optimFit <- function(theta0, ...) UseMethod("optimFit")
##optimFit.numeric = function(theta0, f.model, gr.model=NULL, x, y, wts,
##      fit.method=c("ols", "irwls", "mle"),
##      var.method=c("hessian", "normal", "robust"),
##      phi0=NULL, phi.fixed=TRUE, conf.level=.95, tol=1e-3,...){
##	optimFit.default(theta0,f.model, gr.model, x, y, wts,
##		  fit.method,var.method,phi0, phi.fixed,
##		  conf.level, tol,...)
##}
optim_fit_old = function(theta0, f.model, gr.model=NULL, x, y, wts,
            fit.method=c("ols", "irwls", "mle"),
            var.method=c("hessian", "normal", "robust"),
            phi0=NULL, phi.fixed=TRUE, conf.level = 0.95, tol = 1e-3, ...)
{
  cl = match.call()
  fit.method = match.arg(fit.method)      ## ols, irwls, mle
  var.method = match.arg(var.method)      ## hessian, normal, robust
  ## Get weights
  if ( missing(wts) )
  {
    w.func = weights_varIdent
    attr(w.func, "label") = "weights_varIdent"
    wts = rep(1, length(y))

  }
  else if ( is.numeric(wts) )
  {
    w.func = NA
    attr(w.func, "label") = "user.defined"
    if ( length(wts) != length(y) )
      stop("Number of weight values in 'weights' must equal number of observations in 'y'.")  
  }
  else if ( inherits(wts,'function'))
  {
    w.func = wts
    attr(w.func, "label") = deparse(cl$wts)
    wts = rep(1, length(y))
  }    
  else
    stop("Weights ('wts') must be given as either numeric or a function (or left as missing).")

  if ( nrow( as.matrix(x) ) != length(y) )
    stop("Number of observations in 'x' must equal number of observations in 'y'.")

  ## Check data integrity
  if ( any(is.na(x)) || any(is.na(y)) || any(is.na(wts)) )
    warning("Missing values in your data may cause optim.fit() to fail.")
  
  ## Make smart decisions/warnings:
  if ( fit.method=="mle" && var.method!="hessian"  )
  {
    var.method="hessian"
    warning("MLE algorithm requires var.method='hessian'.  Switching var.method to 'hessian'.")
  }
  if ( fit.method=="ols" && !( attr(w.func, "label") %in% c("weights_varIdent", "user.defined") ) )
  {
    warning("Switching fit.method to irwls to accomodate choice of 'wts'.")
    fit.method = "irwls"
  }
  if ( fit.method=="irwls" && attr(w.func, "label") %in% c("weights_varIdent", "user.defined") )
  {
    warning("Switching fit.method to ols to accomodate choice of 'wts'.")
    fit.method = "ols"
  }
  if ( attr(w.func, "label") %in% c("weights_varIdent", "user.defined") && !is.null(phi0) )
  {
    warning("phi0 parameter is ignored when weights are numeric or 'weight_varIdent'.")
    phi.fixed=TRUE
  }
  if ( attr(w.func, "label") == "weights_tukey_bw" && fit.method!="irwls" )
  {
    warning("Switching fit.method to irwls to accomodate Tukey bi-weight algorithm.")
    fit.method = "irwls"
  }    
  optim.list = .get.optim.args( list(...), tol )

  if ( is.null(gr.model) )  ## Maybe the gradient is an attribute of f.model
    gr.model = attr(f.model, "gradient")
  if ( is.null(theta0) )  ## Make a starting-values function is an attribute of f.model
    theta0 = attr(f.model, "start")(x, y)
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
  ## Fit OLS model.  If method != "ols", these are the starting values.
  fit = optim(theta0, fn=.f.ssq, gr=.gr.ssq, method=optim.list$method, 
          lower=optim.list$lower, upper=optim.list$upper, hessian=TRUE,
          x=x, y=y, w=wts, control=optim.list$control.list)
  attr(fit, "weights") = wts
  
  if ( fit$converge > 0 )
    warning("OLS:  optim failed to converge")

  if ( fit.method=="irwls" )
    fit = .optim.fit.irwls(ols.fit=fit, f.model=f.model, gr.model=gr.model, phi0=phi0, phi.fixed=phi.fixed, x=x, y=y, w.func=w.func, optim.list=optim.list)
  if ( fit.method=="mle" )
    fit = .optim.fit.mle(ols.fit=fit, f.model=f.model, phi0=phi0, phi.fixed=phi.fixed, x=x, y=y, w.func=w.func, optim.list=optim.list)

  fit$fit.method = fit.method    
  fit$var.method = var.method
  attr(fit, "w.func") = w.func
  fit$call = cl
  fit = .get.fit.stats(fit, x, y, conf.level)
  
  fit$call = cl
  class(fit) = c("optim_fit", "list")

  return(fit)
}


