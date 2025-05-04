
##optimFit <- function(theta0, ...) UseMethod("optimFit")
##optimFit.numeric = function(theta0, f.model, gr.model=NULL, x, y, wts,
##      fit.method=c("ols", "irwls", "mle"),
##      var.method=c("hessian", "normal", "robust"),
##      phi0=NULL, phi.fixed=TRUE, conf.level=.95, tol=1e-3,...){
##	optimFit.default(theta0,f.model, gr.model, x, y, wts,
##		  fit.method,var.method,phi0, phi.fixed,
##		  conf.level, tol,...)
##}
optim_fit = function(theta0, f.model, gr.model=NULL, x, y, wts,
            fit.method=c("ols", "irwls", "mle"),
            var.method=c("hessian", "normal", "robust"),
            phi0=NULL, phi.fixed=TRUE, conf.level = 0.95, tol = 1e-3, 
            n.start=1000, ntry=1, start.method=c("fixed", "random"), until.converge=TRUE,
            check.pd.tol = 1e-8,...)
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
  
 if ( ntry > 1 & n.start > 1 ){ 
   M = pmax( 0.1*abs(theta0), 0.1 )
   start.method = match.arg(start.method)
 
   if ( start.method=="fixed" )
   {
     theta00 = rbind(theta0,
               expand.grid(lapply(1:length(theta0), function(i){ 
                           seq( theta0[i]-3*M[i], theta0[i]+3*M[i], length=ceiling(n.start^(1/length(theta0)))) }))
               )
   }else{
     theta00 = rbind(theta0, 
                sapply(1:length(theta0), function(i){ rnorm(n.start-1, mean=theta0[i], sd=M[i]) })
                )
   }
   colnames(theta00) = names(theta0)

   sumSq = apply(theta00, 1, function(th){  sum( wts*(y-f.model(th, x))^2 ) })
   theta00 = theta00[order(sumSq),]

   if ( ntry > nrow(theta00) )
     ntry = nrow(theta00)  
 }else{
   theta00 = t(theta0)
   ntry = 1
 }
 
 fit = list()
 i.best = 1
 bic.lag = Inf
 for ( i in 1:ntry )
 {  
    ## Fit OLS model.  If method != "ols", these are the starting values.
    fit[[i]] = optim(theta00[i,], fn=.f.ssq, gr=.gr.ssq, method=optim.list$method, 
            lower=optim.list$lower, upper=optim.list$upper, hessian=TRUE,
            x=x, y=y, w=wts, control=optim.list$control.list)

    attr(fit[[i]], "weights") = wts
  
    if ( fit[[i]]$converge > 0 )
      warning("OLS:  optim failed to converge")

    if ( fit.method=="irwls" )
      fit[[i]] = .optim.fit.irwls(ols.fit=fit[[i]], f.model=f.model, gr.model=gr.model, phi0=phi0, phi.fixed=phi.fixed, 
                                  x=x, y=y, w.func=w.func, optim.list=optim.list)
    if ( fit.method=="mle" )
      fit[[i]] = .optim.fit.mle(ols.fit=fit[[i]], f.model=f.model, phi0=phi0, phi.fixed=phi.fixed, x=x, y=y, w.func=w.func, optim.list=optim.list)            
    fit[[i]]$fit.method = fit.method    
    fit[[i]]$var.method = var.method
    attr(fit[[i]], "w.func") = w.func
    fit[[i]]$call = cl
    fit[[i]] = .get.fit.stats(fit[[i]], x, y, conf.level)      
    fit[[i]]$call = cl
      
    fit[[i]]$Converge = FALSE
    if ( class(fit[[i]])[1] != "try-error" ){
     fit[[i]]$Converge = test_fit(fit[[i]],check.pd.tol)
    }else{
     fit[[i]]$bic = Inf
    }
    
    ## best fit
   if ( fit[[i]]$bic < bic.lag )
   {
     i.best = i
     bic.lag = fit[[i]]$bic
   }
   if ( until.converge & fit[[i]]$Converge )
   {
     fit = fit[[i]]
     break
   }
 }
 ## If none of the fits show Converge=TRUE, then return the best one (smallest value)
 if ( i==ntry & bic.lag < Inf & !until.converge ){

 ## Should this be the best bic or the best bic among fits with Converge==TRUE
     fit = fit[[i.best]]
 }

  ## Hold onto "x" (can be used for predict_optim_fit, needed for studentized residuals)
  fit$x = x
  
  class(fit) = c("optim_fit", "list")

  return(fit)
}


