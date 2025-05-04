improved_fitter = function( theta0, f.model, x, y, wts,
                            Mult=0.1, n.start=1000, max.try=25, start.method=c("fixed", "random"), until.converge=FALSE, keep.all=FALSE, ... )
{
  cl = match.call()
  if ( is.null(theta0) )
    theta0 = attr(f.model, "start")(x, y)

  w = rep(1, length(y))
  if ( !missing(wts) && is.numeric(wts) )
  {
    if ( length(wts) != length(y) )
      stop("Number of weight values in 'weights' must equal number of observations in 'y'.")
    w = wts
  }

 M = pmax( Mult*abs(theta0), 0.1 )
 start.method = match.arg(start.method)
 
 if ( start.method=="fixed" )
 {
   theta00 = rbind(theta0,
               expand.grid(lapply(1:length(theta0), function(i){ seq( theta0[i]-3*M[i], theta0[i]+3*M[i], length=ceiling(n.start^(1/length(theta0)))) }))
               )
 }else{
   theta00 = rbind(theta0, 
              sapply(1:length(theta0), function(i){ rnorm(n.start-1, mean=theta0[i], sd=M[i]) })
              )
 }
 colnames(theta00) = names(theta0)

 sumSq = apply(theta00, 1, function(th){  sum( w*(y-f.model(th, x))^2 ) })
 theta00 = theta00[order(sumSq),]

 if ( max.try > nrow(theta00) )
   max.try = nrow(theta00)

 fit = list()
 i.best = 1
 bic.lag = Inf
 for ( i in 1:max.try )
 {
	 txt = paste("try( optim_fit( theta00[i,], ", match.call()$f.model, ", x=x, y=y, wts=", match.call()$wts, ", ... ), silent=TRUE )")
   fit[[i]] = eval(parse(text = txt))
   fit[[i]]$Converge = FALSE
   if ( class(fit[[i]])[1] != "try-error" )
   {
     fit[[i]]$Converge = fit[[i]]$converge==0 & (class( try( chol(fit[[i]]$hessian), silent=TRUE ) )[1] == "matrix")
     fit[[i]]$call$theta0 = theta00[i,]
     for ( v in names(list(...)) )
       fit[[i]]$call[[v]] = list(...)[[v]]
   }
   else
     fit[[i]]$bic = Inf
    ## best fit
   if ( fit[[i]]$bic < bic.lag )
   {
     i.best = i
     bic.lag = fit[[i]]$bic
   }
   if ( until.converge & fit[[i]]$Converge )
   {
     fit = fit[1:i]
     i.best = i
     break
   }
 }
 if ( !keep.all )
   fit = fit[[i.best]]

  return(fit)

}              
