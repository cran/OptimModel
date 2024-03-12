predict.optim_fit = function (object, x, se.fit = FALSE,
                        interval = c("none", "confidence", "prediction"), K = 1, level = 0.95, ...)
{

  interval = match.arg(interval)
  
  if ( missing(x) && (se.fit | interval!="none") )
    stop("User must provide 'x' for se.fit=TRUE or interval='confidence' or 'prediction'.")
  if ( interval != "none" )
    se.fit = TRUE
    

  f.model = eval(object$call$f.model)
  if ( missing(x) && interval=="none" )
    return( fitted(object) )

  out = list()
  if ( !missing(x) )
    out$x = x
  out$y.hat = f.model( coef(object), x )
  if ( se.fit )
  {
    jacobian = attr(f.model, "gradient")
    X.mat = if ( is.null(jacobian) )
                f2djac(f.model, coef(object), x=x)
              else
                jacobian(coef(object), x)
    var.y = diag(X.mat%*%object$varBeta%*%t(X.mat))
    out$se.fit = sqrt(var.y)
  }
  if ( interval != "none" )
  {
    sig = object$sigma

      ## For the case:  Var( Y | x ) = sigma^2*g^2(mu, x)

    if ( inherits(attr(object, "w.func"),"function") && any( formalArgs(attr(object, "w.func"))=="mu" ) )
       sig = sig*attr(object, "w.func")( attr(object, "var.param"), out$y.hat )

    total.var = var.y
    if ( interval != "confidence" )
      total.var = total.var + sig^2/K
      
    se = sqrt(total.var)
    out$lower = out$y.hat - qt( (1+level)/2, object$df )*se
    out$upper = out$y.hat + qt( (1+level)/2, object$df )*se
  }

  return( do.call("cbind", out) )
}


    

