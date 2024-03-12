get_se_func = function (object, Func, ..., level=0.95)
{

  g = f2djac(Func, theta=coef(object), ...)
  out = data.frame(est = Func(coef(object), ...))
  
  out$SE = as.vector( sqrt(g%*%object$varBeta%*%t(g)) )
  out$lower = out$est - qt( (1+level)/2, object$df )*out$SE
  out$upper = out$est + qt( (1+level)/2, object$df )*out$SE

  return( out )
}


    

