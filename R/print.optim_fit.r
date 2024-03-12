print.optim_fit = function(x, digits=4, ...)
{
  cat("\n")    
  cat( "Fit method = '", x$fit.method, "'\n", sep="" )
  cat( "Variance computation method = '", x$var.method, "'\n", sep="" )
  cat( "Weights =", attr(attr(x, "w.func"), "label"), "\n" )
  cat( "Convergence :", ifelse(x$Converge, "Achieved", "Failed"), "\n" )  
  cat("\n")   
  cat( 100*attr(x$beta, "conf.level"), "% Wald CI for parameters\n", sep="" )
  print.noquote( round(x$beta, digits) )
  cat( "sigma =", round(x$sigma, digits), "on", x$df, "degrees of freedom", "\n" )
  cat( "r-squared =", round(x$r.squared, digits), "\n" )
  cat( "BIC = ", round(x$bic, digits), "  (smaller is better)\n" )
  if ( x$fit.method != "ols" )
  {
    if ( !is.null( attr(x, "var.param") ) )
    {
      cat("var/weight param(s):\n")
      phi = round(attr(x, "var.param"), digits)
      print.noquote( ifelse(is.na(phi), "none", phi) )
    }
  }

  if ( !is.null(x$message) )
    print.noquote( x$message )

  cat('\n')
  invisible()
}

