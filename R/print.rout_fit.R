print.rout_fit = function(x, digits=4,...)
{
  cat("\n")
  cat("\nROUT Fitted Model Statistics", "\n")
  cat("\n")
  cat( "Convergence :", ifelse(x$Converge, "Achieved", "Failed"), "\n" )
  cat("\n")   
  cat( "Parameter estimates\n", sep="" )
  print.noquote( round(x$par, digits) )
  cat("\n")
  cat( "RSDR =", round(x$rsdr, digits), "from", length(x$residuals), "obsevations and", length(x$par)-1, "parameter(s)", "\n" )
  cat("\n")
  cat( "r-sq (without FDR correction) =", round(x$r.squared, digits), "\n" )
  cat( "r-sq (with FDR correction) =", round(x$r.squared.adj, digits), "\n" )
  cat("\n")
    
  cat("Q =", attr(x, "Q"), "\n")
  cat( "# of outliers detected (without FDR correction) =", sum(x$outlier), "\n" )
  cat( "# of outliers detected (with FDR correction) =", sum(x$outlier.adj), "\n" )

  if ( !is.null(x$message) )
    print.noquote( x$message )

  invisible()
}

