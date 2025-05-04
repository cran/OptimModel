test_fit = function(obj,check.pd.tol = 1e-8){
  ## 1. Test if optim() converged.  optim()$converge==0
  ## 2. Test if hessian matrix is positive definitive.
  ##    Positive definite <=> hessian can be decomposed by cholesky decomposition
  ##                      <=> parameters maximize the likelihood
  pass = FALSE
  if ( class(obj)[1] != "try-error" )
  {
	  if ( obj$converge==0 & class( try(chol(obj$hessian), silent=TRUE) )[1] == "matrix" )
	  {
		  e1 <- try(eigen(obj$hessian)$value,silent=TRUE)
		  
		  if(class(e1)[1] != 'try-error')
		  {
			if(all(is.numeric(e1)) & all(is.finite(e1))){
			  if(all(abs(e1) > check.pd.tol)){
				  pass = TRUE
			  }
			}
		  }
	  }
  }
  return(pass)
}
