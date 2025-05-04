\name{test_fit}
\alias{test_fit}
\title{Test Fit Parameters}
\description{ Test if estimated parameters optimize the regression system (i.e., minimize sums of squares, maximize likelihood).  }
\usage{ 
       test_fit(obj, check.pd.tol = 1e-8)
}
\arguments{
   \item{obj}{An \code{optim_fit} object}
   \item{check.pd.tol}{absolute tolarence for determing whether a matrix is positive definite.}

}
\details{
  The function checks if \code{optim} convergence has been reached and also checks if the cholesky decompoisition of the Hessian matrix is positive definite.  The latter is an indication that optimization has been reached.
  Sometimes the \code{chol} decomposition check doesn't work and to enforce that constriant we use the \code{check.pd.tol} to make sure all the eigenvalues are larger than this minimum threshhold.
}
\seealso{
\code{\link{optim_fit}}
}
\value{
Returns a TRUE or FALSE as to whether or not hessian component of the object is Positive Definite.
}
\examples{
set.seed(123L)
x = rep( c(0, 2^(-4:4)), each=4 )
theta = c(0, 100, log(.5), 2)
y1 = hill_model(theta, x) + rnorm( length(x), sd=2 )
wts = runif( length(y1) )
fit1=optim_fit(theta, hill_model, x=x, y=y1)
test_fit(fit1)
} 
\author{Steven Novick}
\keyword{Nonlinear}
