\name{rout_outlier_test}
\alias{rout_outlier_test}
\title{ROUT Outlier Testing}
\description{ Perform ROUT outlier testing on \code{rout.fitter} object.  }
\usage{
rout_outlier_test(fit, Q = 0.01)
}
\arguments{
    \item{fit}{ A \sQuote{rout.fitter} object from the \code{rout_fitter} function. }
    \item{Q}{ Test size for ROUT outlier detection.  }
}
\details{
  \code{rout_outlier_test}() is typically called from \code{rout_fitter}(), but may also be called directly by the user.
}
\value{

outlier = logical vector.  TRUE indicates observation is an outlier via hypothesis testing with unadjust p-values.

outlier.adj = logical vector.  TRUE indicates observation is an outlier via hypothesis testing with FDR-adjust p-values.

attr(object, "Q") = test size for outlier detection

}
\seealso{
\code{\link{rout_fitter}} 
}
\examples{
set.seed(123L)

x = rep( c(0, 2^(-4:4)), each=4 )
theta = c(0, 100, log(.5), 2)
y = hill_model(theta, x) + rnorm( length(x), sd=2 )

ii = sample( 1:length(y), 2 )
y[ii] = hill_model(theta, x[ii]) + 5.5*2 + rnorm( length(ii), sd=2 )

fit = rout_fitter(c( theta, log(2) ), hill_model, x=x, y=y, Q=0.01)
rout_outlier_test(fit, Q=0.001)

}
\author{Steven Novick}
\keyword{Nonlinear}