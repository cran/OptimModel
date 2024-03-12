\name{get_se_func}
\alias{get_se_func}
\title{Compute standard error for a function of model parameter estimates}
\description{ Compute standard error for a function of model parameter estimates via the delta method. }
\usage{get_se_func(object, Func, \dots, level=0.95)}
\arguments{
    \item{object}{ An optim_fit() object }
    \item{Func}{ Function that returns a numeric value. See details. }
    \item{\dots}{ Other arguments needed for Func.    }
    \item{level}{ Confidence level for confidence interval }
}
\details{
  Func is of the form function(theta, ...).  For example,
  
  Func = function(theta, x)\{ exp(theta[1])*log(x)/theta[2] \}
}
\value{
  Returns a data.frame with a single row for the estimated Func call (Est),
  its standard error (SE), and a confidence interval (lower, upper).
}
\seealso{
\code{\link{optim_fit}}, \code{\link{rout_fitter}}
}
\examples{
set.seed(123L)
x = rep( c(0, 2^(-4:4)), each =4 )
theta = c(0, 100, log(.5), 2)
y = hill_model(theta, x) + rnorm( length(x), sd=2 )
fit = optim_fit(theta, hill_model, x=x, y=y)
  
## Get SE for IC20 and IC40
ic.z = function(theta, z){  attr(hill_model, "backsolve")(theta, z)  }
get_se_func(object=fit, Func=ic.z, z=20)
get_se_func(object=fit, Func=ic.z, z=40)
  
}
\author{Steven Novick}
\keyword{Standard Error}
\keyword{delta method}