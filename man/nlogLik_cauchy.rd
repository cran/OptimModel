\name{nlogLik_cauchy}
\alias{nlogLik_cauchy}
\title{Negative log-likelihood function for Cauchy regression}
\description{ The negative log-likelihood function for Cauchy regression, for use with \code{rout_fitter}. Usually not called by the end user.  }
\usage{ 
       nlogLik_cauchy(theta, x, y, f.model, lbs)
}
\arguments{
    \item{theta}{Parameters for f.model and an extra parameter for the scale parameter; e.g., f.model=hill.model}
    \item{x}{ Explanatory variable(s).  Can be vector, matrix, or data.frame }
    \item{y}{ Response variable. }
   \item{f.model}{Name of mean model function. }
   \item{lbs}{Logical. lbs = log both sides. See details.}
}
\details{
  The function provides the negative log-likelihood for Cauchy regression
  
  Let mu = f.model(theta[1:(p-1)], x) and sigma = exp(theta[p]), where p = number of parameters in theta.
  
  The Cauchy likelihood is
  \deqn{ L = \prod \frac{1}{\pi \sigma} ( 1 + ( \frac{y_i - \mu_i}{\sigma} )^2 )^{-1} }.
  
  The function returns \eqn{\log(L)}.  
  
  If \code{lbs == TRUE}, then \eqn{\mu} is replaced with \eqn{\log(mu)}.
  
}
\value{
 Returns a single numerical value.
}
\seealso{
\code{\link{rout_fitter}}
}

\examples{
set.seed(123L)
x = rep( c(0, 2^(-4:4)), each=4 )
theta = c(emin=0, emax=100, lec50=log(.5), m=2)
y = hill_model(theta, x)  + rnorm( length(x), mean=0, sd=2 )

theta1 = c(theta, lsigma=log(2))
nlogLik_cauchy(theta1, x=x, y=y, f.model=hill_model, lbs=FALSE)

  ## Cauchy regression via maximum likelihood
optim( theta1, nlogLik_cauchy, x=x, y=y, f.model=hill_model, lbs=FALSE )

}
\author{Steven Novick}
\keyword{Nonlinear}
