\name{exp_decay}
\alias{exp_decay}
\title{Three-parameter exponential decay, gradient, starting values, and back-calculation functions}
\description{ Three-parameter exponential decay, gradient, starting values, and back-calculation functions.  }
\usage{ 
        exp_decay(theta, x)
}
\arguments{
    \item{theta}{ Vector of three parameters:  (A, B, k).  See details.}
    \item{x}{ Vector of concentrations. }
 }
\details{
  The three-parameter exponential decay model is given by:
  
  \deqn{y = A + B \times \exp(-Kx).}
  
  The parameter vector is (A, B, k) where
    \eqn{A =\min y} ( minimum y value), \eqn{A + B = \max y} (maximum y value), and \eqn{K = \exp(k)} whichi is the shape parameter.
  
}
\value{
Let N = length(x).  Then
\itemize{
\item exp_decay(theta, x) returns a numeric vector of length N.
\item attr(exp_decay, "gradient")(theta, x) returns an N x 3 matrix.
\item attr(exp_decay, "start")(x, y) returns a numeric vector of length 3 with starting values for
(A, B, k).
\item attr(exp_decay, "backsolve")(theta, y) returns a numeric vector of length=length(y).
}
}
\seealso{
\code{\link{optim_fit}}, \code{\link{rout_fitter}}
}
\examples{
set.seed(123L)
x = 2^(-4:4)
theta = c(25, 75, log(3))
y = exp_decay(theta, x)  + rnorm( length(x), mean=0, sd=1 )
attr(exp_decay, "gradient")(theta, x)
attr(exp_decay, "start")(x, y)
attr(exp_decay, "backsolve")(theta, 38)
}
\author{Steven Novick}
\keyword{Nonlinear}
