\name{exp_2o_decay}
\alias{exp_2o_decay}
\title{Five-parameter second-order exponential decay, gradient, starting values, and back-calculation functions}
\description{ Five-parameter second-order exponential decay, gradient, starting values, and back-calculation functions.  }
\usage{ 
        exp_2o_decay(theta, x)
}
\arguments{
    \item{theta}{ Vector of five parameters: (A, B, k1, k2, p).  See details.}
    \item{x}{ Vector of concentrations. }
}
\details{
  The five-parameter exponential decay model is given by:

\deqn{y = A + B\times P\times \exp(-K1 \times x) + B\times (1 - P)\times \exp(-K2\times x)}  

The parameter vector is (A, B, k1, k2, p) where \eqn{A = \min y} (min y value), \eqn{A+B = \max y} (max y value), 
\eqn{K1 = \exp(k1)} which is the shape parameter for first term, \eqn{K2 = \exp(k2)} which is the shape parameter for second term, and \eqn{P = 1/(1+\exp(p))} which is the  proportion of signal from the first term. 
}
\value{
Let N = length(x).  Then
\itemize{
\item exp_2o_decay(theta, x) returns a numeric vector of length N.
\item attr(exp_2o_decay, "gradient")(theta, x) returns an N x 5 matrix.
\item attr(exp_2o_decay, "start")(x, y) returns a numeric vector of length 5 with starting values for (A, B, k1, k2, p). 
\item attr(exp_2o_decay, "backsolve")(theta, y) returns a numeric vector of length = length(y).
}
}
\seealso{
\code{\link{optim_fit}}, \code{\link{rout_fitter}}
}
\examples{
set.seed(123L)
x = 2^(-4:4)
theta = c(25, 75, log(3), log(1.2), 1/(1+exp(.7)))
y = exp_2o_decay(theta, x) + rnorm( length(x), mean=0, sd=1 )
attr(exp_2o_decay, "gradient")(theta, x)
attr(exp_2o_decay, "start")(x, y)
attr(exp_2o_decay, "backsolve")(theta, 38)
}
\author{Steven Novick}
\keyword{Nonlinear}
