\name{exp_decay_pl}
\alias{exp_decay_pl}
\title{Three-parameter exponential decay with initial plateau, gradient, starting values, and back-calculation functions}
\description{ Three-parameter exponential decay with initial plateau, gradient, starting values, and back-calculation functions.  }
\usage{ 
        exp_decay_pl(theta, x)
}
\arguments{
    \item{theta}{ Vector of four parameters:  (x0, yMax, yMin, k).  See details.}
    \item{x}{ Vector of concentrations. }
}
\details{
  The three-parameter exponential decay with initial plateau model is given by \eqn{y = \text{yMax}} whenever \eqn{x\le 0} otherwise 
  \deqn{y = \text{yMin} + (\text{yMax}-\text{yMin})\times\exp(-K(x-X0))\text{ if }  x > X0,}

  where \eqn{X0 = \exp(x0)} is an inflection point between plateau and exponential decay curve,
  \eqn{\text{yMin} = \min y} (min response), \eqn{\text{yMax} = \max y} (maximum response), and \eqn{K = \exp(k)} is the shape parameter.
}
\value{
Let N = length(x).  Then
\itemize{
\item exp_decay_pl(theta, x) returns a numeric vector of length N.
\item attr(exp_decay_pl, "gradient")(theta, x) returns an N x 4 matrix.
\item attr(exp_decay_pl, "start")(x, y) returns a numeric vector of length 4 with starting values for
(x0, yMax, yMin, k).
\item attr(exp_decay_pl, "backsolve")(theta, y) returns a numeric vector of length=length(y).
}
}
\seealso{
\code{\link{optim_fit}}, \code{\link{rout_fitter}}
}
\examples{
set.seed(100)
x = 2^(-4:4)
theta = c(0.4, 75, 10, log(3))
y = exp_decay_pl(theta, x)  + rnorm( length(x), mean=0, sd=1 )
attr(exp_decay_pl, "gradient")(theta, x)
attr(exp_decay_pl, "start")(x, y)
attr(exp_decay_pl, "backsolve")(theta, 38)
}
\author{Steven Novick}
\keyword{Nonlinear}
