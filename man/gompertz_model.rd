\name{gompertz_model}
\alias{gompertz_model}
\title{Four-parameter Gompertz model, gradient, starting values, and back-calculation functions}
\description{ Four-parameter Gompertz model, gradient, starting values, and back-calculation functions.  }
\usage{ 
        gompertz_model(theta, x)
}
\arguments{
    \item{theta}{ Vector of four parameters:  (A, B, m, offset).  See details.}
    \item{x}{ Vector of concentrations for the Gompertz model. }
}
\details{
  The four parameter Gompertz model is given by:
                                    
 \deqn{y = A + (B-A)\times\exp( -\exp( m(x-\text{offset}) ) )\text{, where}}
  
  \eqn{A = \min y} (minimum y value), \eqn{A+(B-A)\exp(-\exp( -m*\text{offset} ))} is the maximum y value, m is the shape parameter, and offset shifts the curve, relative to the concentration x.
  
}
\value{
Let N = length(x).  Then
\itemize{
\item gompertz_model(theta, x) returns a numeric vector of length N.
\item gompertz_model(hill_model, "gradient")(theta, x) returns an N x 4 matrix.
\item attr(gompertz_model, "start")(x, y) returns a numeric vector of length 4 with starting values for
(A, B, m, offset).
\item attr(gompertz_model, "backsolve")(theta, y) returns a numeric vector of length=length(y).
}
}
\seealso{
\code{\link{optim_fit}}, \code{\link{rout_fitter}}
}

\examples{
set.seed(100)
x = rep( c(0, 2^(-4:4)), each=4 )
theta = c(0, 100, log(.5), 2)
y = gompertz_model(theta, x)  + rnorm( length(x), mean=0, sd=1 )
attr(gompertz_model, "gradient")(theta, x)
attr(gompertz_model, "start")(x, y)
attr(gompertz_model, "backsolve")(theta, 50)
}
\author{Steven Novick}
\keyword{Nonlinear}
