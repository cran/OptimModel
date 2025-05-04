\name{hill5_model}
\alias{hill5_model}
\title{Five-parameter Hill model, gradient, starting values, and back-calculation functions}
\description{ Five-parameter Hill model, gradient, starting values, and back-calculation functions.  }
\usage{ 
        hill5_model(theta, x)
}
\arguments{
    \item{theta}{ Vector of five parameters:  \eqn{(e_{\min}, e_{\max}, \text{log.ic50}, m, \text{log.sym})}.  See details. }
    \item{x}{ Vector of concentrations for the five-parameter Hill model. }
}
\details{
  The five parameter Hill model is given by:
  
  \deqn{y = e_{\min} + \frac{e_{\max}-e+{\min}}{ 1 + \exp( m\log(x) - m\text{ log.ic50}) )^{\exp(\text{log.sym})}}}
  
  \eqn{e_{\min} = \min y} (minimum y value), \eqn{e_{\max} = \max y} (maximum y value), \eqn{\text{log.ic50} = \log( \text{ic50} )}, m  is the shape parameter,  and \eqn{\text{log.sym} = \log( \text{symmetry parameter})}.

  Note:  ic50 is defined such that hill5_model(theta, ic50) \eqn{= e_{\min}+(e_{\max}-e_{\min})/2^{\exp(\text{log.sym})}}
}
\value{
Let N = length(x).  Then
\itemize{
\item hill5_model(theta, x) returns a numeric vector of length N.
\item attr(hill5_model, "gradient")(theta, x) returns an N x 5 matrix.
\item attr(hill5_model, "start")(x, y) returns a numeric vector of length 5 with starting values for
\eqn{(e_{\min}, e_{\max}, \text{log.ic50}, m, \text{log.sym})}.
\item attr(hill5_model, "backsolve")(theta, y) returns a numeric vector of length=length(y).
}
}
\seealso{
\code{\link{optim_fit}}, \code{\link{rout_fitter}}
}
\examples{
set.seed(123L)
x = rep( c(0, 2^(-4:4)), each=4 )
theta = c(0, 100, log(.5), 2, log(10))
y = hill5_model(theta, x)  + rnorm( length(x), mean=0, sd=1 )
attr(hill5_model, "gradient")(theta, x)
attr(hill5_model, "start")(x, y)
attr(hill5_model, "backsolve")(theta, 50)
}
\author{Steven Novick}
\keyword{Nonlinear}
