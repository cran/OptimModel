\name{hill_model}
\alias{hill_model}
\title{Four-parameter Hill model, gradient, starting values, and back-calculation functions}
\description{ Four-parameter Hill model, gradient, starting values, and back-calculation functions.  }
\usage{ 
        hill_model(theta, x)
}
\arguments{
    \item{theta}{ Vector of four parameters:  \eqn{(e_{\min}, e_{\max}, \text{lec50}, m)}.  See details.}
    \item{x}{ Vector of concentrations for the Hill model. }
}
\details{
  The four parameter Hill model is given by:
  \deqn{y = e_{\min} + \frac{(e_{\max}-e_{\min})}{( 1 + \exp( m\log(x) - m*\text{lec50} ) )}\text{, where }}
  
  \eqn{e_{\min} = \min y} (minimum y value), \eqn{e_{\max} = \max y} (maximum y value), \eqn{\text{lec50} = \log( \text{ec5} )}, and m is the shape parameter.
    Note:  ec50 is defined such that hill.model(theta, ec50) = .5*( emin+ emax ).  
  
}
\value{
Let N = length(x).  Then
\itemize{
\item hill_model(theta, x) returns a numeric vector of length N.
\item attr(hill_model, "gradient")(theta, x) returns an N x 4 matrix.
\item attr(hill_model, "start")(x, y) returns a numeric vector of length 4 with starting values for
\eqn{(e_{\min}, e_{\max}, \text{lec50}, m)}.
\item attr(hill_model, "backsolve")(theta, y) returns a numeric vector of length=length(y).
}
}
\seealso{
\code{\link{optim_fit}}, \code{\link{rout_fitter}}
}
\examples{
set.seed(123L)
x = rep( c(0, 2^(-4:4)), each=4 )
theta = c(0, 100, log(.5), 2)
y = hill_model(theta, x)  + rnorm( length(x), mean=0, sd=1 )
attr(hill_model, "gradient")(theta, x)
attr(hill_model, "start")(x, y)
attr(hill_model, "backsolve")(theta, 50)
}
\author{Steven Novick}
\keyword{Nonlinear}
