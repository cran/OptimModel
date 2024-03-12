\name{hill_quad_model}
\alias{hill_quad_model}
\title{Five-parameter Hill model with quadratic component, gradient, starting values, and back-calculation functions}
\description{ Five-parameter Hill model with quadratic component, gradient, starting values, and back-calculation functions.  }
\usage{ 
        hill_quad_model(theta, x)
}
\arguments{
    \item{theta}{ Vector of five parameters:  (A, B, a, b, c).  See details. }
    \item{x}{ Vector of concentrations for the five-parameter Hill model with quadratic component. }
}
\details{
  The five parameter Hill model with quadratic component is given by:
  
  \deqn{y = A + \frac{B-A}{( 1 + \exp( -(a + bz + cz^2) )  )}\text{, where }z = \log(x)}
  
  \eqn{A =\min y} ( minimum y value), \eqn{B = \max y} (maximum y value), (a, b, c) are quadratic parameters for \eqn{\log(x)}.

  Notes:

  1.  If \eqn{c = 0}, this model is equivalent to the four-parameter Hill model (hill.model).

  2.  The ic50 is defined such that \eqn{a + bz + cz^2 = 0}.  If the roots of the quadratic equation are real, then the ic50
  is given by \eqn{\tfrac{-b \pm\sqrt{b^2 - 4ac }}{2a}}.

}
\value{
Let N = length(x).  Then
\itemize{
\item hill_quad_model(theta, x) returns a numeric vector of length N.
\item attr(hill_quad_model, "gradient")(theta, x) returns an N x 5 matrix.
\item attr(hill_quad_model, "start")(x, y) returns a numeric vector of length 5 with starting values for
(A, B, a, b, c).

If the quadratic roots are real-valued, attr(hill_quad_model, "backsolve")(theta, y) returns a numeric vector of length=2.
}
}
\seealso{
\code{\link{optim_fit}}, \code{\link{rout_fitter}}
}
\examples{
set.seed(123L)
x = rep( c(0, 2^(-4:4)), each=3 )      ## Dose
theta = c(0, 100, 2, 1, -0.5)          ## Model parameters
y = hill_quad_model(theta, x) + rnorm( length(x), mean=0, sd=5 )

## Generate data
hill_quad_model(theta, x)
attr(hill_quad_model, "gradient")(theta, x)
attr(hill_quad_model, "start")(x, y)
attr(hill_quad_model, "backsolve")(theta, 50)
}
\author{Steven Novick}
\keyword{Nonlinear}
