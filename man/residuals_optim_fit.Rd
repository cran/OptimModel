\name{residuals.optim_fit}
\alias{residuals.optim_fit}
\title{Residuals for optim.fit objects}
\description{ Provides raw and studentized residuals for \code{optim_fit} objects.  }
\usage{ 
    \method{residuals}{optim_fit}(object, type=c("raw", "studentized"), \dots)
}
\arguments{
    \item{object}{ An object resulting from \code{optim.fit}(). }
    \item{type}{ 'raw' or 'studentized' residuals. }
    \item{\dots}{mop up additional arguments.}
}
\value{
Returns a numeric vector.
}
\seealso{
\code{\link{optim_fit}} 
}
\examples{
set.seed(123)

x = rep( c(0, 2^(-4:4)), each=4 )
theta = c(0, 100, log(.5), 2)
y1 = hill_model(theta, x) + rnorm( length(x), sd=2 )

fit1=optim_fit(theta, hill_model, x=x, y=y1)
residuals(fit1)
residuals(fit1, type="s")
}
\author{Steven Novick}
\keyword{Nonlinear}