\name{print.optim_fit}
\alias{print.optim_fit}
\title{Prints optim_fit objects}
\description{ Provides a nice printed summary of \code{optim_fit} objects.  }
\usage{ 
  \method{print}{optim_fit}(x, digits=4,\dots)
}
\arguments{
    \item{x}{ An object resulting from optim_fit(). }
    \item{digits}{ Number of digits to print for output. }
    \item{\dots}{other arguments not used by this function.}
}
\seealso{
\code{\link{optim_fit}} 
}
\value{
No Return Value.
}
\examples{
set.seed(123L)
x = rep( c(0, 2^(-4:4)), each=4 )
theta = c(0, 100, log(.5), 2)
y1 = hill_model(theta, x) + rnorm( length(x), sd=2 )

fit1=optim_fit(theta, hill_model, x=x, y=y1)
print(fit1)
fit1

}
\author{Steven Novick}
\keyword{Nonlinear}