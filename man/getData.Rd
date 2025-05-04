\name{getData}
\alias{getData}
\alias{getData.optim.fit}
\title{Extract data object from an optim fit}
\description{ Extract data object from an \code{optim_fit} object. }
\usage{
  getData(object)
}
\arguments{
    \item{object}{ object of class \code{optim_fit}. }
}
\value{
  Returns a data frame with elements x and y.
}
\seealso{
\code{\link{optim_fit}}, \code{\link{rout_fitter}}
}
\examples{
set.seed(123L)
x = rep( c(0, 2^(-4:4)), each =4 )
theta = c(0, 100, log(.5), 2)
y = hill_model(theta, x) + rnorm( length(x), sd=2 )
fit = optim_fit(c(0, 100, .5, 1), f.model=hill_model, x=x, y=y)
d=getData(fit)
}
\author{Steven Novick}
