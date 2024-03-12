\name{linear_model}
\alias{linear_model}
\title{Linear model, gradient, and starting values}
\description{ Linear model, gradient, and starting values.  }
\usage{ 
        linear_model(theta, x)
}
\arguments{
    \item{theta}{ Vector of model parameters intercept and slope.  See details.}
    \item{x}{ Matrix, possibly from \code{model.matrix}(). }
}
\details{
  The linear model is given by:
  
  \deqn{y = x * \theta \text{, where}}
  x is a \code{matrix}, possibly generated from \code{model.matrix}()
  \eqn{\theta} is a vector of linear parameters

}
\value{
Let N = nrow(x).  Then
\itemize{
\item linear_model(theta, x) returns a numeric vector of length N.
\item attr(linear_model, "gradient")(theta, x) returns x.
\item attr(linear_model, "start")(x, y) returns solve(t(x) * x) * t(x) * y
}
}
\seealso{
\code{\link{optim_fit}}, \code{\link{rout_fitter}}
}
\examples{
set.seed(123)
d = data.frame( Group=factor(rep(LETTERS[1:3], each=5)), age=rnorm(15, mean=20, sd=3) )
d$y = c(80, 100, 120)[unclass(d$Group)] - 3*d$age + rnorm(nrow(d), mean=0, sd=5)

X = model.matrix(~Group+age, data=d)  ## This is the "x" in linear.model()
theta = c(80, 20, 40, -3)   ## Intercept, effect for B, effect for C, slope for age
linear_model(theta, x=X)
attr(linear_model, "gradient")(theta, x=X)
attr(linear_model, "start")(x=X, y=d$y)

}
\author{Steven Novick}
\keyword{Nonlinear}
