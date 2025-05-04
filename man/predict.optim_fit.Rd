\name{predict.optim_fit}
\alias{predict.optim_fit}
\title{Predicted values for optim.fit objects}
\description{ Provides predicted values, standard errors, confidence intervals and prediction intervals for \code{optim_fit} objects.  }
\usage{ 
    \method{predict}{optim_fit}(object, x, se.fit=FALSE, 
    interval=c("none", "confidence", "prediction"), K = 1, level = 0.95,\dots)
}
\arguments{
    \item{object}{ An object resulting from \code{optim_fit}. }
    \item{x}{ If supplied, a vector, data.frame, or matrix of Explanatory variables. }
    \item{se.fit}{ Logical.  Should standard errors be returned?  Requires that 'x' is supplied. }
    \item{interval}{ If equal to 'confidence', returns a 100*level\% confidence interval for the
    mean response.  If equal to 'prediction', returns a 100*level\% prediction interval for the mean of the next
    K observations.  Requires that 'x' is supplied. }    
    \item{K}{ Only used for prediction interval.  Number of observations in the mean for the prediction interval. }
    \item{level}{ Confidence/prediction interval level.}
    \item{\dots}{mop up additional arguments.}
}
\value{
Returns a vector (if interval='none').
Otherwise returns a data.frame with possible columns 'x', 'y.hat', 'se.fit', 'lower', and 'upper'.
}
\seealso{
\code{\link{optim_fit}} 
}
\examples{
set.seed(123L)

x = rep( c(0, 2^(-4:4)), each=4 )
theta = c(0, 100, log(.5), 2)
y1 = hill_model(theta, x) + rnorm( length(x), sd=2 )


fit1=optim_fit(theta, hill_model, x=x, y=y1)
fitted(fit1)
predict(fit1)
predict(fit1, x=x)
predict(fit1, x=seq(0, 1, by=.1), se.fit=TRUE)
predict(fit1, x=seq(0, 1, by=.1), interval="conf")
predict(fit1, x=seq(0, 1, by=.1), interval="pred")
}
\author{Steven Novick}
\keyword{Nonlinear}
