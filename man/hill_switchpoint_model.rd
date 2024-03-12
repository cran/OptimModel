\name{hill_switchpoint_model}
\alias{hill_switchpoint_model}
\title{Five-parameter Hill model with switch point component, gradient, starting values, and back-calculation functions}
\description{ Five-parameter Hill model with switch point component, gradient, starting values, and back-calculation functions.  }
\usage{ 
        hill_switchpoint_model(theta, x)
}
\arguments{
    \item{theta}{ Vector of five parameters:  \eqn{(e_{\min}, e_{\max}, \text{lec50}, m, \text{lsp})}.  See details. }
    \item{x}{ Vector of concentrations for the five-parameter Hill model with switch point component. }
}
\details{
  The five parameter Hill model with switch point component is given by:
  
  \deqn{y = e_{\min} + \frac{e_{\max}-e_{\min}}{1 + \exp( m\times f(\exp(\text{lsp}), x)\times(\log(x) - \text{log.ic50}) )}\text{, where}}
    
    \eqn{e_{\min} = \min y} (minimum y value), \eqn{e_{\max} = \max y} (maximum y value), \eqn{\text{log.ic50} = \log( \text{ic50} )}, m is the shape parameter, and \eqn{f(s, x)} is the switch point function.
    
 The function \eqn{f(s, x) = (s-x)/(s+x) = 2/(1+x/s) - 1}.  This function is constrained to be between -1 and +1 with \eqn{s > 0.}


  Notes:

  1.  At \eqn{x = 0}, \eqn{f(s, x) = 1}, which reduces to hill_model(theta[1:4], 0).

  2.  The \code{hill_switchpoint_model}() is more flexible compared to \code{hill_quad_model}().
  
  3.  When the data does not contain a switchpoint, then lsp should be a large value so that \eqn{f(\exp(\text{lsp}), x)} will be near 1 for all x.

}
\value{
Let N = length(x).  Then
\itemize{
\item hill_switchpoint_model(theta, x) returns a numeric vector of length N.
\item attr(hill_switchpoint_model, "gradient")(theta, x) returns an N x 5 matrix.
\item attr(hill_switchpoint_model, "start")(x, y) returns a numeric vector of length 5 with starting values for
\eqn{(e_{\min}, e_{\max}, \text{lec50}, m, \text{lsp})}.

Because \code{hill_switchpoint_model}() can be fitted to biphasic data with a hook-effect, attr(hill_switchpoint_model, "backsolve")(theta, y0) returns the first x that satisfies
y0=hill_switchpoint_model(theta, x)
}
}
\seealso{
\code{\link{optim_fit}}, \code{\link{rout_fitter}}
}
\examples{
set.seed(123L)
x = rep( c(0, 2^(-4:4)), each=3 )      ## Dose
## Create model with no switchpoint term
theta = c(0, 100, log(.5), 2)
y = hill_model(theta, x) + rnorm( length(x), mean=0, sd=5 )

## fit0 and fit return roughly the same r-squared and sigma values.
## Note that BIC(fit0) < BIC(fit), as it should be.
fit0 = optim_fit(NULL, hill_model, x=x, y=y)
fit = optim_fit(c(coef(fit0), lsp=0), hill_switchpoint_model, x=x, y=y)
fit = optim_fit(NULL, hill_switchpoint_model, x=x, y=y) 

## Generate data from hill.quad.model() with a biphasic (hook-effect) profile
set.seed(123L)
theta = c(0, 100, 2, 1, -0.5)          ## Model parameters
y = hill_quad_model(theta, x) + rnorm( length(x), mean=0, sd=5 )

## fit.qm and fit.sp return nearly identical fits
fit.qm = optim_fit(theta, hill_quad_model, x=x, y=y)  
fit.sp = optim_fit(NULL, hill_switchpoint_model, x=x, y=y, ntry=50)  

plot(log(x+0.01), y)
lines(log(x+0.01), fitted(fit.qm))
lines(log(x+0.01), fitted(fit.sp), col="red")

## Generate data from hill.switchback.model()
set.seed(123)
theta = c(0, 100, log(0.25), -3, -2)
y = hill_switchpoint_model(theta, x) + rnorm( length(x), mean=0, sd=5 )
plot( log(x+0.01), y )

## Note that this model cannot be fitted by hill.quad.model()
fit = optim_fit(NULL, hill_switchpoint_model, x=x, y=y, ntry=50, 
       start.method="fixed", until.converge=FALSE)
pred = predict(fit, x=exp(seq(log(0.01), log(16), length=50)), interval='confidence')

plot(log(x+0.01), y, main="Fitted curve with 95\% confidence bands")
lines(log(pred[,'x']+0.01), pred[,'y.hat'], col='black')
lines(log(pred[,'x']+0.01), pred[,'lower'], col='red', lty=2)
lines(log(pred[,'x']+0.01), pred[,'upper'], col='red', lty=2)


## Other functions
hill_switchpoint_model(theta, x)
attr(hill_switchpoint_model, "gradient")(theta, x)
attr(hill_switchpoint_model, "start")(x, y)
attr(hill_switchpoint_model, "backsolve")(theta, 50)

}
\author{Steven Novick}
\keyword{Nonlinear}
