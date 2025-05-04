\name{rout_fitter}
\alias{rout_fitter}
\title{Fit Model with ROUT}
\description{ 
  The \code{rout_fitter} method in \R fits nonlinear regression using the ROUT method as described in the reference below. The starting point is to fit a robust nonlinear regression approach assuming the Lorentzian distribution. An adaptive method then proceeds. The False Discovery Rate is used to detect outliers and the method fits in an iterative fashion. 
  
 The \code{rout_fitter} function provides a wrapper algorithm to the \code{optim} function in package \pkg{stats}.
}
\usage{
   rout_fitter(theta0 = NULL, f.model, x, y, lbs = FALSE, ntry = 0, tol = 1e-3, 
               Q = 0.01, check.pd.tol = 1e-8, \dots)
}
\arguments{
    \item{theta0}{ a numeric vector of starting values.  Alternatively, if given as NULL, \code{theta0} can be computed within [rout.fitter()]
        if a starting values function is supplied as attr(f.model, "start"), as a function of \code{x} and \code{y}. If \code{theta0} is user supplied,
        the last entry of \code{theta0} should be log(sigma), where sigma = residual standard deviation. Otherwise, log(sigma) will be
        estimated and appended to the results from attr(f.model, "start"). }
    \item{f.model}{ Name of mean model function. See detials below. }
    \item{x}{ Explanatory variable(s).  Can be a numeric \code{vector}, a \code{matrix}, or a \code{data.frame}. }
    \item{y}{ a numeric \code{vector} for the response variable. }
    \item{lbs}{Parmeter}
    \item{ntry}{Parmeter}
    \item{tol}{ Tolerance for \code{optim} algorithm. }
    \item{Q}{ The test size for ROUT testing. }
    \item{check.pd.tol}{absolute tolarence for determing whether a matrix is positive definite. Refer to \code{test_fit}.}
    \item{\dots}{ Other arguments to passed to \code{optim}.  See ?optim.  For example, lower=, upper=, method=}
}
\details{
  [rout.fitter()] is a wrapper for [optim()], specifically for Cauchy likelihood linear and non-linear regression.
  The Default algorithm uses method=\dQuote{BFGS} or \dQuote{L-BFGS-B}, if lower= and upper= arguments
  are specified.  These can easily be overridden using the \dQuote{\dots}.
  
  The assumed model is:   
 \deqn{y = \text{f.model}(theta, x) + \sigma*\epsilon \text{, where } \epsilon \sim Cauchy(0,1).}

  After the Cauchy likelihood model is fitted to data, the residuals are interrogated to determine which observations
  might be outliers.  An FDR correction is made to p-values (for outlier testing) through the p.adjust(method="fdr") function of the \pkg{stats} package.

  The package supports several mean model functions for the f.model parameter. This includes \code{beta_model}, \code{exp_2o_decay}, \code{exp_decay_pl},
  \code{gompertz_model}, \code{hill_model}, \code{hill_quad_model}, \code{hill_switchpoint_model}, \code{hill5_model} and \code{linear_model}.
}
\value{

An object with class \dQuote{rout_fit} is returned that gives a list with the following components and attributes:

par = estimated Cauchy model coefficients.  The last term is log(sigma)

value, counts, convergence = returns from [optim()]

message = character, indicating problem if any.  otherwise = NULL

hessian = hessian matrix of the objective function (e.g., sum of squares)

Converge = logical value to indicate hessian is positive definite

call = [rout.fitter()] function call

residuals = model residuals

rsdr = robust standard deviation from ROUT method

sresiduals = residuals/rsdr

outlier = logical vector.  TRUE indicates observation is an outlier via hypothesis testing with unadjust p-values.

outlier.adj = logical vector.  TRUE indicates observation is an outlier via hypothesis testing with FDR-adjust p-values.

\code{attr}(object, "Q") = test size for outlier detection

}

\seealso{
\code{\link{optim_fit}}, 
\code{\link{rout_outlier_test}},
\code{\link{beta_model}},
\code{\link{exp_2o_decay}},
\code{\link{exp_decay_pl}},
\code{\link{gompertz_model}}, 
\code{\link{hill_model}}, 
\code{\link{hill5_model}}, 
\code{\link{hill_quad_model}}, 
\code{\link{hill_switchpoint_model}}, 
\code{\link{linear_model}}
}

\references{
Motulsky, H.J. and Brown, R.E. (2006) Detecting Outliers When Fitting Data with Nonlinear Regression: A New Method Based on Robust Nonlinear Regression and the False Discovery Rate. BMC Bioinformatics, 7, 123.
}


\examples{
set.seed(123L)
x = rep( c(0, 2^(-4:4)), each=4 )
theta = c(0, 100, log(.5), 2)
y = hill_model(theta, x) + rnorm( length(x), sd=2 )

rout_fitter(NULL, hill_model, x=x, y=y)
rout_fitter(c( theta, log(2) ), hill_model, x=x, y=y)

ii = sample( 1:length(y), 2 )
y[ii] = hill_model(theta, x[ii]) + 5.5*2 + rnorm( length(ii), sd=2 )

rout_fitter(c( theta, log(2) ), hill_model, x=x, y=y, Q=0.01)
rout_fitter(c( theta, log(2) ), hill_model, x=x, y=y, Q=0.05)
rout_fitter(c( theta, log(2) ), hill_model, x=x, y=y, Q=0.0001)

## Use optim method="L-BFGS-B"
rout_fitter(NULL, hill_model, x=x, y=y, Q=0.01, lower=c(-2, 95, NA, 0.5), upper=c(5, 110, NA, 4) )

}
\author{Steven Novick}
\keyword{Nonlinear}