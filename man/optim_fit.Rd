\name{optim_fit}
\alias{optim_fit}
\alias{robust_fit}
\title{Fit Model with optim}
\description{ 
Fit nonlinear model using the \code{optim} function in the \pkg{stats} library.  This defaults to Ordinary Least Squares (OLS) The other options are Iterative Reweighted Least Squares (IRWLS), and Maximum Likelihood Estimator (MLE).
}
\usage{

optim_fit( theta0, f.model, gr.model=NULL, x, y, wts,
            fit.method=c("ols", "irwls", "mle"),
            var.method=c("hessian", "normal", "robust"),
            phi0=NULL, phi.fixed=TRUE, conf.level = 0.95, tol = 1e-3, 
            n.start=1000, ntry=1, start.method=c("fixed", "random"), 
            until.converge=TRUE, check.pd.tol = 1e-8, \dots)


robust_fit(theta0, f.model, gr.model=NULL, x, y, wts=c("huber", "tukey"), 
    var.method=c("hessian", "normal", "robust"), conf.level=.95, tol=1e-3, \dots)             
}
\arguments{
    \item{theta0}{ starting values.  Alternatively, if given as NULL, theta0 can be computed within \code{optim.fit}()
        if a starting values function is supplied as attr(f.model, "start"), as a function of x and y.}
    \item{f.model}{ Name of mean model function. }
    \item{gr.model}{ If specified, name of the partial derivative of f.model with respect to its parameter argument.  If
            not specified, \code{f2djac} will approximate the derivative.  Alternatively, the gradient of f.model can
            be specified as attr(f.model, "gradient") }
    \item{x}{ Explanatory variable(s).  Can be \code{vector}, \code{matrix}, or \code{data.frame} }
    \item{y}{ Response variable. }
    \item{fit.method}{ "ols" for ordinary least squares, "irwls" for iterative re-weighted least squares, "mle" for
            maximum likelihood. }
    \item{wts}{ For \code{optim.fit}(), can be a numeric vector or a function.  Functions supplied in the library are \code{weights_varIdent}, \code{weights_tukey_bw}, \code{weights_huber}, \code{weights_varExp}, \code{weights_varPower}, and \code{weights_varConstPower}.  For \code{robust_fit}(), choices are a character string of "huber" for \code{weights_huber} and "tukey" for \code{weights_tukey_bw} }
    \item{var.method}{ Method to compute variance-covariance matrix of estimated model parameters.  Choices are "hessian" to
        use the hessian inverse, "normal" to use the so-called 'folk-lore' theorem estimator, and "robust" to use a
        sandwich-variance estimator.  When \code{fit.method = "mle"}, \code{var.method = "hessian"} and cannot be overridden. }
    \item{phi0}{ Not meaningful for \code{fit.method = "ols"}.  Starting value(s) for variance parameters (for weights). }
    \item{phi.fixed}{ Not meaningful for \code{fit.method = "ols"}.  If set to \code{TRUE}, the variance parameter(s) remain fixed at the given starting value, phi0.  Otherwise, the variance parameter(s) are estimated. }
    \item{conf.level}{ Confidence level of estimated parameters. }
    \item{tol}{ Tolerance for \code{optim} algorithm. }
    \item{n.start}{ Number of starting values to generate (see details). }
    \item{ntry}{ Maximum number of calls to \code{optim.fit}(). }
    \item{start.method}{ Parameter }
    \item{until.converge}{ Logical (\code{TRUE}/\code{FALSE}) indicating when algorithm should stop. }
    \item{check.pd.tol}{absolute tolarence for determing whether a matrix is positive definite. Refer to \code{test_fit}.}
    \item{\dots}{ Other arguments to passed to \code{optim}.  See ?\code{optim}.  For example, lower=, upper=, method=}
}
\details{
  \code{optim_fit}() is a wrapper for \code{stats::optim}(), specifically for non-linear regression.
  The Default algorithm is ordinary least squares (ols) using method="BFGS" or "L-BFGS-B", if lower= and upper=
  are specified.  These can easily be overridden.
  
  The assumed model is: 
  
  \deqn{y = \text{f.model}(\theta, x) + g(\theta, \phi, x)\sigma\epsilon\text{, where }\epsilon\sim N(0, 1).}
  
  Usually the function \eqn{g(\cdot) = 1}.
  
  With the exception of weights.tukey.bw and weights.huber, the weights functions are equivalent 
  to \eqn{g(\theta, \phi, x)}.
  
  Note that \code{robust_fit}() is a convenience function to simplify the model call with \code{fit.method = "irwls"},
  \code{phi0 = NULL}, and \code{phi.fixed = TRUE}.
  
  \bold{Algorithms}:
  
  1.  OLS.   Minimize  sum(  (y - f.model(theta, x))^2 )
  
  2.  IRWLS. Minimize sum( g(theta, phi, x)*(y - f.model(theta, x))^2 ), where g(theta, phi, x) act as weights.  See section
      on Variance functions below for more details on \eqn{g(\cdot)}.
      
  3.  MLE.   Minimize the -log(Likelihood).  See section on Variance functions below for more details on \eqn{g(\cdot)}.

      
  \bold{Variance functions}:
  
  Weights are given by some variance function.  Some common variance functions are supplied.
  
  See \code{weights_varIdent}, \code{weights_varExp}, \code{weights_varPower}, \code{weights_varConstPower}, \code{weights_tukey_bw}, \code{weights_huber}.
  
  User-specified variance functions can be provided for weighted regression.
  
}
\value{
The returned object is a list with the following components and attributes:

coefficients = estimated model coefficients

value, counts, convergence = returns from optim()

message = character, indicating problem if any.  otherwise=NULL

hessian = hessian matrix of the objective function (e.g., sum of squares)

fit.method = chosen fit.method (e.g., "ols")

var.method = chosen var.method (e.g., "hessian")

call = optim.fit() function call

fitted, residuals = model mean and model residuals

r.squared, bic = model statistics

df = error degrees of freedom = N - p, where N = # of observations and p = # of parameters

dims = list containing the values of N and p

sigma = estimated standard deviation parameter

varBeta = estimated variance-covariance matrix for the coefficients

beta = data.frame summary of the fit

attr(object, "weights") = weights

attr(object, "w.func") = weights model for the variance

attr(object, "var.param") = variance parameter values

attr(object, "converge.pls") = logical indicating if IRWLS algorithm converged.

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
\code{\link{linear_model}},
\code{\link{weights_huber}},
\code{\link{weights_tukey_bw}},
\code{\link{weights_varConstPower}},
\code{\link{weights_varExp}},
\code{\link{weights_varIdent}},
\code{\link{weights_varPower}}
}
\examples{
set.seed(123L)
x = rep( c(0, 2^(-4:4)), each=4 )
theta = c(0, 100, log(.5), 2)
y1 = hill_model(theta, x) + rnorm( length(x), sd=2 )
y2 = hill_model(theta, x) + rnorm( length(x), sd=.1*hill_model(theta, x) )
wts = runif( length(y1) )
fit1=optim_fit(theta, hill_model, x=x, y=y1)
fit2=optim_fit(theta, hill_model, x=x, y=y1, wts=weights_varIdent)
fit3=optim_fit(theta, hill_model, x=x, y=y1, wts=wts)
fit4=optim_fit(theta, hill_model, attr(hill_model, "gradient"), x=x, y=y1, wts=wts)

fit5=optim_fit(NULL, hill_model, x=x, y=y2, wts=weights_varPower, fit.method="irwls")
fit6=optim_fit(theta, hill_model, x=x, y=y2, wts=weights_varPower, fit.method="mle")

fit7=optim_fit(theta, hill_model, x=x, y=y2, wts=weights_varPower, fit.method="irwls", 
               phi0=0.5, phi.fixed=FALSE)
fit8=optim_fit(theta, hill_model, x=x, y=y2, wts=weights_varPower, fit.method="mle", 
              phi0=0.5, phi.fixed=FALSE)

fit9a=optim_fit(theta, hill_model, x=x, y=y1, wts=weights_tukey_bw, fit.method="irwls", 
             phi0=4.685, phi.fixed=TRUE)

fit9b=robust_fit(theta, hill_model, x=x, y=y1, wts="tukey")

}
\author{Steven Novick}
\keyword{Nonlinear}