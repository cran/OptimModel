\name{beta_model}
\alias{beta_model}
\title{Beta hook-effect model, gradient, starting values, and back-calculation functions }
\description{ Five-parameter hook-effect model for dose-response curve fitting  }
\usage{ 
       beta_model(theta, x)
}
\arguments{
    \item{theta}{ Vector of five parameters:  \eqn{ (e_{\min}, e_{\max}, \log(\delta_1), \log(\delta_2), \log(\delta_3)) }.  See details.}
    \item{x}{ Vector of concentrations for the Beta model. }
}
\details{
  The five-parameter Beta model is given by:
  
  \deqn{y = e_{\min} + e_{\max} \times \exp( log( \beta(\delta_1, \delta_2) ) + \delta_1 \times\log(x) + \delta_2*\log(\text{sc}-x) - 
                      (\delta_1+\delta_2)\times\log(\text{sc})   }

  where 
  
  \deqn{\beta(\delta_1, \delta_2) = (\delta_1+\delta_2)^(\delta_1+\delta_2) /(\delta_1^{\delta_1} \times \delta_2^{\delta_2})}
              
  and
  
  \deqn{\text{sc} = \max(x) + \delta_3. }
  
  Note that the Beta model depends on the maximum x value. For a particular data set, this may be set by
  
  attr(theta), "maxX") = max(x).
}
\value{
Let N = length(x).  Then
\itemize{
\item beta_model(theta, x) returns a numeric vector of length N.
\item attr(beta_model, "gradient")(theta, x) returns an N x 5 matrix.
\item attr(beta_model, "start")(x, y) returns a numeric vector of length 5 with starting values for
\deqn{(e_{\min}, e_{\max}, \log(\delta_1), \log(\delta_2), \log(\delta_3)).}       
\item attr(beta_model, "backsolve")(theta, y) returns a numeric vector of length=length(y) with the first x such that beta_model(theta, x)=y.
}
}
\seealso{
\code{\link{optim_fit}}, \code{\link{rout_fitter}}
}
\examples{
set.seed(123L)
x = rep( c(0, 2^(-4:4)), each=4 )
theta = c(emin=0, emax=115, ldelta1=-1.5, ldelta2=9, ldelta3=11.5)
y = beta_model(theta, x)  + rnorm( length(x), mean=0, sd=1 )

beta_model(theta, x)
attr(beta_model, "gradient")(theta, x)
attr(beta_model, "start")(x, y)

attr(theta, "maxX") = max(x)
attr(beta_model, "backsolve")(theta, 50)
}
\author{Steven Novick}
\keyword{Nonlinear}

