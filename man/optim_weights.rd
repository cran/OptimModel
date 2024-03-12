\name{optim_weights}
\alias{weights_huber}
\alias{weights_tukey_bw}
\alias{weights_varConstPower}
\alias{weights_varExp}
\alias{weights_varIdent}
\alias{weights_varPower}
\title{Weight functions for optim_fit}
\description{ Weight functions for \code{optim_fit}. May be used when \code{fit.method=="irwls"} or \code{fit.method=="mle"}. Generally, not called by the user.  }
\usage{ 
       weights_varIdent(phi, mu)
       weights_varExp(phi, mu)
       weights_varPower(phi, mu)
       weights_varConstPower(phi, mu)
       weights_tukey_bw(phi = 4.685, resid)
       weights_huber(phi=1.345, resid)
}
\arguments{
   \item{phi}{Variance parameter(s)}
   \item{mu}{Vector of means}
   \item{resid}{Vector of model residuals}
}
\details{
\itemize{
  \item \code{weights_varIdent} returns a vector of ones.
  \item \code{weights_varExp} returns \eqn{\exp(\phi*\mu)}
  \item \code{weights_varPower} returns \eqn{|\mu|^{\phi}}
  \item \code{weights_varConstPower} returns \eqn{\phi_1 + |\mu|^{\phi_2}} where \eqn{\phi_i =\phi}[i]
  \item \code{ weights_tukey_bw} is a Tukey bi-weight function. Let 
\deqn{r=\tfrac{|\text{resid}|}{\text{mad}(\text{resid},\text{center}=\text{TRUE})}.} 
Then this function returns
\deqn{\left(1-\left(\tfrac{r}{\phi}\right)^2\right)^2\text{ whenever } r <= \phi\text{ and }0{ o.w.}}
For this the user should use \code{phi.fixed=TRUE} in the \code{optim_fit} function.
   \item\code{weights_huber} is a Huber weighting function that returns \eqn{\min(1, \phi/r)}, where \eqn{r = |\text{resid}|/\text{sig}} and \eqn{\text{sig} = \text{mad}(\text{resid}, \text{center} = \text{TRUE})}.  For this the user should use \code{phi.fixed = TRUE} in the \code{optim_fit} function.
  }
}
\value{
A vector of numeric weights.
}
\seealso{
\code{\link{optim_fit}}, \code{\link{rout_fitter}}
}
\author{Steven Novick}
\keyword{Nonlinear}
