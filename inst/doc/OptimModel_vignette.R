## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE, results='asis'-----------------------------------------------

d = data.frame( Attribute=c("attr(f_model, 'gradient')", "attr(f_model, 'backsolve')",
                            "attr(f_model, 'start')"),
                Purpose=c("Gradient of f_model with respect to theta", 
                          "Find x such that f_model(theta, x) = y",
                          "Starting values for optimization")
)
                           
knitr::kable(d)

## ----echo=FALSE, results='asis'-----------------------------------------------

d = data.frame( Function=c("weights_varIdent(phi, mu)", "weights_varExp(phi, mu)", 
                           "weights_varPower(phi, mu)", "weights_varConstPower(phi, mu)", 
                           "weights_tukey_bw(phi=4.685, resid)", "weights_huber(phi=1.345, resid)"),
                Code=c("rep(1, length(mu))", "exp(phi*mu)", "abs(mu)^(phi)", "phi[1]+abs(mu)^(phi[2])",
                       "see below", "see below") )
                           
knitr::kable(d)

## ----echo=TRUE----------------------------------------------------------------

library(OptimModel)

set.seed(456)
x = rep( c(0, 2^(-4:4)), each=4 )
theta = c(0, 100, log(.5), 2)
y = hill_model(theta, x) + rnorm( length(x), sd=2 )

  ## No need to include a starting value.
fit = optim_fit(theta0=NULL, f.model=hill_model, x=x, y=y)
print(fit)

## Coefficients
coef(fit)

## Fitted values
fitted(fit)

## Residuals
residuals(fit)

## ----echo=TRUE----------------------------------------------------------------
predict(fit, x=2:5)

predict(fit, x=2:5, se.fit=TRUE)

## 95% confidence interval
predict(fit, x=2:5, interval="confidence")

## 90% confidence interval
predict(fit, x=2:5, interval="confidence", level=0.9)

## 95% prediciton interval for next observation (K=1)
predict(fit, x=2:5, interval="prediction", K=1)


d = data.frame(x=x, y=y)
pred = as.data.frame(predict(fit, x=2^seq(-6, 4, length=25), interval="confidence"))

library(ggplot2)
ggplot( d, aes(x=x, y=y) ) + geom_point() + 
  geom_line( data=pred, aes(x, y.hat)) +
  geom_ribbon( data=pred, aes(x=x, y=y.hat, ymin=lower, ymax=upper), alpha=0.3) +
  scale_x_log10() + ggtitle("OLS")

## Fixed weights.  In this example, weights are increasing step-wise with x
w = ifelse(x < 0.1, 0.5, ifelse(x < 2, 1, 2))
  ## No need to include a starting value.
fit = optim_fit(theta0=NULL, f.model=hill_model, x=x, y=y, wts=w)
print(fit)

pred = as.data.frame(predict(fit, x=2^seq(-6, 4, length=25), interval="confidence"))

ggplot( d, aes(x=x, y=y) ) + geom_point() + 
  geom_line( data=pred, aes(x, y.hat)) +
  geom_ribbon( data=pred, aes(x=x, y=y.hat, ymin=lower, ymax=upper), alpha=0.3) +
  scale_x_log10() + ggtitle("WLS")

## ----echo=TRUE----------------------------------------------------------------

fit = optim_fit(theta0=NULL, f.model=hill_model, x=x, y=y, fit.method="mle")
print(fit)

## ----echo=TRUE----------------------------------------------------------------

set.seed(876)

## Standard deviation = sigma*( Mean )^0.5
y2 = hill_model(theta, x) + rnorm( length(x), sd=0.7*sqrt(hill_model(theta, x)) )


fit = optim_fit(theta0=NULL, f.model=hill_model, x=x, y=y2, wts=weights_varPower,
                fit.method="mle", phi.fixed=FALSE)
print(fit)

## ----echo=TRUE----------------------------------------------------------------

fit = optim_fit(theta0=NULL, f.model=hill_model, x=x, y=y2, wts=weights_varPower,
                fit.method="irwls", phi.fixed=FALSE)
print(fit)


d = data.frame(x=x, y=y2)
pred = as.data.frame(predict(fit, x=2^seq(-6, 4, length=25), interval="confidence"))

ggplot( d, aes(x=x, y=y) ) + geom_point() + 
  geom_line( data=pred, aes(x, y.hat)) +
  geom_ribbon( data=pred, aes(x=x, y=y.hat, ymin=lower, ymax=upper), alpha=0.3) +
  scale_x_log10()


## ----echo=TRUE----------------------------------------------------------------

set.seed(456)
x = rep( c(0, 2^(-4:4)), each=4 )
theta = c(0, 100, log(.5), 2)
y = hill_model(theta, x) + rnorm( length(x), sd=2 )

## Create outliers
y[c(8, 11)] = y[c(8, 11)] + -30

## Ordinary least squares
fit = optim_fit(theta0=NULL, f.model=hill_model, x=x, y=y)


d = data.frame(x=x, y=y)
pred = as.data.frame(predict(fit, x=2^seq(-6, 4, length=25), interval="confidence"))

ggplot( d, aes(x=x, y=y) ) + geom_point() + 
  geom_line( data=pred, aes(x, y.hat)) +
  geom_ribbon( data=pred, aes(x=x, y=y.hat, ymin=lower, ymax=upper), alpha=0.3) +
  scale_x_log10() + ggtitle("OLS fit")

## IRWLS with Huber weights
fit = optim_fit(theta0=NULL, f.model=hill_model, x=x, y=y, wts=weights_huber,
                fit.method="irwls")

d = data.frame(x=x, y=y)
pred = as.data.frame(predict(fit, x=2^seq(-6, 4, length=25), interval="confidence"))

ggplot( d, aes(x=x, y=y) ) + geom_point() + 
  geom_line( data=pred, aes(x, y.hat)) +
  geom_ribbon( data=pred, aes(x=x, y=y.hat, ymin=lower, ymax=upper), alpha=0.3) +
  scale_x_log10() + ggtitle("IRWLS + Huber weights fit")



## ----echo=TRUE----------------------------------------------------------------

set.seed(123)
theta = c(0, 100, log(0.25), -3, -2)
y = hill_switchpoint_model(theta, x) + rnorm( length(x), mean=0, sd=5 )

## Try OLS, but original starting value does not provide convergence
fit1 = optim_fit(NULL, hill_switchpoint_model, x=x, y=y) 
print(fit1)


## Fixed grid of equally-spaced starting values
fit2 = optim_fit(NULL, hill_switchpoint_model, x=x, y=y, ntry=50, 
       start.method="fixed", until.converge=FALSE)
print(fit2)

## Random grid of starting values
fit3 = optim_fit(NULL, hill_switchpoint_model, x=x, y=y, ntry=50, 
       start.method="fixed", until.converge=FALSE)
print(fit3)

d = data.frame(x=x, y=y)
x.seq = 2^seq(-6, 4, length=25)
pred = data.frame(x=rep(x.seq, 3),
                  Fit=rep(c("1 starting value", "Fixed grid", "Random grid"), each=25),
                  mu=c(predict(fit1, x=x.seq)[,"y.hat"],
                       predict(fit2, x=x.seq)[,"y.hat"],
                       predict(fit3, x=x.seq)[,"y.hat"])
                )

ggplot( d, aes(x=x, y=y) ) + geom_point() + 
  geom_line( data=pred, aes(x, mu, col=Fit)) +
  scale_x_log10() + theme(legend.position="top") +
  guides(col=guide_legend(ncol=2))



## ----echo=TRUE----------------------------------------------------------------

set.seed(456)
x = rep( c(0, 2^(-4:4)), each=4 )
theta = c(0, log(.5), 2)

hill3_model = function(theta, x){ 
  ## Parameters are theta = (emin, lec50, m).  Assumes emax = 100.
  mu = theta[1] + (100 - theta[1])/(1+exp(theta[3]*log(x) - theta[3]*theta[2]))
  return(mu)
  }

y = hill3_model(theta, x) + rnorm( length(x), sd=2 )

fit = optim_fit(theta0=c(emin=-1, lec50=log(0.5), m=1), f.model=hill3_model, x=x, y=y)
print(fit)

## ----echo=TRUE----------------------------------------------------------------

set.seed(456)
x = rep( c(0, 2^(-4:4)), each=4 )
theta = c(0, 100, log(.5), 2)
y = hill_model(theta, x) + rnorm( length(x), sd=2 )

fit = optim_fit(theta0=NULL, f.model=hill_model, x=x, y=y)

myfunc = function(theta){ theta[3] + (1/theta[4])*log( 0.5*theta[2]/(0.5*theta[2] - theta[1])) }
myfunc( coef(fit) )
get_se_func(object=fit, Func=myfunc) 

## ----echo=TRUE----------------------------------------------------------------

set.seed(456)
x = rep( c(0, 2^(-4:4)), each=4 )
theta = c(0, 100, log(.5), 2)
y = hill_model(theta, x) + rnorm( length(x), sd=2 )

## Create outliers
y[c(8, 11)] = y[c(8, 11)] + -30

## ROUT
fit_r = rout_fitter(theta0=NULL, f.model=hill_model, x=x, y=y)
print(fit_r)

## Get the outliers and remove them
print(which(fit_r$outlier.adj))
index = !fit_r$outlier.adj
x.clean = x[index]
y.clean = y[index]

## Refit model with OLS, but outliers removed
fit = optim_fit(NULL, hill_model, x=x.clean, y=y.clean)


d = data.frame(x=x, y=y)
pred = as.data.frame(predict(fit, x=2^seq(-6, 4, length=25), interval="confidence"))

ggplot( d, aes(x=x, y=y) ) + geom_point() + 
  geom_line( data=pred, aes(x, y.hat)) +
  geom_ribbon( data=pred, aes(x=x, y=y.hat, ymin=lower, ymax=upper), alpha=0.3) +
  scale_x_log10() + ggtitle("OLS fit after ROUT")



