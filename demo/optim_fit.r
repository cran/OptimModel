require(OptimModel)
set.seed(123)

x = rep( c(0, 2^(-4:4)), each=4 )
theta = c(0, 100, log(.5), 2)
y1 = hill_model(theta, x) + rnorm( length(x), sd=2 )
y2 = hill_model(theta, x) + rnorm( length(x), sd=.1*hill_model(theta, x) )
wts = runif( length(y1) )

a=optim_fit(theta, hill_model, x=x, y=y1)
a=optim_fit(theta, hill_model, x=x, y=y1, weights=weights_varIdent)
b1=optim_fit(theta, hill_model, x=x, y=y1, wts=wts)
b2=optim_fit(theta, hill_model, attr(hill_model, "gradient"), x=x, y=y1, wts=wts)

optim_fit(theta, hill_model, x=x, y=y2, wts=weights_varPower, fit.method="irwls")
optim_fit(theta, hill_model, x=x, y=y2, wts=weights_varPower, fit.method="mle")

a=optim_fit(theta, hill_model, x=x, y=y2, wts=weights_varPower, fit.method="irwls", phi.fixed=FALSE)
b=optim_fit(theta, hill_model, x=x, y=y2, wts=weights_varPower, fit.method="mle", phi.fixed=FALSE)

b=optim_fit(theta, hill_model, x=x, y=y1, wts=weights_tukey_bw, fit.method="irwls")
