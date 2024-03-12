test_that("optim_fit", {

  x = c(0, 2^(-4:4))
  theta = c(0, 100, log(.5), 2)
  y1 = c(99.91, 96.892, 90.782, 79.24, 51.838, 18.849, 7.098, -1.697, 0.278, 1.136)
  
  w = c(0.531, 0.785, 0.168, 0.404, 0.472, 0.868, 0.926, 0.882, 0.674, 0.95)
  
  y2 = c(100.412, 94.302, 74.793, 89.051, 42.697, 21.48, 7.005, 1.316, 0.416, 0.095)

  ## OLS
  fit1 = optim_fit(NULL, hill_model, x=x, y=y1)
  
  expect_equal( coef(fit1), c(emin=-0.641, emax=98.402, lec50=-0.661, m=1.977), tolerance=1e-3 )  
  expect_equal( c(fit1$sigma, fit1$r.sq, fit1$bic, fit1$df), c(1.963, 0.9987, 48.2729, 6), tolerance=1e-3 )    

  ## WLS
  fit2 = optim_fit(NULL, hill_model, x=x, y=y1, wt=w)
  
  expect_equal( coef(fit2), c(emin=-0.3979, emax=98.6384, lec50=-0.6683, m=2.0246), tolerance=1e-3 )  
  expect_equal( c(fit2$sigma, fit2$r.sq, fit2$bic, fit2$df), c(1.6117, 0.9986, 49.4347, 6), tolerance=1e-3 )    

  ## IRWLS 
  fit3a=optim_fit(NULL, hill_model, x=x, y=y2, wts=weights_varPower, fit.method="irwls", phi0=0.5, phi.fixed=TRUE)
  expect_equal( coef(fit3a), c(emin=-0.0438, emax=95.2918, lec50=-0.6754, m=1.923), tolerance=1e-3 )
  expect_equal( c(fit3a$sigma, fit3a$r.sq, fit3a$bic, fit3a$df), c(0.9838, 0.9998, 58.319, 6), tolerance=1e-3 )
  expect_equal( attr(fit3a, "var.param"), 0.5 )
  
    ## Why did coefficient names disappear?
  fit3b=optim_fit(theta, hill_model, x=x, y=y2, wts=weights_varPower, fit.method="irwls", phi0=0.5, phi.fixed=FALSE)
  ##expect_equal( coef(fit3b), c(-0.0009, 93.6834, -0.6194, 2.028), tolerance=1e-3 )
  expect_equal( c(fit3b$sigma, fit3b$r.sq, fit3b$bic, fit3b$df), c(0.1087, 1, 43.906, 6), tolerance=1e-3 )
  ##expect_equal( attr(fit3b, "var.param"), 1.1221, tolerance=1e-3 )


  ## MLE
  fit4a=optim_fit(NULL, hill_model, x=x, y=y2, wts=weights_varPower, fit.method="mle", phi0=0.5, phi.fixed=TRUE)  

  expect_equal( coef(fit4a), c(emin=-0.0827, emax=94.706, lec50=-0.6570, m=2.010, lsigma=-0.2195), tolerance=1e-3 )  
  expect_equal( c(fit4a$sigma, fit4a$r.sq, fit4a$bic, fit4a$df), c(lsigma=0.8029, 0.9999, 59.234, Inf), tolerance=1e-3 )    
    
    ## Why did coefficient names disappear?
  fit4b=optim_fit(theta, hill_model, x=x, y=y2, wts=weights_varPower, fit.method="mle", phi0=0.5, phi.fixed=FALSE)
  expect_equal( coef(fit4b), c(par.1=-0.0021, par.2=93.0056, par.3=-0.61033, par.4=2.0290, v1=1.0823, lsigma=-2.387), tolerance=1e-3 )  
  expect_equal( c(fit4b$sigma, fit4b$r.sq, fit4b$bic, fit4b$df), c(lsigma=0.0919, 1, 48.357, Inf), tolerance=1e-3 )    
  expect_equal( attr(fit4b, "var.param"), 1.082, tolerance=1e-3 )

  ## Robust fit with IRWLS
  y1[3] = y1[3] - 25
  fit5=optim_fit(theta, hill_model, x=x, y=y1, wts=weights_huber, fit.method="irwls", phi0=1.345, phi.fixed=TRUE)  
  expect_equal( coef(fit5), c(-0.7433, 98.0296, -0.6584, 1.9523), tolerance=1e-3 )  
  expect_equal( c(fit5$sigma, fit5$r.sq, fit5$bic, fit5$df), c(4.252, 0.9928, 65.8193, 6), tolerance=1e-3 )  
  expect_equal( attr(fit5, "weights"), c(1, 1, 0.1237, 1, 1, 1, 1, 1, 1, 1), tolerance=1e-3 )  
  
  
})
