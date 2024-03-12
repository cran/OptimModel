test_that("exp_2o_decay", {

  x = c(0, 2^c(-4, 0, 4))
  theta = c(25, 75, log(3), log(1.2), 1/(1+exp(.7)))    ## Model parameters
  
  ## Mean
  mu = exp_2o_decay(theta, x) 
  
  expect_equal( mu, c(100, 91.488, 39.711, 25), tolerance=1e-3 )
  
  ## Gradient
  G.f2djac = rbind(  c(1, 1, 0, 0, 0),
              c(1, 0.887, -4.870, -3.038, 1.801),
              c(1, 0.196, -4.680, -15.782, 4.586),
              c(1, 0, 0, 0, 0)
              )
  
  G = attr(exp_2o_decay, "gradient")(theta, x)
  expect_equal( G, G.f2djac, tolerance=1e-3 )
  
  ## Starting values
  y = mu + c(-1, 1, 3, 0.5)
  theta0 = c(A=34.136, B=50.533, log.k1=-1.0986, log.k2=-1.0986, logit.p=0)
  theta0.start = attr(exp_2o_decay, "start")(x, y)
  
  expect_equal( theta0.start, theta0, tolerance=1e-3 )  

  ## Backsolve
  x0 = attr(exp_2o_decay, "backsolve")(theta, 40)
  x1 = attr(exp_2o_decay, "backsolve")(theta, 40, log=TRUE)
  y0 = exp_2o_decay(theta, x0)
  y1 = exp_2o_decay(theta, exp(x1))
  
  expect_equal( y0, 40, tolerance=1e-3 )
  expect_equal( y1, 40, tolerance=1e-3 )
  
})