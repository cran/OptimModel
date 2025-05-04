test_that("exp_decay_pl", {

  x = c(0, 2^c(-4, 1, 4))
  theta = c(0.4, 75, 10, log(3))     ## Model parameters
  
  ## Mean
  mu = exp_decay_pl(theta, x) 
  
  expect_equal( mu, c(75, 75, 24.152, 10), tolerance=1e-3 )
  
  ## Gradient
  G.f2djac = rbind(  c(0, 1, 0, 0),
              c(0, 1, 0, 0),
              c(63.337, 0.218, 0.782, -21.575),
              c(0, 0, 1, 0)
              )
  
  G = attr(exp_decay_pl, "gradient")(theta, x)
  expect_equal( G, G.f2djac, tolerance=1e-3 )
  
  ## Starting values
  y = mu + c(-1, 1, 3, 0.5)
  theta0 = c(x0=1.0987, yMax=76.001, yMin=10.499, k=0.528)
  theta0.start = attr(exp_decay_pl, "start")(x, y)
  
  expect_equal( theta0.start, theta0, tolerance=1e-3 )  

  ## Backsolve
  x0 = attr(exp_decay_pl, "backsolve")(theta, 40)
  x1 = attr(exp_decay_pl, "backsolve")(theta, 40, log=TRUE)
  y0 = exp_decay_pl(theta, x0)
  y1 = exp_decay_pl(theta, exp(x1))
  
  expect_equal( y0, 40, tolerance=1e-3 )
  expect_equal( y1, 40, tolerance=1e-3 )
  
})