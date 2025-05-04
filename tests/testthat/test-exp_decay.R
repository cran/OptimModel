test_that("exp_decay", {

  x = c(0, 2^c(-4, 0, 4))
  theta = c(25, 75, log(3))      ## Model parameters
  
  ## Mean
  mu = exp_decay(theta, x) 
  
  expect_equal( mu, c(100, 87.177, 28.734, 25), tolerance=1e-3 )
  
  ## Gradient
  G.f2djac = rbind(  c(1, 1, 0),
              c(1, 0.829, -11.658),
              c(1, 0.050, -11.2),
              c(1, 0, 0)
              )
  
  G = attr(exp_decay, "gradient")(theta, x)
  expect_equal( G, G.f2djac, tolerance=1e-3 )
  
  ## Starting values
  y = mu + c(-1, 1, 3, 0.5)
  theta0 = c(A=39.183, B=35.773, k=-1.157)
  theta0.start = attr(exp_decay, "start")(x, y)
  
  expect_equal( theta0.start, theta0, tolerance=1e-3 )  

  ## Backsolve
  x0 = attr(exp_decay, "backsolve")(theta, 40)
  x1 = attr(exp_decay, "backsolve")(theta, 40, log=TRUE)
  y0 = exp_decay(theta, x0)
  y1 = exp_decay(theta, exp(x1))
  
  expect_equal( y0, 40, tolerance=1e-3 )
  expect_equal( y1, 40, tolerance=1e-3 )
  
})