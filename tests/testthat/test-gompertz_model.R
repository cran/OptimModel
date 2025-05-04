test_that("gompertz_model", {

  x = c(0, 2^c(-4, 0, 4))
  theta = c(0, 100, log(.5), 2)       ## Model parameters
  
  ## Mean
  mu = gompertz_model(theta, x) 
  
  expect_equal( mu, c(1.8531, 2.170, 13.533, 99.994), tolerance=1e-3 )
  
  ## Gradient
  G.f2djac = rbind(  c(0.982, 0.0183, 14.653, -5.078),
              c(0.978, 0.022, 16.105, -5.762),
              c(0.8647, 0.135, 27.067, -18.761),
              c(6.11e-5, 1, -0.085, -0.0042)
              )
  
  G = attr(gompertz_model, "gradient")(theta, x)
  expect_equal( G, G.f2djac, tolerance=1e-3 )
  
  ## Starting values
  y = mu + c(-1, 1, 3, 0.5)
  theta0 = c(A=0.823, B=101.499, m=-0.386, offset=3.995)
  theta0.start = attr(gompertz_model, "start")(x, y)
  
  expect_equal( theta0.start, theta0, tolerance=1e-3 )  

  ## Backsolve
  x0 = attr(gompertz_model, "backsolve")(theta, 40)
  x1 = attr(gompertz_model, "backsolve")(theta, 40, log=TRUE)
  y0 = gompertz_model(theta, x0)
  y1 = gompertz_model(theta, exp(x1))
  
  expect_equal( y0, 40, tolerance=1e-3 )
  expect_equal( y1, 40, tolerance=1e-3 )
  
})