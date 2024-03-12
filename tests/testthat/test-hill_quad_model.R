test_that("hill_quad_model", {

  x = c(0, 2^c(-4, 0, 4))
  theta = c(0, 100, 2, 1, -0.5)          ## Model parameters
  
  ## Mean
  mu = hill_quad_model(theta, x)
  
  expect_equal( mu, c(0, 0.979, 88.080, 71.690), tolerance=1e-3 )
  
  ## Gradient
  G.f2djac = rbind(  c(1, 0, 0, 0, 0),
              c(0.990, 0.0098, 0.970, -2.689, 7.545),
              c(0.119, 0.881, 10.499, 0, 0),
              c(0.283, 0.717, 20.297, 56.275, 156.0)
              )
  
  G = attr(hill_quad_model, "gradient")(theta, x)
  expect_equal( G, G.f2djac, tolerance=1e-3 )
  
  ## Starting values
  y = mu + c(-1, 1, 3, 0.5)
  theta0 = c(A=-2.97, B=96.03, a=2.944, b=0.738, c=-0.5)
  theta0.start = attr(hill_quad_model, "start")(x, y)
  
  expect_equal( theta0.start, theta0, tolerance=1e-3 )  

  ## Backsolve
  x0 = attr(hill_quad_model, "backsolve")(theta, 40)
  x1 = attr(hill_quad_model, "backsolve")(theta, 40, log=TRUE)
  y0 = hill_quad_model(theta, x0)
  y1 = hill_quad_model(theta, exp(x1))
  
  expect_equal( y0, c(40, 40), tolerance=1e-3 )
  expect_equal( y1, c(40, 40), tolerance=1e-3)
  
})