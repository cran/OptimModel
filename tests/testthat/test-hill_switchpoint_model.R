test_that("hill_switchpoint_model", {

  x = c(0, 2^c(-4, 0, 4))
  theta = c(0, 100, log(0.25), -3, -2)         ## Model parameters
  
  ## Mean
  mu = hill_switchpoint_model(theta, x)
  
  expect_equal( mu, c(0, 17.783, 4.041, 0.00047), tolerance=1e-3 )
  
  ## Gradient
  G.f2djac = rbind(  c(1, 0, 0, 0, 0),
              c(0.822, 0.178, -16.148, 7.462, -26.282),
              c(0.960, 0.040, 8.86, 4.094, 3.387),
              c(1, 4.7e-6, 0.0014, 0.0019, 9.76e-5)
              )
  
  G = attr(hill_switchpoint_model, "gradient")(theta, x)
  expect_equal( G, G.f2djac, tolerance=1e-3 )
  
  ## Starting values
  y = mu + c(-1, 1, 3, 0.5)
  theta0 = c(emin=-1, emax=0.5, ec50=0, m=-1, lsp=0)
  theta0.start = attr(hill_switchpoint_model, "start")(x, y)
  
  expect_equal( theta0.start, theta0, tolerance=1e-3 )  

  ## Backsolve
  x0 = attr(hill_switchpoint_model, "backsolve")(theta, 40)
  x1 = attr(hill_switchpoint_model, "backsolve")(theta, 40, log=TRUE)
  y0 = hill_switchpoint_model(theta, x0)
  y1 = hill_switchpoint_model(theta, exp(x1))
  
  expect_equal( y0, 40, tolerance=1e-3 )
  expect_equal( y1, 40, tolerance=1e-3)
  
})