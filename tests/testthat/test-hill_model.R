test_that("hill_model", {

  x = c(0, 2^c(-4, 0, 4))
  theta = c(0, 100, log(.5), 2)
  
  ## Mean
  mu = hill_model(theta, x)
  
  expect_equal( mu, c(100, 98.462, 20, 0.098), tolerance=1e-3 )
  
  ## Gradient
  G.f2djac = rbind(  c(0, 1, 0, 0),
              c(0.015, 0.985, 3.030, 3.15),
              c(0.8, 0.2, 32, -11.09),
              c(0.999, 0.000976, 0.195, -0.338)
              )
  
  G = attr(hill_model, "gradient")(theta, x)
  expect_equal( G, G.f2djac, tolerance=1e-3 )
  
  ## Starting values
  y = mu + c(-1, 1, 3, 0.5)
  theta0 = c(emin=0.597, emax=99, lec50=-1.222, m=1)
  theta0.start = attr(hill_model, "start")(x, y)
  
  expect_equal( theta0.start, theta0, tolerance=1e-3 )  

  ## Backsolve
  x0 = attr(hill_model, "backsolve")(theta, 40)
  x1 = attr(hill_model, "backsolve")(theta, 40, log=TRUE)
  y0 = hill_model(theta, x0)
  y1 = hill_model(theta, exp(x1))
  
  expect_equal( y0, 40, tolerance=1e-3 )
  expect_equal( y1, 40, tolerance=1e-3)
  
})