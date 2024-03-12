test_that("hill5_model", {

  x = c(0, 2^c(-4, 0, 4))
  theta = c(0, 100, log(.5), 2, log(10))        ## Model parameters
  
  ## Mean
  mu = hill5_model(theta, x)
  
  expect_equal( mu, c(100, 85.638, 1.02e-5, 0), tolerance=1e-3 )
  
  ## Gradient
  G.f2djac = rbind(  c(0, 1, 0, 0, 0),
              c(0.144, 0.856, 26.35, 27.397, -13.277),
              c(1, 1.02e-7, 1.64e-4, -5.68e-5, -1.648e-4),
              c(1, 0, 0, 0, 0)
              )
  
  G = attr(hill5_model, "gradient")(theta, x)
  expect_equal( G, G.f2djac, tolerance=1e-3 )
  
  ## Starting values
  y = mu + c(-1, 1, 3, 0.5)
  theta0 = c(emin=0.5, emax=99, lec50=-0.831, m=1, sym=0)
  theta0.start = attr(hill5_model, "start")(x, y)
  
  expect_equal( theta0.start, theta0, tolerance=1e-3 )  

  ## Backsolve
  x0 = attr(hill5_model, "backsolve")(theta, 40)
  x1 = attr(hill5_model, "backsolve")(theta, 40, log=TRUE)
  y0 = hill5_model(theta, x0)
  y1 = hill5_model(theta, exp(x1))
  
  expect_equal( y0, 40, tolerance=1e-3 )
  expect_equal( y1, 40, tolerance=1e-3 )
  
})