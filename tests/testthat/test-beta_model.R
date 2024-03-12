test_that("beta_model", {

  x = c(0, 2^c(-4, 0, 4))
  theta = c(0, 115, -1.5, 9, 11.5)         ## Model parameters
  attr(theta, "maxX") = max(x)
  
  ## Mean
  mu = beta_model(theta, x)
  
  expect_equal( mu, c(0, 61.628, 105.935, 57.414), tolerance=1e-3 )
  
  ## Gradient
  G.f2djac = rbind(  c(1, 0, 0, 0, 0),
              c(1, 0.536, -51.895, 13.432, -13.429),
              c(1, 0.921, -23.637, 14.936, -14.933),
              c(1, 0.499, 22.725, -62.592, 62.599)
              )
  
  G = attr(beta_model, "gradient")(theta, x)
  expect_equal( G, G.f2djac, tolerance=1e-3 )
  
  ## Starting values
  y = mu + c(-1, 1, 3, 0.5)
  theta0 = c(emin=-1, emax=109.935, ldelta1=0, ldelta2=0, ldelta3=0)
  attr(theta0, "maxX") = max(x)
  theta0.start = attr(beta_model, "start")(x, y)
  
  expect_equal( theta0.start, theta0, tolerance=1e-3 )  

  ## Backsolve
  x0 = attr(beta_model, "backsolve")(theta, 40)
  x1 = attr(beta_model, "backsolve")(theta, 40, log=TRUE)
  y0 = beta_model(theta, x0)
  y1 = beta_model(theta, exp(x1))
  
  expect_equal( y0, 40, tolerance=0.01 )
  expect_equal( y1, 40, tolerance=0.01 )
  
})