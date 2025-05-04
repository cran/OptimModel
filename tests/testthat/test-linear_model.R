test_that("linear_model", {


  d = data.frame( Group=factor(c("A", "A", "B", "B", "C", "C")), age=c(19, 17, 18, 18, 15, 22) )
  X = model.matrix(~Group+age, data=d)  ## This is the "x" in linear.model()
  theta = c(80, 20, 40, -3) 

  mu0 = as.vector(X%*%theta)
  
  ## Mean
  mu = linear_model(theta, X) 
  
  expect_equal( mu, mu0, tolerance=1e-3 )
  
  ## Gradient
  G0 = X
  
  G = attr(linear_model, "gradient")(theta, X)
  expect_equal( G, G0, tolerance=1e-3 )
  
  ## Starting values
  y = mu + c(-1, 1, 3, 0.5, -1.3, 0.8)
  theta0 = qr.coef(qr(X), y)
  theta0.start = attr(linear_model, "start")(X, y)
  
  expect_equal( theta0.start, theta0, tolerance=1e-3 )  
  
})