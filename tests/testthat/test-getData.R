test_that("test-getData", {

  x = c(0, 2^(-4:4))
  theta = c(0, 100, log(.5), 2)
  y = c(100.045, 99.204, 92.527, 80.899, 49.32, 20.83, 4.819, 2.522, -1.677, -0.884)
  fit = optim_fit(c(0, 100, .5, 1), f.model=hill_model, x=x, y=y)

  d=getData(fit)
  
  expect_equal( d, list(x=x, y=y) )
  
  
})