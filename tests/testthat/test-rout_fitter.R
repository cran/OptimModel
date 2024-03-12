test_that("rout_fitter", {

  ## Test on linear_model
  d = expand.grid(Group=factor(LETTERS[1:4]), Treatment=c("Control", "Treated"), replicates=1:3)
  X = model.matrix(~Group*Treatment, data=d)
  theta = c(100, -1, -4, 2, -5, -2, -1, 0)
  y = c(98.946, 94.83, 95.216, 99.339, 93.772, 90.088, 88.353, 102.349, 
          99.463, 97.348, 93.019, 106.735, 95.42, 92.821, 86.711, 92.016, 
          100.903, 100.351, 91.99, 104.674, 94.836, 95.413, 93.099, 97.888)

  fit1 = rout_fitter(NULL, linear_model, x=X, y=y)
  fit2 = rout_fitter(c(theta, lsig=log(3)), linear_model, x=X, y=y)

  expect_true(sum(fit1$outlier.adj)==0)
  expect_true(sum(fit2$outlier.adj)==0)
  
  ii = c(6, 9)
  y[ii] = linear_model(theta, X[ii,]) + 15

  fit3 = rout_fitter(NULL, linear_model, x=X, y=y, Q=0.05)
  expect_true(sum(fit3$outlier)==2)
  expect_true(sum(fit3$outlier.adj)==2)
  
  
  
  ## Test on hill_model
  x = c(0, 2^(-4:4))
  theta = c(0, 100, log(.5), 2)
  y = c(99.91, 96.892, 90.782, 79.24, 51.838, 18.849, 7.098, -1.697, 0.278, 1.136)

  fit1 = rout_fitter(NULL, hill_model, x=x, y=y)
  fit2 = rout_fitter(c( theta, log(2) ), hill_model, x=x, y=y)
  
  expect_true(sum(fit1$outlier.adj)==0)
  expect_true(sum(fit2$outlier.adj)==0)
  
  ii = c(6, 9)
  y[ii] = hill_model(theta, x[ii]) + 15

  fit3 = rout_fitter(c( theta, log(2) ), hill_model, x=x, y=y, Q=0.05)
  expect_true(sum(fit3$outlier)==2)
  expect_true(sum(fit3$outlier.adj)==0)

  fit3 = rout_fitter(c( theta, log(2) ), hill_model, x=x, y=y, Q=0.1)
  expect_true(sum(fit3$outlier)==2)
  expect_true(sum(fit3$outlier.adj)==2)  

  ## hill_quad_model
  theta = c(0, 100, 2, 1, -0.5)          ## Model parameters
  y = c(-3.204, -3.269, 4.488, 41.994, 69.658, 85.627, 90.797, 101.094, 83.925, 72.863)
  fit1 = rout_fitter(NULL, hill_quad_model, x=x, y=y, Q=0.01 )
  expect_true(sum(fit1$outlier.adj)==0)  

  ii = c(3, 7)
  y[ii] = hill_quad_model(theta, x[ii]) - 30
  fit2 = rout_fitter(NULL, hill_quad_model, x=x, y=y, Q=0.1 )
  expect_true(sum(fit2$outlier)==2)
  expect_true(sum(fit2$outlier.adj)==2) 

  
})