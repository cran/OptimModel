test_that("f2djac", {


  f = function(theta, x){ theta[1] + (theta[2]-theta[1])/(1 + (x/theta[3])^theta[4]) }
  theta = c(-50, 100, 0.5, 2)
  x = c(0, 4, 8, 10)
  f.grad = f2djac(f, theta, x=x)
  
  f.grad.true = cbind(1 - 1/(1 + (x/theta[3])^theta[4]),
                      1/(1 + (x/theta[3])^theta[4]),
                      (theta[2]-theta[1])*theta[4]*x^theta[4]/(theta[3]^(theta[4]+1) *(1 + (x/theta[3])^theta[4])^2),
                      ifelse(x==0, 0, -(theta[2]-theta[1])*(x/theta[3])^theta[4]*log(x/theta[3])/(1 + (x/theta[3])^theta[4])^2)
                      )                      
  
  expect_equal( f.grad, f.grad.true, tolerance=1e-3 )  
  
})