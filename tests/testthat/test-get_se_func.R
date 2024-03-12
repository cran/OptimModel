test_that("get_se_func", {


  theta = c(-50, 100, 0.5, 2)
  V = rbind( c(3.1, 0.4, -0.7, -0.1),
             c(0.4, 2.3, 0.0, -1.7),
             c(-0.7, 0, 0.9, -0.2),
             c(-0.1, -1.7, -0.2, 2.3)
             ) 
  obj = list( coefficients=theta, varBeta=V, df=10 )

  f = function(theta){ theta[1] + (theta[3])^theta[4] }
  
  res = get_se_func(object=obj, Func=f, level=0.95)
  
  est = f(theta)
  f.grad = c(1, 0, theta[4]*theta[3]^(theta[4]-1), (theta[3])^theta[4]*log(theta[3]))
  se = as.vector(sqrt(f.grad%*%V%*%f.grad))
  lower = est - qt(0.975, 10)*se
  upper = est + qt(0.975, 10)*se
  
  expect_equal( res, data.frame(est=est, SE=se, lower=lower, upper=upper), tolerance=1e-3 )  
  
})