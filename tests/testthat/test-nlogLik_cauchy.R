test_that("nlogLik_cauchy", {


  x = c(0, 2^c(-4, 0, 4))
  theta = c(emin=0, emax=100, lec50=log(.5), m=2)
  y = c(104., 101, 18, 0.7)

  theta1 = c(theta, lsigma=log(2))
  h_model = function(theta, x){
                theta[1] + (theta[2] - theta[1])/(1 + exp(theta[4] * log(x) - theta[4] * theta[3]))
                }

  res = nlogLik_cauchy(theta1, x=x, y=y, f.model=h_model, lbs=FALSE)
  
  res.true = -sum( dcauchy(y, location=h_model(theta, x), scale=exp(theta1[5]), log=TRUE) )
  
  expect_equal( res, res.true, tolerance=1e-3 )  

  res = nlogLik_cauchy(theta1, x=x, y=log(y), f.model=h_model, lbs=TRUE)
  
  res.true = -sum( dcauchy(log(y), location=log(h_model(theta, x)), scale=exp(theta1[5]), log=TRUE) )

  expect_equal( res, res.true, tolerance=1e-3 )    
})