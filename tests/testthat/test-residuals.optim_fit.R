test_that("residuals.optim_fit", {


  ## With an without gradient
  h_model = function(theta, x){
    theta[1] + (theta[2] - theta[1])/(1 + exp(theta[4] * log(x) - 
        theta[4] * theta[3]))
  }
  
  get_call = function(f.model, x, y){
                      list(call=match.call())
                      }

  
  theta = c(10, 100, 0.5, 2)
  sigma = 4.1
  
  x = c(0, 2^(-4:4))  
  y = c(99.572, 99.9, 99.884, 96.141, 93.599, 76.127, 45.429, 23.676, 13.809, 12.65)
  
  mu = h_model(theta, x)
  
     ## Without gradient function
  obj1 = list( coefficients=theta, sigma=sigma, call=get_call(f.model=h_model, x=x, y=y)$call, fitted=mu, residuals=y-mu, fit.method="ols", x=x )
    ## With gradient function
  obj2 = list( coefficients=theta, sigma=sigma, call=get_call(f.model=hill_model, x=x, y=y)$call, fitted=mu, residuals=y-mu, fit.method="ols", x=x )

    ## With gradient function + MLE
  obj3 = list( coefficients=c(theta, lsig=log(sigma)), sigma=sigma, call=get_call(f.model=hill_model, x=x, y=y)$call, 
              fitted=mu, residuals=y-mu, fit.method="mle", x=x )

  
  class(obj1) = class(obj2) = class(obj3) = c("optim_fit", "list")
  
  X.mat = f2djac(h_model, theta, x=x)
  X.mat.true = attr(hill_model, "gradient")(theta, x)  
  
 
  ## Raw residuals 
  expect_equal( as.vector(residuals(obj1, type="raw")), y-mu, tolerance=1e-3 ) 
  expect_equal( as.vector(residuals(obj2)), y-mu, tolerance=1e-3 )   
  
## Studentized residuals
  xTx.inv = solve(t(X.mat)%*%X.mat) 
  Hat.mat = sqrt( 1 - apply( X.mat, 1, function(z){ t(z)%*%xTx.inv%*%z}) )  
  r1 = as.vector(residuals(obj1, type="raw")/(sigma*Hat.mat))

  expect_equal( as.vector(residuals(obj1, type="studentized")), r1, tolerance=1e-3 )   
   
  xTx.inv = solve(t(X.mat.true)%*%X.mat.true) 
  Hat.mat = sqrt( 1 - apply( X.mat.true, 1, function(z){ t(z)%*%xTx.inv%*%z}) )  
  r2 = as.vector(residuals(obj2, type="raw")/(sigma*Hat.mat))

  expect_equal( as.vector(residuals(obj2, type="studentized")), r2, tolerance=1e-3 )   

  expect_equal( as.vector(residuals(obj3, type="studentized")), r2, tolerance=1e-3 )     
   
})
