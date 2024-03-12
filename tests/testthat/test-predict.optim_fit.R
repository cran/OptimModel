test_that("predict.optim_fit", {


  ## With an without gradient
  f1 = f2 = function(theta, x){ theta[1] + (theta[2]-theta[1])/(1 + (x/theta[3])^theta[4]) }
  theta = c(-50, 100, 0.5, 2)
  sigma = 4.1
  phi = 0.1  ## Variance parameter
  
  V = rbind( c(3.1, 0.4, -0.7, -0.1),
             c(0.4, 2.3, 0.0, -1.7),
             c(-0.7, 0, 0.9, -0.2),
             c(-0.1, -1.7, -0.2, 2.3)
             )   
  
  x = c(0, 4, 8, 10)  
  
  f.grad = f2djac(f1, theta, x=x)
  attr(f2, "gradient") = function(theta, x){
                cbind(1 - 1/(1 + (x/theta[3])^theta[4]),
                      1/(1 + (x/theta[3])^theta[4]),
                      (theta[2]-theta[1])*theta[4]*x^theta[4]/(theta[3]^(theta[4]+1) *(1 + (x/theta[3])^theta[4])^2),
                      ifelse(x==0, 0, -(theta[2]-theta[1])*(x/theta[3])^theta[4]*log(x/theta[3])/(1 + (x/theta[3])^theta[4])^2)
                      )                      
                }
  
  
  mu = f1(theta, x)
  
  sig.with.varpower = sigma*weights_varPower(phi, mu)

  obj1 = list( coefficients=theta, sigma=sigma, call=list(f.model=f1), fitted=mu, varBeta=V, df=10 )  ## Without gradient function
  attr(obj1, "w.func") = weights_varIdent
  obj2 = list( coefficients=theta, sigma=sigma, call=list(f.model=f2), fitted=mu, varBeta=V, df=10 )  ## With gradient function
  attr(obj2, "w.func") = weights_varPower
  attr(obj2, "var.param") = phi
  
  class(obj1) = class(obj2) = c("optim_fit", "list")

    ## Default returns fitted(obj)
  expect_equal( predict(obj1), mu, tolerance=1e-3 ) 
   
    ## Ask for SE fit
  pred2a = try(predict(obj1, se.fit=TRUE), silent=TRUE)   ## Fails
  expect_true( class(pred2a)[1]=="try-error" )
  
  pred2b = predict(obj1, x=x, se.fit=TRUE)  ## Without gradient
  se1 = sqrt(apply(f.grad, 1, function(g){ g%*%V%*%g })) 
  expect_equal( pred2b, cbind(x=x, y.hat=mu, se.fit=se1), tolerance=1e-3 ) 

  pred2b = predict(obj2, x=x, se.fit=TRUE)  ## With gradient
  se2 = sqrt(apply(attr(f2, "gradient")(theta, x), 1, function(g){ g%*%V%*%g })) 
  expect_equal( pred2b, cbind(x=x, y.hat=mu, se.fit=se2), tolerance=1e-3 ) 


    ## Ask for confidence interval  
  pred3 = predict(obj1, x=x, se.fit=TRUE, interval="confidence", level=0.95)  
  expect_equal( pred3, cbind(x=x, y.hat=mu, se.fit=se1, lower=mu-qt(0.975, obj1$df)*se1, upper=mu+qt(0.975, obj1$df)*se1), tolerance=1e-3 )   

    ## With weights_varPower()
  pred3 = predict(obj2, x=x, se.fit=TRUE, interval="confidence", level=0.95)  
  expect_equal( pred3, cbind(x=x, y.hat=mu, se.fit=se2, lower=mu-qt(0.975, obj1$df)*se2, upper=mu+qt(0.975, obj1$df)*se2), tolerance=1e-3 )   
    

    ## Ask for prediction interval  of next mean with K=2
  pred4 = predict(obj1, x=x, se.fit=TRUE, interval="prediction", K=2, level=0.95)  
  expect_equal( pred4, cbind(x=x, y.hat=mu, se.fit=se1, 
                             lower=mu-qt(0.975, obj2$df)*sqrt(sigma^2/2+se1^2), 
                             upper=mu+qt(0.975, obj2$df)*sqrt(sigma^2/2 + se1^2)), tolerance=1e-3 )   
    
    
  pred4 = predict(obj2, x=x, se.fit=TRUE, interval="prediction", K=2, level=0.95)  
  expect_equal( pred4, cbind(x=x, y.hat=mu, se.fit=se2, 
                             lower=mu-qt(0.975, obj2$df)*sqrt(sig.with.varpower^2/2+se2^2), 
                             upper=mu+qt(0.975, obj2$df)*sqrt(sig.with.varpower^2/2 + se2^2)), tolerance=1e-3 )   


  
})