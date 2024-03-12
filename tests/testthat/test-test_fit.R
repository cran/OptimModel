test_that("test_fit", {

  Vgood = rbind( c(3.1, 0.4, -0.7, -0.1),
             c(0.4, 2.3, 0.0, -1.7),
             c(-0.7, 0, 0.9, -0.2),
             c(-0.1, -1.7, -0.2, 2.3)
             ) 
  Vbad = matrix(2, 4, 4)             
             
  res1 = test_fit(list(converge=0, hessian=Vgood))
  res2 = test_fit(list(converge=1, hessian=Vgood))
  res3 = test_fit(list(converge=0, hessian=Vbad))
  res4 = test_fit(list(converge=1, hessian=Vbad))
  
  
  expect_true( res1 )  
  expect_false( res2 )
  expect_false( res3 )
  expect_false( res4 )    

})