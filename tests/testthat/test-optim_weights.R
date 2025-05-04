test_that("optim_weights", {

  mu = c(0, 25, 80, 125)
  phi = 1.5
  phi2 = c(0.25, phi)
  
  expect_equal( weights_varIdent(phi, mu), rep(1, length(mu)), tolerance=1e-3 )  
  expect_equal( weights_varExp(phi, mu), exp(phi*mu), tolerance=1e-3 )
  expect_equal( weights_varPower(phi, mu), abs(mu)^phi, tolerance=1e-3 )
  expect_equal( weights_varConstPower(phi2, mu), phi2[1] + abs(mu)^phi2[2], tolerance=1e-3 )  
  
  resid = c(3, -2, -1.5, 4.1, 90)
  sig = mad(resid, center=0)
  r = abs(resid)/sig
  tbw = c( (1-(r[1:4]/4.685)^2)^2, 0 )

  expect_equal( weights_tukey_bw(phi=4.685, resid), tbw, tolerance=1e-3 )  

  hw = c(rep(1, 4), 1.345/r[5])
  
  expect_equal( weights_huber(phi=1.345, resid), hw, tolerance=1e-3 )   

})