context("netsnp")

test_that("npn in netsnp works fine ", 
  {
	 data(CviCol)
	 L.M1 <- netsnp(CviCol[ 1:20, c(1:5, 61:65)], method="npn", rho = 0.3)$loglik
	
	  expect_equal( L.M1, -57.50442  )  
   })


test_that("Gibbs in netsnp works fine ", 
  {
	  data(CviCol)
	  L.M2 <- netsnp(CviCol[ 1:5, c(1:5, 61:65)], method="gibbs", rho = 0.8, ncores = 1)$loglik
	
	  expect_equal( L.M2,  -25  )  
   })


test_that("Approx in netsnp works fine ", 
  {
	data(CviCol)
	L.M3 <- netsnp(CviCol[ 1:5, c(1:5, 61:65)], method="approx", rho = 0.8, ncores = 1)$loglik
	
	 expect_equal( L.M3, -25 )  
   })
