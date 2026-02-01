context("netphenogeno")

test_that("npn in netphenogeno works fine ", 
  {
	 data(thaliana)
	 L.M1 <- netphenogeno(thaliana[ 1:5, c(1:5, 61:65)], method="npn", rho = 0.3)$loglik
	
	  expect_equal( ceiling(L.M1), -6 )  
   })


test_that("Gibbs in netphenogeno works fine ", 
  {
	  data(CviCol)
	  L.M2 <- netphenogeno(thaliana[ 1:5, c(1:5, 61:65)], method="gibbs", rho = 0.5, ncores = 1)$loglik
	
	  expect_equal( ceiling(L.M2) , -15 )  
   })