context("netmap")

test_that("npn map construction method works", 
          {
            data(CviCol)
            M1 <- netmap(CviCol, method="npn", cross= "inbred", rho = 0.4)
            #dput(M1$map$LG)
            
            expect_equal( max(M1$map$LG), 5 )  
          })


test_that("Gibbs map construction method works ", 
  {
	 data(CviCol)
	 D2 <- CviCol[ 1:20, c(1:5, 61:65)]
	 M2 <- netmap(D2, method="gibbs", cross= "inbred", rho = 0.3, ncores = 1)
	
	 expect_equal( max(M2$map$LG), 2)  
   })


test_that("Approx map construction method works ", 
  {
	 data(CviCol)
	 D3 <- CviCol[ 1:5, 1:5 ]
	 M3 <- netmap(D3, method="approx", cross= "inbred", rho = 0.6, ncores = 1)
	
	 expect_equal( max(M3$map$LG), 2 )  
   })