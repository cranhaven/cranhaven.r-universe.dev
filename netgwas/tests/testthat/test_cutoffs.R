
context("cutoffs")

test_that("cut-points of ordinal variables works", 
  {
	d <- structure(c(2,0, 1, 1, 1, 0, 1, 1, 0, 2, 0, 2, 2, 1, 2, 2, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 2, 0, 2, 2), .Dim = c(5L, 6L))
    res1 <- cutoffs(d)   
	
	  expect_equal( res1, structure(c(-Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -0.841621233572914, 
-0.2533471031358, -0.841621233572914, -0.2533471031358, -0.2533471031358, 
-0.2533471031358, 0.841621233572914, 0.841621233572914, -0.2533471031358, 
0.841621233572914, Inf, -0.2533471031358, Inf, Inf, Inf, Inf, 
Inf, Inf), .Dim = c(6L, 4L))  )  
   })
