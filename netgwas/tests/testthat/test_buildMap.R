context("buildMap")

test_that("buildMap construction method works ", 
  {
    data(CviCol)
    M1 <- netmap(CviCol, method="npn", cross= "inbred")
    bm <- buildMap(M1, opt.index=5)	 
    #dput(bm$map$markers)
	
    expect_equal( max(bm$map$LG), 5 )
   })
