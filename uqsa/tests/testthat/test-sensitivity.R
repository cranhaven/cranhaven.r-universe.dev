test_that("sensitivity function works",{
	X <- c(rnorm(100,sd=1),7) # 7>3*sd to have at least a few NA values in binMeansY
	ny <- 2
	Y <- matrix(c(sin(X),cos(X)),ncol=ny)
	S <- gsa_binning(as.matrix(X),Y,nBins=30)
	expect_type(S,"double")
	expect_true(is.matrix(S))
	expect_equal(dim(S),c(2,1))
	expect_lt(max(S),1.0)
	expect_gt(min(S),0.0)
})

