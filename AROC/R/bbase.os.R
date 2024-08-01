bbase.os <-
function(x, K, bdeg = 3, eps = 1e-5, intercept = TRUE) {
	
	# Using the function bs
	B <- bs(x, degree = bdeg, df = K + bdeg, intercept = intercept)
	#class(B) <- c("bbase.os", "matrix")
	B
}
