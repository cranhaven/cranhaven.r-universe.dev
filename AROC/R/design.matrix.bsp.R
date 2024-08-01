design.matrix.bsp <-
function(formula, data) {
	mf <- model.frame(formula, data, drop.unused.levels = TRUE)
	mt <- terms(mf)   
	X <- model.matrix(mt, mf) # Includes the intercept
	res <- list(X = X, mf = mf, mt = mt)
	class(res) <- "design.matrix.bsp"
	res

}
