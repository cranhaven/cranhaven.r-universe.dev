predict.design.matrix.bsp <-
function(object, newdata, ...) {
	mfp <- model.frame(object$mt, newdata, xlev = attr(object$mt, "xlev"))
	Xp <- model.matrix(object$mt, data = mfp, contrasts.arg = attr(object$mt, "contrast")) # Includes the intercept
	res <- list()
	res$X <- Xp
	res
}
