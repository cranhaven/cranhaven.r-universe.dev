compute.threshold.AROC.sp <- 
function(object, newdata, FPF = 0.5) {
	if(class(object)[2] != "AROC.sp") {
		stop(paste0("This function can not be used for this object class: ", class(object)[2]))
	}
	ncov <- nrow(newdata)
	np <- length(FPF)

	thresholds <- matrix(0, nrow = np, ncol = ncov)
	rownames(thresholds) <- FPF
	#colnames(thresholds) <- newcovariate

	fit.new <- predict(object$fit.h, newdata = newdata)

	if(object$est.surv.h == "normal") {
		csf0_inv <- qnorm(1-FPF)
	} else {
		h.residuals <- object$fit.h$residuals/summary(object$fit.h)$sigma
		csf0 <- apply(outer(h.residuals, h.residuals, ">="), 2, mean)
		csf0_inv <- apply(outer(csf0, FPF, "<="), 2, function(x, z) {
			res <- min(c(z[x], max(z)))
			res
		}, z = h.residuals)
		csf0_inv <- replace(csf0_inv, is.infinite(csf0_inv), max(h.residuals))
	}
	for(i in 1:ncov) {
		thresholds[,i] <- fit.new[i] + summary(object$fit.h)$sigma*csf0_inv
	}
	res <- list()
	res$thresholds <- thresholds
	res
}
