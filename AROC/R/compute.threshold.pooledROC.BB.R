compute.threshold.pooledROC.BB <- function(object, FPF = 0.5) {
	if(class(object)[2] != "pooledROC.BB") {
		stop(paste0("This function can not be used for this object class: ", class(object)[2]))
	}

	B <- ncol(object$weights$h)

	weights.h <- object$weights$h
	weights.d <- object$weights$d

	np <- length(FPF)
	thresholds.s <- TPF.s <- matrix(0, nrow = np, ncol = B)

	for(l in 1:B) {
		thresholds.s[,l] <- quantile(ewcdf(object$marker$h, weights.h[,l]), 1- FPF, type = 1)
		TPF.s[,l] <- 1 - ewcdf(object$marker$d, weights.d[,l])(thresholds.s[,l])
	}

	thresholds <- matrix(0, ncol = 3, nrow = np, dimnames = list(1:np, c("est","ql", "qh")))
	rownames(thresholds) <- FPF

	thresholds[,1] <- apply(thresholds.s, 1, mean)
	thresholds[,2] <- apply(thresholds.s, 1, quantile, prob = 0.025)
	thresholds[,3] <- apply(thresholds.s, 1, quantile, prob = 0.975)

	TPF <- matrix(0, ncol = 3, nrow = np, dimnames = list(1:np, c("est","ql", "qh")))
	rownames(TPF) <- FPF

	TPF[,1] <- apply(TPF.s, 1, mean)
	TPF[,2] <- apply(TPF.s, 1, quantile, prob = 0.025)
	TPF[,3] <- apply(TPF.s, 1, quantile, prob = 0.975)

	res <- list()
	res$thresholds <- thresholds
	res$FPF <- FPF
	res$TPF <- TPF
	res	
}