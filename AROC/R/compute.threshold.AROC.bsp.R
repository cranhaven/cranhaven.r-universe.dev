compute.threshold.AROC.bsp <-
function(object, newdata, FPF = 0.5) {
	if(class(object)[2] != "AROC.bsp") {
		stop(paste0("This function can not be used for this object class: ", class(object)[2]))
	}
	Xp <- predict.design.matrix.bsp(object$fit$mm, newdata)$X
	
	ncov <- nrow(Xp)
	nrep <- length(object$fit$sd)
	np <- length(FPF)

	thresholds <- array(0,c(np,ncov,nrep))
	for(inrep in 1:nrep) {
		for(incov in 1:ncov) {
			thresholds[,incov,inrep] <- qnorm(1-FPF, mean = Xp[incov,]%*%object$fit$beta[inrep,], sd = object$fit$sd[inrep])
		}
	} 

	thresholdsm <- thresholdsl <- thresholdsh <- matrix(0, nrow = np, ncol = ncov)
	rownames(thresholdsm) <- rownames(thresholdsl) <- rownames(thresholdsh) <- FPF
	
	thresholdsm <- apply(thresholds, c(1,2), mean)
	thresholdsl <- apply(thresholds, c(1,2), quantile, 0.025)
	thresholdsh <- apply(thresholds, c(1,2), quantile, 0.975)

	res <- list()
	res$thresholds.est <- thresholdsm
	res$thresholds.ql <- thresholdsl
	res$thresholds.qh <- thresholdsh
	res
}
