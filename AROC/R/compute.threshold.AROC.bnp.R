compute.threshold.AROC.bnp <-
function(object, newdata, FPF = 0.5) {
	#qF0 <- function(q, iter, Xpred, icov, object){
	#	toInvert <-  function(x, q, iter, Xpred, icov, object){
	#		return(sum(object$fit$probs[iter,]*pnorm(x, mean = Xpred[icov,]%*%t(object$fit$beta[iter,,]), sd = object$fit$sd[iter,])) - q)
	#	}
	#	res <-  uniroot(toInvert, interval = c(-10^15,10^15), q, iter, Xpred, icov, object)$root
	#	return(res)
	#}
	
	if(class(object)[2] != "AROC.bnp") {
		stop(paste0("This function can not be used for this object class: ", class(object)[2]))
	}
	# Create the data frame
	Xp <- predict.design.matrix.bnp(object$fit$mm, newdata)$X

	ncov <- nrow(Xp)
	nrep <- nrow(object$fit$probs)
	np <- length(FPF)

	#thresholds <- array(0,c(np,ncov,nrep))
	#for(inp in 1:np) {
	#	for(inrep in 1:nrep) {
	#		for(incov in 1:ncov) {
	#			thresholds[inp,incov,inrep] <- qF0(q = 1-FPF[inp], iter = inrep, Xpred = Xp, icov = incov, object = object)  
	#		}
	#	}
	#}
	thresholds <- array(0,c(np,ncov,nrep))
	for(inrep in 1:nrep) {
        mu.h <- Xp%*%t(object$fit$beta[inrep,,])
        for(incov in 1:ncov) {
            aux <- norMix(mu = c(mu.h[incov,]), sigma = object$fit$sd[inrep,], w = object$fit$probs[inrep,])
            thresholds[,incov,inrep] <- qnorMix(1-FPF, aux)
        }
    }

	thresholdsm <- thresholdsl <- thresholdsh <- matrix(0, nrow = np, ncol = ncov)
	rownames(thresholdsm) <- rownames(thresholdsl) <- rownames(thresholdsh) <- FPF
	for(incov in 1:ncov){
		for(inp in 1:np){  
			thresholdsm[inp,incov] <- mean(thresholds[inp,incov,])       
			thresholdsl[inp,incov] <- quantile(thresholds[inp,incov,],0.025)
			thresholdsh[inp,incov] <- quantile(thresholds[inp,incov,],0.975)
		}
	}
	res <- list()
	res$thresholds.est <- thresholdsm
	res$thresholds.ql <- thresholdsl
	res$thresholds.qh <- thresholdsh
	res
}
