compute.threshold.pooledROC.emp <- function(object, FPF = 0.5) {
	if(class(object)[2] != "pooledROC.emp") {
		stop(paste0("This function can not be used for this object class: ", class(object)[2]))
	}
	
	F1emp <- ecdf(object$marker$d)
	thresholds <- quantile(object$marker$h, 1 - FPF, type = 1)
	TPF <- 1 - F1emp(thresholds)
	
	res <- list()
	res$thresholds <- thresholds
	res$FPF <- FPF
	res$TPF <- TPF
	res

}
