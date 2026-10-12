plotSampledPrev <-
function(samples,obs_prev,exp_prev,maxN=NULL,...){

	if (is.null(maxN)){
		ul <- max(c(samples,obs_prev,exp_prev))
	} else {
		ul <- maxN
	}
	dat_temp <- table(samples)/length(samples)
	dat <- rep(0,ul+1)
	names(dat) <- 0:ul
	dat[names(dat_temp)] <- dat_temp
	barplot(as.vector(dat),names=names(dat),xlab="N. affected",ylab="Proportion",...)
	abline(v = 0.7 + 1.2*(obs_prev),col="red",lty=2,lwd=3)
	abline(v =  0.7 + 1.2*(exp_prev),col="green",lty=2,lwd=3)
	#abline(v = 0.7 + 1.2*(mean(samples)),col="blue",lty=2,lwd=3)
	#abline(v =  0.7 + 1.2*(exp_prev),col="green",lty=2,lwd=3)
}#### Members functions of MangroveTree objects ####

