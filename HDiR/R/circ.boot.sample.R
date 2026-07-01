#' circ.boot.sample
#'
#' Generates bootstrap samples from a circular kernel estimator
#'
#' @param sample Ramdon sample of points on the unit circle
#' @param n Size of bootstrap samples
#' @param bw Smoothing parameter of the kernel estimator
circ.boot.sample<-function(sample,n,bw){  #bw0 bandwidth
		boot.sample<-rcircmix(n,dist=rep("vm",n),
                     param=list(p=rep(1/n,n),
                     mu=sample,
                     kappa=rep(bw,n)))
		return(boot.sample)
}
#' @noRd
#' @keywords internal

