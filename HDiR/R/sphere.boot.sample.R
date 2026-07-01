#' sphere.boot.sample
#'
#' Generates bootstrap samples from a spherical kernel estimator
#'
#' @param sample Ramdon sample of points on the unit sphere
#' @param n Size of bootstrap samples
#' @param bw Smoothing parameter of the kernel estimator
sphere.boot.sample<-function(sample,n,bw){  #bw0 bandwidth
		boot.sample<-rmovMF(n, rep(1/bw^2,n)*(sample/sqrt(rowSums(sample^2))),alpha=rep(1/n,n))
		return(boot.sample)
}
#' @noRd
#' @keywords internal
