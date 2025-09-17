#' Combine sample variances from two samples
#'
#' This function combines the sample variance information from two samples (of 
#' the same phenomena) to return the sample variance of the union of the two 
#' samples.
#' 
#' This function assumes the data is normalized by n (the MLE estimator) 
#' instead of n-1 (the unbiased estimator).
#'
#' @param x.var sample variance from the first sample, 'x'
#' @param y.var sample variance from the second sample, 'y'
#' @param x.mean sample mean from the first sample, 'x'
#' @param y.mean sample mean from the second sample, 'y'
#' @param x.n sample size from the first sample, 'x'
#' @param y.n sample size from the second sample, 'y'
#'
#' @references Chan, T.F., Golub, G.H., and LeVeque, R.J., 1979, Updating formulae and a pairwise algorithm for computing sample variances: Technical Report, Stanford University .
#' 

mergeVar = function(x.var, y.var, x.mean, y.mean, x.n, y.n) {
  N = x.n + y.n
  ( x.var*x.n + y.var*y.n + (x.mean - y.mean)^2*x.n*y.n/N ) / N
}