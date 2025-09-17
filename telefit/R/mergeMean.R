#' Combine sample means from two samples
#'
#' This function combines the sample mean information from two samples (of 
#' the same phenomena) to return the sample mean of the union of the two 
#' samples.
#' 
#' @param x.mean sample mean from the first sample, 'x'
#' @param y.mean sample mean from the second sample, 'y'
#' @param x.n sample size from the first sample, 'x'
#' @param y.n sample size from the second sample, 'y'
#'
#' 

mergeMean = function(x.mean, y.mean, x.n, y.n) {
  
  N = x.n + y.n
  
  (x.mean*x.n + y.mean*y.n)/N
}