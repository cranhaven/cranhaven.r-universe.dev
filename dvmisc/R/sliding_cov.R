#' Moving Covariance as Short Vector Slides Across Long Vector
#' 
#' Uses C++ code for efficiency.
#' 
#' 
#' @param short Numeric vector.
#' @param long Numeric vector.
#' 
#' 
#' @return
#' Numeric vector.
#' 
#' 
#' @examples
#' short <- rnorm(4)
#' long <- rnorm(10)
#' sliding_cov(short, long)
#' 
#' 
#' @export
sliding_cov <- function(short, long) {
  return(.Call(`_dvmisc_sliding_cov_c`, short, long))
}