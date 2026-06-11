#' Moving Correlations as Short Vector Slides Across Long Vector
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
#' sliding_cor(short, long)
#' 
#' 
#' @export
sliding_cor <- function(short, long) {
  .Call(`_dvmisc_sliding_cor_c`, short, long, sd(short))
}