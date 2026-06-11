#' Return Index of (First) Minimum of a Vector
#' 
#' Returns index of minimum for vectors and index or (row, column) position for 
#' matrices. For optimal speed, use \code{integer = TRUE} if \code{x} is an 
#' integer vector/matrix and \code{integer = FALSE} otherwise. Typically faster 
#' than \code{\link[base]{which.min}} for matrices and for large vectors.
#' 
#' 
#' @param x Integer or numeric vector/matrix.
#' @param arr.ind Logical value for whether to return (row, col) position rather 
#' than vector position, if \code{x} is a matrix.
#' @param integer Logical value for whether \code{x} is an integer vector/matrix.
#' 
#' 
#' @return Numeric value.
#' 
#' 
#' @examples
#' # which.min2 vs. which.min for integer vector
#' x <- rpois(10000, lambda = 10)
#' all.equal(which.min(x), which.min2(x, integer = TRUE))
#' benchmark(which.min(x), which.min2(x, integer = TRUE), replications = 10000)
#' 
#' # which.min2 vs. which.min for numeric vector
#' x <- rnorm(10000)
#' all.equal(which.min(x), which.min2(x))
#' benchmark(which.min(x), which.min2(x), replications = 10000)
#' 
#' 
#' @export
which.min2 <- function(x, arr.ind = FALSE, integer = FALSE) {
  if (! arr.ind) {
    if (integer) {
      return(.Call(`_dvmisc_which_min_iv`, x))
    }
    return(.Call(`_dvmisc_which_min_nv`, x))
  }
  if (integer) {
    return(.Call(`_dvmisc_which_min_im`, x))
  }
  return(.Call(`_dvmisc_which_min_nm`, x))
}