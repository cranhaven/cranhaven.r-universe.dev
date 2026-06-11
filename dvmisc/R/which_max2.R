#' Return Index of (First) Maximum of a Vector
#' 
#' Returns index of maximum for vectors and index or (row, column) position for 
#' matrices. For optimal speed, use \code{integer = TRUE} if \code{x} is an 
#' integer vector/matrix and \code{integer = FALSE} otherwise. Typically faster 
#' than \code{\link[base:which.min]{which.max}} for matrices and for large 
#' vectors.
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
#' # which.max2 vs. which.max for integer vector
#' x <- rpois(10000, lambda = 5)
#' all.equal(which.max(x), which.max2(x, integer = TRUE))
#' benchmark(which.max(x), which.max2(x, integer = TRUE), replications = 10000)
#' 
#' # which.max2 vs. which.max for numeric vector
#' x <- rnorm(10000)
#' all.equal(which.max(x), which.max2(x))
#' benchmark(which.max(x), which.max2(x), replications = 10000)
#' 
#' 
#' @export
which.max2 <- function(x, arr.ind = FALSE, integer = FALSE) {
  if (! arr.ind) {
    if (integer) {
      return(.Call(`_dvmisc_which_max_iv`, x))
    }
    return(.Call(`_dvmisc_which_max_nv`, x))
  }
  if (integer) {
    return(.Call(`_dvmisc_which_max_im`, x))
  }
  return(.Call(`_dvmisc_which_max_nm`, x))
}