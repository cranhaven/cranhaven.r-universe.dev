#' Range of a Vector (Not Min/Max!)
#' 
#' The base R function \code{\link[base]{range}} returns the minimum and maximum 
#' of a vector, but the "range" is actually defined as the difference between 
#' the minimum and maximum. This function calculates the actual range. It is 
#' equivalent to the base R code \code{diff(range(x))}, but a bit simpler and 
#' much faster.
#' 
#' 
#' @param x Integer or numeric vector.
#' @param integer Logical value for whether \code{x} is an integer vector.
#' 
#' 
#' @return Integer or numeric value.
#' 
#' 
#' @examples
#' # truerange vs. diff(range()) for integer vector
#' x <- rpois(1000, lambda = 5)
#' all.equal(diff(range(x)), truerange(x, TRUE))
#' benchmark(diff(range(x)), truerange(x, TRUE), replications = 2000)
#' 
#' # truerange vs. diff(range()) for numeric vector
#' x <- rnorm(1000)
#' all.equal(diff(range(x)), truerange(x))
#' benchmark(diff(range(x)), truerange(x), replications = 2000)
#' 
#' 
#' @export
truerange <- function(x, integer = FALSE) {
  if (integer) {
    return(.Call(`_dvmisc_truerange_i`, x))
  }
  return(.Call(`_dvmisc_truerange_n`, x))
}