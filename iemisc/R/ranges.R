#' Sample range
#'
#' This function computes the sample range.
#'
#' "The range is the difference between the largest number and the smallest
#' number in the set." Source: Onwubiko page 176.
#'
#' The following statements are from \code{\link[base]{range}}:
#'
#' "If na.rm is \code{FALSE}, \code{NA} and \code{NaN} values in any of the
#' arguments will cause \code{NA} values to be returned, otherwise \code{NA}
#' values are ignored."
#'
#' "If finite is \code{TRUE}, the minimum and maximum of all finite values is
#' computed, i.e., \code{finite = TRUE} includes \code{na.rm = TRUE}."
#'
#'
#'
#' @param x any numeric vector
#' @param na.rm logical vector that determines whether the missing values
#'    should be removed or not. The default is FALSE.
#' @param finite logical vector that determines whether non-finite values
#'    should be removed or not. The default is FALSE.
#'
#' @return ranges as the difference between the maximum and minimum values in \code{x}
#'    as a numeric \code{\link[base]{vector}}. Unlike the \code{\link[base]{range}}, ranges can't
#'    take character vectors as arguments, only numeric vectors.
#'
#'
#' @references
#' Chinyere Onwubiko, \emph{An Introduction to Engineering}, Mission, Kansas: Schroff Development Corporation, 1997, page 176.
#'
#'
#' @source
#' \enumerate{
#'    \item r - How to not run an example using roxygen2? - Stack Overflow answered and edited by samkart on Jul 9 2017. (Also see the additional comments in response to the answer.) See \url{https://stackoverflow.com/questions/12038160/how-to-not-run-an-example-using-roxygen2}.
#'    \item devtools - Issues in R package after CRAN asked to replace dontrun by donttest - Stack Overflow answered by Hong Ooi on Sep 1 2020. (Also see the additional comments in response to the answer.) See \url{https://stackoverflow.com/questions/63693563/issues-in-r-package-after-cran-asked-to-replace-dontrun-by-donttest}.
#' }
#'
#'
#'
#' @author Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#'
#'
#' @seealso \code{\link{sgm}} for geometric mean, \code{\link{shm}} for harmonic mean, \code{\link{cv}} for
#'  coefficient of variation (CV), \code{\link{rms}} for root-mean-square (RMS), \code{\link{relerror}}
#'  for relative error, and \code{\link{approxerror}} for approximate error.
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' @examples
#' 
#' # Example 1
#' 
#' install.load::load_package("iemisc", "rando")
#' 
#' set_n(100) # makes the example reproducible
#' 
#' x <- r_norm(.seed = 943)
#' 
#' ranges(x)
#'
#'
#' install.load::load_package("iemisc", "rando")
#'
#' set_n(100) # makes the example reproducible
#'
#' (r.x <- ranges(r_norm(.seed = 100))); r.x
#'
#'
#' \donttest{
#' # See Source 1 and Source 2
#' 
#' # Example 2 (from the base range function)
#'
#' library(iemisc)
#'
#' xi <- c(NA, 1:3, -1:1/0); xi
#' 
#' try(ranges(xi))
#' 
#' try(ranges(xi, na.rm = TRUE))
#' 
#' try(ranges(xi, finite = TRUE))
#' }
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#'
#' @export
ranges <- function (x, na.rm = FALSE, finite = FALSE) {

# checks
assert_that(!any(qtest(x, "N+(,)") == FALSE), msg = "Either x is NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(qtest(na.rm, "B==1"), msg = "There is not a logical value for na.rm or more than 1 logical value for na.rm.")
# only process with enough known variables and provide an error message if the check fails

assert_that(qtest(finite, "B==1"), msg = "There is not a logical value for finite or more than 1 logical value for finite.")
# only process with enough known variables and provide an error message if the check fails


# The moments::kurtosis code has been helpful with regards to the treatment of na.rm

newx <- range(x, na.rm = na.rm, finite = finite)
# R's range (minimum and maximum value)

ranges <- diff(newx)
# sample range

return(ranges)
}
