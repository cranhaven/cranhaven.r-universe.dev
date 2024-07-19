#' Coefficient of variation (CV)
#'
#' This function computes the sample coefficient of variation (CV).
#'
#' CV is expressed as
#'
#' \deqn{\frac{s}{\bar{x}} \cdot 100}
#'
#' \describe{
#'	\item{\emph{s}}{the sample standard deviation}
#'	\item{\emph{\eqn{\bar{x}}}}{the sample arithmetic mean}
#' }
#'
#'
#' @param x numeric vector, matrix, data.frame, or data.table that contains the
#'   sample data points.
#' @param na.rm logical vector that determines whether the missing values
#'   should be removed or not.
#'
#' @return coefficient of variation (CV), as a percent (\%), as an R object: a numeric
#'   \code{\link[base]{vector}} or a named numeric vector if using a named object (\code{\link[base]{matrix}},
#'   \code{\link[base]{data.frame}}, or \code{\link[data.table]{data.table}}). The default choice is that any NA values
#'   will be kept (\code{na.rm = FALSE}). This can be changed by specifying \code{na.rm = TRUE},
#'   such as \code{cv(x, na.rm = TRUE)}.
#'
#'
#'
#'
#' @source
#' \enumerate{
#'    \item r - How to not run an example using roxygen2? - Stack Overflow answered and edited by samkart on Jul 9 2017. (Also see the additional comments in response to the answer.) See \url{https://stackoverflow.com/questions/12038160/how-to-not-run-an-example-using-roxygen2}.
#'    \item devtools - Issues in R package after CRAN asked to replace dontrun by donttest - Stack Overflow answered by Hong Ooi on Sep 1 2020. (Also see the additional comments in response to the answer.) See \url{https://stackoverflow.com/questions/63693563/issues-in-r-package-after-cran-asked-to-replace-dontrun-by-donttest}.
#' }
#'
#'
#' @references
#' \enumerate{
#'    \item Masoud Olia, Ph.D., P.E. and Contributing Authors, \emph{Barron's FE (Fundamentals of Engineering Exam)}, 3rd Edition, Hauppauge, New York: Barron's Educational Series, Inc., 2015, page 84.
#'    \item Irwin R. Miller, John E. Freund, and Richard Johnson, \emph{Probability and Statistics for Engineers}, Fourth Edition, Englewood Cliffs, New Jersey: Prentice-Hall, Inc., 1990, page 25, 38.
#' }
#'
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
#' @seealso \code{\link{sgm}} for geometric mean, \code{\link{shm}} for harmonic mean, \code{\link{rms}}
#'  for root-mean-square (RMS), \code{\link{relerror}} for relative error, \code{\link{approxerror}} for
#'  approximate error, and \code{\link{ranges}} for sample range.
#'
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
#' # Example 2.60 from Miller (page 38)
#'
#' library(iemisc)
#'
#' x <- c(14, 12, 21, 28, 30, 63, 29, 63, 55, 19, 20)
#' # suspended solids in parts per million (ppm)
#' 
#' cv(x)
#'
#'
#' # using a matrix of the numeric vector x
#' mat1 <- matrix(data = x, nrow = length(x), ncol = 1, byrow = FALSE,
#'         dimnames = list(c(rep("", length(x))), "Samples"))
#' cv(mat1)
#'
#'
#' # using a data.frame of the numeric vector x
#' df <- data.frame(x)
#' cv(df)
#'
#'
#' # using a data.table of the numeric vector x
#' 
#' library("data.table")
#' 
#' dt <- data.table(x)
#' cv(dt)
#'
#'
#'
#' # modified Example 2.60 from Miller (page 38)
#' xx <- c(14, 12, 21, 28, 30, 63, 29, 63, 55, 19, 20, NA)
#' # suspended solids in parts per million (ppm)
#' 
#' cv(xx) # na.rm = FALSE is the default
#' cv(xx, na.rm = TRUE)
#'
#'
#'
#'
#' \donttest{
#' # See Source 1 and Source 2
#'
#' # Example 3
#'
#' # Please see the error messages
#' 
#' library(iemisc)
#'
#' try(cv(0))
#' try(cv(1))
#' try(cv(1003.23))
#'
#' }
#'
#'
#'
#'
#' # Example 4 - from the archived cvcqv README
#' 
#' xu <- c(0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4, 4.6, 5.4,
#' 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9)
#'
#' results2 <- cv(xu)
#' results2
#'
#'
#'
#'
#' @importFrom stats sd
#' @importFrom checkmate qtest test_data_frame
#' @importFrom assertthat assert_that
#' @importFrom data.table is.data.table
#'
#' @export
cv <- function (x, na.rm = FALSE) {

# checks
assert_that(qtest(na.rm, "B==1"), msg = "There is not a logical value for na.rm or more than 1 logical value for na.rm.")
# only process with enough known variables and provide an error message if the check fails


# The moments::kurtosis code has been helpful with regards to the treatment of
# na.rm & the use of apply functions for different R objects


if (is.matrix(x)) {

assert_that(!any(qtest(x, "m+[0,)") == FALSE), msg = "An element within the x matrix is Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

  apply(x, 2, cv, na.rm = na.rm)

} else if (is.vector(x)) {

# Check for x
assert_that(!any(qtest(x, "n>1[0,)") == FALSE), msg = "Either x is Inf, -Inf, empty, a string Or there is only 1 x value. There must be more than 1 x value. Please try again.")
# only process with finite values and provide an error message if the check fails

if (na.rm)

  x <- x[!is.na(x)]

  ((sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)) * 100)
# sample coefficient of variation

} else if (is.data.frame(x)) {

test_data_frame(x, types = character(0L), any.missing = TRUE, all.missing = TRUE, min.rows = NULL, max.rows = NULL, min.cols = NULL, max.cols = NULL, nrows = NULL, ncols = NULL, row.names = NULL, col.names = NULL, null.ok = FALSE)

  sapply(x, cv, na.rm = na.rm)

} else if (is.data.table(x)) {

x <- as.data.frame(x)

test_data_frame(x, types = character(0L), any.missing = TRUE, all.missing = TRUE, min.rows = NULL, max.rows = NULL, min.cols = NULL, max.cols = NULL, nrows = NULL, ncols = NULL, row.names = NULL, col.names = NULL, null.ok = FALSE)

  sapply(x, cv, na.rm = na.rm)

} else {

# Check for x
assert_that(!any(qtest(x, "n>1[0,)") == FALSE), msg = "Either x is Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

cv(as.vector(x), na.rm = na.rm)

}
}
