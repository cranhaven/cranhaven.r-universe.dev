#' Harmonic mean
#'
#' This function computes the sample harmonic mean.
#'
#' Harmonic mean is expressed as
#'
#' \deqn{\bar{x}_h = \frac{1}{\left(\frac{1}{n}\right)\left[\left(\frac{1}{x_1}\right) + \left(\frac{1}{x_2}\right) + \cdots + \left(\frac{1}{x_n}\right)\right]}}
#'
#' \describe{
#'	\item{\emph{\eqn{\bar{x}_h}}}{the sample harmonic mean}
#'	\item{\emph{x}}{the values in a sample}
#'	\item{\emph{n}}{the number of values}
#' }
#'
#'   "The harmonic mean is the reciprocal of the mean of the reciprocals. It is
#'   applied in situations where the reciprocal of a variable is averaged."
#'
#'
#' @param x numeric vector that contains the sample data points.
#' @param na.rm logical vector that determines whether the missing
#'   values should be removed or not.
#'
#' @return sample harmonic mean as a numeric vector. The default choice is that
#'   any NA values will be kept (\code{na.rm = FALSE}). This can be changed by
#'   specifying \code{na.rm = TRUE}, such as \code{shm(x, na.rm = TRUE)}.
#'
#'
#'
#' @references
#' Nathabandu T. Kottegoda and Renzo Rosso, \emph{Statistics, Probability, and Reliability for Civil and Environmental Engineers}, New York City, New York: The McGraw-Hill Companies, Inc., 1997, page 13.
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
#'
#' @seealso \code{\link[base]{mean}} for arithmetic mean
#'
#'
#' @seealso \code{\link{sgm}} for geometric mean, \code{\link{cv}} for coefficient of
#'  variation (CV), \code{\link{relerror}} for relative error, \code{\link{approxerror}} for
#'  approximate error, \code{\link{rms}} for root-mean-square (RMS), and \code{\link{ranges}}
#'  for sample range.
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
#' # Example 1.12 from Kottegoda (page 13)
#'
#' install.load::load_package("iemisc", "data.table")
#'
#' x <- c(0.20, 0.24, 0.16) # stream velocities in m/s
#' shm(x)
#'
#' # using a matrix of the numeric vector x
#' mat1 <- matrix(data = x, nrow = length(x), ncol = 1, byrow = FALSE,
#'         dimnames = list(c(rep("", length(x))), "Velocities"))
#' shm(mat1)
#'
#'
#' # using a data.frame of the numeric vector x
#' df1 <- data.frame(x)
#' shm(df1)
#'
#'
#' # using a data.table of the numeric vector x
#' df2 <- data.table(x)
#' shm(df2)
#'
#'
#'
#' @importFrom checkmate qtest
#' @importFrom assertthat assert_that
#' @importFrom data.table is.data.table
#'
#'
#' @export
shm <- function (x, na.rm = FALSE) {

# Check
assert_that(qtest(na.rm, "B==1"), msg = "There is not a logical value for na.rm or more than 1 logical value for na.rm.")
# only process with enough known variables and provide an error message if the check fails

# The moments::kurtosis code has been helpful with regards to the treatment of na.rm

  n <- length(x)

if (is.matrix(x))

  apply(x, 2, shm, na.rm = na.rm)

else if (is.vector(x)) {

if (na.rm)

  x <- x[!is.na(x)]

  1 / ((1 / n) * sum(1 / x, na.rm = na.rm))
# sample harmonic mean

} else if (is.data.frame(x))

  sapply(x, shm, na.rm = na.rm)

else if (is.data.table(x))

  sapply(x, shm, na.rm = na.rm)

else shm(as.vector(x), na.rm = na.rm)

}





#' Geometric mean
#'
#' This function computes the sample geometric mean.
#'
#' Geometric mean is expressed as
#'
#' \deqn{\bar{x}_g = \left(x_{1}x_{2} \cdots x_{n}\right)^{\frac{1}{n}}}
#'
#' \describe{
#'	\item{\emph{\eqn{\bar{x}_g}}}{the sample geometric mean}
#'	\item{\emph{x}}{the values in a sample}
#'	\item{\emph{n}}{the number of positive values}
#' }
#'
#'   "The geometric mean is used in averaging values that represent a rate of
#'   change. It is the positive nth root of the product of the n values."
#'
#'
#' @param x numeric vector that contains the sample data points (any
#'    negative values will be ignored).
#' @param na.rm logical vector that determines whether the missing
#'   values should be removed or not.
#'
#' @return sample geometric mean as a numeric vector. The default choice is
#'   that any NA values will be kept (\code{na.rm = FALSE}). This can be
#'   changed by specifying \code{na.rm = TRUE}, such as \code{sgm(x, na.rm = TRUE)}.
#'
#'
#' @references
#' Nathabandu T. Kottegoda and Renzo Rosso, \emph{Statistics, Probability, and Reliability for Civil and Environmental Engineers}, New York City, New York: The McGraw-Hill Companies, Inc., 1997, page 13.
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
#'
#' @seealso \code{\link[base]{mean}} for arithmetic mean
#'
#'
#' @seealso \code{\link{shm}} for harmonic mean, \code{\link{cv}} for coefficient of
#'  variation (CV), \code{\link{relerror}} for relative error, \code{\link{approxerror}} for
#'  approximate error, \code{\link{rms}} for root-mean-square (RMS), and \code{\link{ranges}}
#'  for sample range.
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
#' # Example 1.13 from Kottegoda (page 13)
#'
#' library(iemisc)
#'
#' city_pop <- c(230000, 310000)
#' sgm(city_pop)
#'
#' # Compare the geometric mean to the arithmetic mean
#' mean(city_pop)
#'
#'
#'
#' @importFrom checkmate qtest
#' @importFrom assertthat assert_that
#'
#'
#' @export
sgm <- function (x, na.rm = FALSE) {

# Check
assert_that(!any(qtest(x, "N+(,)") == FALSE), msg = "Either x is NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(qtest(na.rm, "B==1"), msg = "There is not a logical value for na.rm or more than 1 logical value for na.rm.")
# only process with enough known variables and provide an error message if the check fails


# The moments::kurtosis code has been helpful with regards to the treatment of na.rm

newx <- subset(x, x > 0)
# subset of x where the values are greater than 0

n <- length(newx)

xuse <- prod(newx, na.rm = na.rm)
# product of all values of newx

nthroot(xuse, n)
# sample geometric mean

}
