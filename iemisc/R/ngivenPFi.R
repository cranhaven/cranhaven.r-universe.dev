#' To Find i Given F, n, and P (Engineering Economics)
#'
#' Compute n given P, F, and i
#'
#' n is expressed as
#'
#' 	\deqn{n = \frac{\log \left(\frac{F}{P}\right)}{\log \left(1 + i\right)}}
#'
#' \describe{
#'	\item{\emph{n}}{the "number of interest periods"}
#'	\item{\emph{F}}{the "future equivalent"}
#'	\item{\emph{P}}{the "present equivalent"}
#'	\item{\emph{i}}{the "effective interest rate per interest period"}
#' }
#'
#'
#' @param F numeric vector that contains the future value(s)
#' @param i numeric vector that contains the interest rate(s) as a percent
#' @param P numeric vector that contains the present value(s)
#'
#' @return n numeric vector that contains the period value(s)
#'
#' @references
#' William G. Sullivan, Elin M. Wicks, and C. Patrick Koelling, \emph{Engineering Economy}, Fourteenth Edition, Upper Saddle River, New Jersey: Pearson/Prentice Hall, 2009, page 129, 142.
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
#' @examples
#'
#' # Example for equation 4-7 from the Reference text (page 142)
#'
#' library(iemisc)
#'
#' ngivenPFi(P = 500, F = 1000, i = 15)
#'
#'
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#' @export
ngivenPFi <- function (P, F, i) {

checks <- c(P, F, i)

# Check
assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either P, F, or i is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails



i <- i / 100

ngivenPFi <- log(F / P) / log(1 + i)

return(ngivenPFi)
}
