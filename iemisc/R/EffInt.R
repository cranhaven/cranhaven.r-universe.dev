#' Effective Interest rate (Engineering Economics)
#'
#' Computes the effective interest rate given the nominal interest rate per
#' period
#'
#' i is expressed as
#'
#' 	\deqn{i = \left(1 + \frac{r}{n}\right)^n - 1}
#'
#' \describe{
#'	\item{\emph{i}}{the "effective interest rate per interest period"}
#'	\item{\emph{r}}{the "nominal interest rate"}
#'	\item{\emph{n}}{the "number of compounding periods per year"}
#' }
#'
#'
#' @param r numeric vector that contains the nominal interest rate(s) per
#'    period as a percent
#' @param frequency character vector that contains the frequency used to
#'    obtain the number of periods [annual (1), semiannual (2), quarter (4),
#'    bimonth (6), month (12), daily (365)]
#'
#' @return EffInt numeric vector that contains the effective interest rate
#'    rounded to 2 decimal places (this is the \code{i} used in the other
#'    Engineering Economics functions)
#'
#'
#'
#' @references
#' \enumerate{
#'  \item \emph{SFPE Handbook of Fire Protection Engineering}. 3rd Edition, DiNenno, P. J.; Drysdale, D.; Beyler, C. L.; Walton, W. D., Editor(s), page 5-95, 2002. Chapter 7; Section 5; NFPA HFPE-02. See \url{https://web.archive.org/web/20180127185316/http://fire.nist.gov/bfrlpubs/build02/PDF/b02155.pdf}.
#'  \item William G. Sullivan, Elin M. Wicks, and C. Patrick Koelling, \emph{Engineering Economy}, Fourteenth Edition, Upper Saddle River, New Jersey: Pearson/Prentice Hall, 2009, pages 164-165.
#' }
#'
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
#' library(iemisc)
#'
#' # Example 4-28 from Sullivan Reference text (page 165)
#' EffInt(1.375, frequency = "month")
#' # the nominal interest rate per period (month) is 1.375\%
#'
#'
#' # Example from SFPE Reference text
#' EffInt(18 / 12, frequency = "month")
#' # the nominal interest rate is 18\% per year or 18\% / 12 months
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom round round_r3
#'
#' @export
EffInt <- function (r, frequency = c("annual", "semiannual", "quarter", "bimonth", "month", "daily")) {

frequency <- frequency


# check on r and frequency
assert_that(!any(qtest(r, "N==1(,)") == FALSE), msg = "r is NA, NaN, Inf, -Inf, empty, or a string. Or, r has more than 1 value. Please check the r value and try again.")
# only process with enough known variables and provide an error message if the check fails

assert_that(qtest(frequency, "S==1"), msg = "There is not a frequency type or more than 1 frequency type.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("annual", "semiannual", "quarter", "bimonth", "month", "daily") %in% frequency)), msg = "The unit system has not been identified correctly as either 'annual', 'semiannual', 'quarter', 'bimonth', 'month', or 'daily'. Please try again.")
# only process with a specified unit and provide a stop warning if not


r <- r / 100

fr <- frequency

if (fr == "annual") {
fr <- 1
n <- fr

r <- (fr * r)

i <- r / n

EffInt <- ((1 + i) ^ n - 1) * 100 # effective interest rate

return(round_r3(EffInt, d = 2))

} else if (fr == "semiannual") {

fr <- 2
n <- fr

r <- (fr * r)

i <- r / n

EffInt <- ((1 + i) ^ n - 1) * 100 # effective interest rate

return(round_r3(EffInt, d = 2))

} else if (fr == "quarter") {

fr <- 4
n <- fr

r <- (fr * r)

i <- r / n

EffInt <- ((1 + i) ^ n - 1) * 100 # effective interest rate

return(round_r3(EffInt, d = 2))

} else if (fr == "bimonth") {

fr <- 6
n <- fr

r <- (fr * r)

i <- r / n

EffInt <- ((1 + i) ^ n - 1) * 100 # effective interest rate

return(round_r3(EffInt, d = 2))

} else if (fr == "month") {

fr <- 12
n <- fr

r <- (fr * r)

i <- r / n

EffInt <- ((1 + i) ^ n - 1) * 100 # effective interest rate

return(round_r3(EffInt, d = 2))

} else if (fr == "daily") {

fr <- 365
n <- fr

r <- (fr * r)

i <- r / n

EffInt <- ((1 + i) ^ n - 1) * 100 # effective interest rate

return(round_r3(EffInt, d = 2))

}
}
