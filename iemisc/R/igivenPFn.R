#' Interest rate given Future value, Number of periods, and Present value (Engineering Economics)
#'
#' Compute i given F, n, and P
#'
#' i is expressed as
#'
#' 	\deqn{i = \sqrt[n]{\frac{F}{P}} - 1}
#'
#' \describe{
#'	\item{\emph{i}}{the "effective interest rate per interest period"}
#'	\item{\emph{F}}{the "future equivalent"}
#'	\item{\emph{P}}{the "present equivalent"}
#'	\item{\emph{n}}{the "number of interest periods"}
#' }
#'
#'
#' @param F numeric vector that contains the future value(s)
#' @param n numeric vector that contains the period value(s)
#' @param P numeric vector that contains the present value(s)
#'
#' @return i numeric vector that contains the effective interest rate as a
#' percent rounded to 2 decimal places
#'
#'
#'
#' @references
#' William G. Sullivan, Elin M. Wicks, and C. Patrick Koelling, \emph{Engineering Economy}, Fourteenth Edition, Upper Saddle River, New Jersey: Pearson/Prentice Hall, 2009, page 128-129, 142.
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
#' # Example for equation 4-6 from the Reference text (page 128)
#' 
#' library(iemisc)
#' 
#' igivenPFn(P = 500, F = 1000, n = 10)
#'
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom round round_r3
#'
#'
#'
#' @export
igivenPFn <- function (P, F, n) {


checks <- c(P, F, n)

# Check
assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either P, F, or n is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails


igivenPFn <- (nthroot(F / P, n) - 1) * 100 # interest rate as percent

return(round_r3(igivenPFn, d = 2))
}
















#' Simple Interest rate given Interest Charged, Number of years, and Principal value
#'
#' Compute i given IC, n, and P
#'
#' i is expressed as
#'
#' 	\deqn{i = {\frac{IC * 100}{P * n}}}
#'
#' \describe{
#'	\item{\emph{i}}{the interest rate}
#'	\item{\emph{IC}}{the total amount of interest charged}
#'	\item{\emph{P}}{the principal amount of the loan}
#'	\item{\emph{n}}{the number of years}
#' }
#'
#'
#' @param IC numeric vector that contains the total amount of interest charged
#' @param n numeric vector that contains the number of years
#' @param begin_event character vector that contains the start date
#' @param end_event character vector that contains the end date
#' @param P numeric vector that contains the principal value
#'
#' @return i numeric vector that contains the simple interest rate as a
#' percent rounded to 2 decimal places
#'
#'
#'
#' @references
#' TN Code § 47-14-102 (2021). 2021 Tennessee Code -- Title 47 - Commercial Instruments and Transactions -- Chapter 14 - Interest Rates Generally -- § 47-14-102. Definitions. See \url{https://law.justia.com/codes/tennessee/2021/title-47/chapter-14/section-47-14-102/}.
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
#' # Example 1
#' 
#' library(iemisc)
#' 
#' igivenICPn(P = 500, IC = 1000, n = 10)
#'
#'
#'
#'
#' # Example 2
#' 
#' library(iemisc)
#' 
#' igivenICPn(P = 500, IC = 1000, begin_event = "1 January 2020", end_event = "1 January 2030")
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom lubridate %--% dyears
#' @importFrom anytime anydate
#' @importFrom round round_r3
#' @importFrom sjmisc is_empty
#'
#'
#'
#' @export
igivenICPn <- function (P, IC, n = NULL, begin_event = NULL, end_event = NULL) {


checks1 <- c(P, IC)

# Check
assert_that(qtest(checks1, "N==2"), msg = "Either P or IC is missing. Please try again.")
# only process with enough known variables and provide an error message if the check fails


# if the number of years is present
if (!missing(n)) {

n <- n

checks <- c(P, IC, n)

checks2 <- n

assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either P, IC, or n is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

# Check
assert_that(qtest(checks2, "N==1"), msg = "n is missing. Please try again.")
# only process with enough known variables and provide an error message if the check fails


} else {

checks3 <- c(begin_event, end_event)

# Check
assert_that(!sjmisc::is_empty(checks3), msg = "Either begin_event or end_event is missing. Please try again.")
# only process with enough known variables and provide an error message if the check fails

Time_begin <- anydate(begin_event)

Time_end <- anydate(end_event)

n <- (Time_begin %--% Time_end)/dyears(1)

}

igivenICPn <- (IC * 100) / (P * n) # interest rate as percent

return(round_r3(igivenICPn, d = 2))

}
