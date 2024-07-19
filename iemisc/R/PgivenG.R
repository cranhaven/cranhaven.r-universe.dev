#' Present value given Gradient value (Engineering Economics)
#'
#' Compute P given G
#'
#' 	\deqn{P = G\left\lbrace \frac{1}{i} \left[\frac{\left(1 + i\right)^n - 1}{i\left(1 + i\right)^n} - \frac{n}{\left(1 + i\right)^n}\right]\right\rbrace }
#'
#' \describe{
#'	\item{\emph{P}}{the "present equivalent"}
#'	\item{\emph{G}}{the "uniform gradient amount"}
#'	\item{\emph{i}}{the "effective interest rate per interest period"}
#'   \item{\emph{n}}{the "number of interest periods"}
#' }
#'
#'
#' @param G numeric vector that contains the gradient value(s)
#' @param n numeric vector that contains the period value(s)
#' @param i numeric vector that contains the interest rate(s) as a percent
#' @param frequency character vector that contains the frequency used to
#'    obtain the number of periods [annual (1), semiannual (2), quarter (4),
#'    bimonth (6), month (12), daily (365)]
#'
#' @return PgivenG numeric vector that contains the present value(s) rounded to
#'    2 decimal places
#'
#'
#' @references
#' William G. Sullivan, Elin M. Wicks, and C. Patrick Koelling, \emph{Engineering Economy}, Fourteenth Edition, Upper Saddle River, New Jersey: Pearson/Prentice Hall, 2009, page 142, 150, 152-154.
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
#' # Example 4-20 from the Reference text (pages 153-154)
#' PgivenG(1000, 4, 15, "annual") # the interest rate is 15%
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom round round_r3
#'
#' @export
PgivenG <- function (G, n, i, frequency = c("annual", "semiannual", "quarter", "bimonth", "month", "daily")) {

frequency <- frequency

checks <- c(G, n, i)

# Check
assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either G, n, or i is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(qtest(frequency, "S==1"), msg = "There is not a frequency or more than 1 frequency is stated. Please specify either 'annual', 'semiannual', 'quarter', 'bimonth', 'month', or 'daily'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("annual", "semiannual", "quarter", "bimonth", "month", "daily") %in% frequency)), msg = "Incorrect frequency. The only options are annual, semiannual, quarter, bimonth, month, daily. Please try again.")
# only process with a specified frequency and provide a stop warning if not


i <- i / 100

fr <- frequency

if (fr == "annual") {
fr <- 1
n <- n * fr

i <- i / fr

PgivenG <- G * (1 / i) * (((((1 + i) ^ n) - 1) / (i * ((1 + i) ^ n))) - (n / (1 + i) ^ n))

return(round_r3(PgivenG, d = 2))

} else if (fr == "semiannual") {

fr <- 2
n <- n * fr

i <- i / fr

PgivenG <- G * (1 / i) * (((((1 + i) ^ n) - 1) / (i * ((1 + i) ^ n))) - (n / (1 + i) ^ n))

return(round_r3(PgivenG, d = 2))

} else if (fr == "quarter") {

fr <- 4
n <- n * fr

i <- i / fr

PgivenG <- G * (1 / i) * (((((1 + i) ^ n) - 1) / (i * ((1 + i) ^ n))) - (n / (1 + i) ^ n))

return(round_r3(PgivenG, d = 2))

} else if (fr == "bimonth") {

fr <- 6
n <- n * fr

i <- i / fr

PgivenG <- G * (1 / i) * (((((1 + i) ^ n) - 1) / (i * ((1 + i) ^ n))) - (n / (1 + i) ^ n))

return(round_r3(PgivenG, d = 2))

} else if (fr == "month") {

fr <- 12
n <- n * fr

i <- i / fr

PgivenG <- G * (1 / i) * (((((1 + i) ^ n) - 1) / (i * ((1 + i) ^ n))) - (n / (1 + i) ^ n))

return(round_r3(PgivenG, d = 2))

} else if (fr == "daily") {

fr <- 365
n <- n * fr

i <- i / fr

PgivenG <- G * (1 / i) * (((((1 + i) ^ n) - 1) / (i * ((1 + i) ^ n))) - (n / (1 + i) ^ n))

return(round_r3(PgivenG, d = 2))

}
}







#' Annual value given Gradient value (Engineering Economics)
#'
#' Compute A given G
#'
#' 	\deqn{A = G\left[\frac{1}{i} - \frac{n}{\left(1 + i\right)^n - 1}\right]}
#'
#' \describe{
#'	\item{\emph{A}}{the "uniform series amount (occurs at the end of each
#'   interest period)"}
#'	\item{\emph{G}}{the "uniform gradient amount"}
#'	\item{\emph{i}}{the "effective interest rate per interest period"}
#'	\item{\emph{n}}{the "number of interest periods"}
#' }
#'
#'
#' @param G numeric vector that contains the gradient value(s)
#' @param n numeric vector that contains the period value(s)
#' @param i numeric vector that contains the interest rate(s) as a percent
#' @param frequency character vector that contains the frequency used to
#'    obtain the number of periods [annual (1), semiannual (2), quarter (4),
#'    bimonth (6), month (12), daily (365)]
#'
#' @return AgivenG numeric vector that contains the annual value(s) rounded to
#'    2 decimal places
#'
#'
#' @references
#' William G. Sullivan, Elin M. Wicks, and C. Patrick Koelling, \emph{Engineering Economy}, Fourteenth Edition, Upper Saddle River, New Jersey: Pearson/Prentice Hall, 2009, page 142, 150, 152-154, 164, 166-167.
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
##' 
#' library(iemisc)
#'
#' # Example 4-20 from the Reference text (pages 153-154)
#'   AgivenG(1000, 4, 15, "annual") # the interest rate is 15\%
#'
#'
#' # Example 4-31 from the Reference text (pages 166-167)
#'   AgivenG(1000, 4, 20, "semiannual") # the nominal interest rate is 20\% compounded semiannually
#'
#'
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#' @export
AgivenG <- function (G, n, i, frequency = c("annual", "semiannual", "quarter", "bimonth", "month", "daily")) {


frequency <- frequency

checks <- c(G, n, i)

# Check
assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either G, n, or i is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(qtest(frequency, "S==1"), msg = "There is not a frequency or more than 1 frequency is stated. Please specify either 'annual', 'semiannual', 'quarter', 'bimonth', 'month', or 'daily'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("annual", "semiannual", "quarter", "bimonth", "month", "daily") %in% frequency)), msg = "Incorrect frequency. The only options are annual, semiannual, quarter, bimonth, month, daily. Please try again.")
# only process with a specified frequency and provide a stop warning if not




i <- i / 100

fr <- frequency

if (fr == "annual") {
fr <- 1
n <- n * fr

i <- i / fr

AgivenG <- G * ((1 / i) - (n / ((1 + i) ^ n - 1)))

return(round_r3(AgivenG, d = 2))

} else if (fr == "semiannual") {

fr <- 2
n <- n * fr

i <- i / fr

AgivenG <- G * ((1 / i) - (n / ((1 + i) ^ n - 1)))

return(round_r3(AgivenG, d = 2))

} else if (fr == "quarter") {

fr <- 4
n <- n * fr

i <- i / fr

AgivenG <- G * ((1 / i) - (n / ((1 + i) ^ n - 1)))

return(round_r3(AgivenG, d = 2))

} else if (fr == "bimonth") {

fr <- 6
n <- n * fr

i <- i / fr

AgivenG <- G * ((1 / i) - (n / ((1 + i) ^ n - 1)))

return(round_r3(AgivenG, d = 2))

} else if (fr == "month") {

fr <- 12
n <- n * fr

i <- i / fr

AgivenG <- G * ((1 / i) - (n / ((1 + i) ^ n - 1)))

return(round_r3(AgivenG, d = 2))

} else if (fr == "daily") {

fr <- 365
n <- n * fr

i <- i / fr

AgivenG <- G * ((1 / i) - (n / ((1 + i) ^ n - 1)))

return(round_r3(AgivenG, d = 2))

}
}
