#' Simple Interest Paid (Engineering Economics)
#'
#' Computes the total amount paid at the end of n periods using simple interest
#'
#' Simple Interest is expressed as
#'
#' \deqn{I = Pni}
#'
#' \deqn{S_n = P + I}
#'
#' or
#'
#' \deqn{S_n = P\left(1 + ni\right)}
#'
#' \describe{
#'	\item{\emph{P}}{the "principal amount (lent or borrowed)"}
#'	\item{\emph{\eqn{S_n}}}{the "total amount paid back"}
#'	\item{\emph{I}}{the "simple interest"}
#'	\item{\emph{i}}{the "interest rate per interest period"}
#'	\item{\emph{n}}{the "number of interest periods"}
#' }
#'
#'
#' @param P numeric vector that contains the present value(s)
#' @param n numeric vector that contains the period value(s)
#' @param i numeric vector that contains the interest rate(s) as whole number
#'    or decimal
#'
#' @return SimpIntPaid numeric vector that contains the total amount paid at
#'    the end of n periods rounded to 2 decimal places
#'
#'
#' @references
#' \enumerate{
#' \item Chinyere Onwubiko, \emph{An Introduction to Engineering}, Mission, Kansas: Schroff Development Corporation, 1997, page 205-206.
#' \item William G. Sullivan, Elin M. Wicks, and C. Patrick Koelling, \emph{Engineering Economy}, Fourteenth Edition, Upper Saddle River, New Jersey: Pearson/Prentice Hall, 2009, page 116.
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
#'
#' @examples
#' 
#' library(iemisc)
#'
#' # Example for equation 4-1 from the Sullivan Reference text (page 116)
#' SimpIntPaid(1000, 3, 10) # the interest rate is 10%
#'
#'
#'
#' @importFrom round round_r3
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#'
#'
#' @export
SimpIntPaid <- function (P, n, i) {

checks <- c(P, n, i)

# Check
assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either P, n, or i is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails


i <- i / 100

I <- P * n * i # total interest using simple interest

return(round_r3(P * (1 + n * i), d = 2)) # total amount paid
}










#' Compound Interest Paid (Engineering Economics)
#'
#' Computes the total amount paid at the end of n periods using compound
#' interest
#'
#' Compound Interest is expressed as
#'
#' \deqn{S_n = P\left(1 + i\right)^n}
#'
#' \describe{
#'	\item{\emph{P}}{the "principal amount (lent or borrowed)"}
#'	\item{\emph{\eqn{S_n}}}{the "total amount paid back"}
#'	\item{\emph{i}}{the "interest rate per interest period"}
#'	\item{\emph{n}}{the "number of interest periods"}
#' }
#'
#'
#' @param P numeric vector that contains the present value(s)
#' @param n numeric vector that contains the period value(s)
#' @param i numeric vector that contains the interest rate(s) as a percent
#' @param frequency character vector that contains the frequency used to
#'    obtain the number of periods [annual (1), semiannual (2), quarter (4),
#'    bimonth (6), month (12), daily (365)]
#'
#' @return CompIntPaid numeric vector that contains the total amount paid at
#'    the end of n periods rounded to 2 decimal places
#'
#'
#' @references
#' \enumerate{
#'  \item \emph{SFPE Handbook of Fire Protection Engineering}. 3rd Edition, DiNenno, P. J.; Drysdale, D.; Beyler, C. L.; Walton, W. D., Editor(s), page 5-94, 2002. Chapter 7; Section 5; NFPA HFPE-02. See \url{https://web.archive.org/web/20180127185316/http://fire.nist.gov/bfrlpubs/build02/PDF/b02155.pdf}.
#'  \item William G. Sullivan, Elin M. Wicks, and C. Patrick Koelling, \emph{Engineering Economy}, Fourteenth Edition, Upper Saddle River, New Jersey: Pearson/Prentice Hall, 2009, page 120.
#' \item Chinyere Onwubiko, \emph{An Introduction to Engineering}, Mission, Kansas: Schroff Development Corporation, 1997, pages 205-206.
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
#' # Compound Interest example from SFPE Reference text
#' CompIntPaid(100, 5, 10, frequency = "annual") # the interest rate is 10%
#'
#'
#'
#'
#' @importFrom round round_r3
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#'
#'
#' @export
CompIntPaid <- function (P, n, i, frequency = c("annual", "semiannual", "quarter", "bimonth", "month", "daily")) {

frequency <- frequency


checks <- c(P, n, i)

# Check
assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either P, n, or i is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
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

CompIntPaid <- P * (1 + i) ^ n # total amount paid

return(round_r3(CompIntPaid, d = 2))

} else if (fr == "semiannual") {

fr <- 2
n <- n * fr

i <- i / fr

CompIntPaid <- P * (1 + i) ^ n # total amount paid

return(round_r3(CompIntPaid, d = 2))

} else if (fr == "quarter") {

fr <- 4
n <- n * fr

i <- i / fr

CompIntPaid <- P * (1 + i) ^ n # total amount paid

return(round_r3(CompIntPaid, d = 2))

} else if (fr == "bimonth") {

fr <- 6
n <- n * fr

i <- i / fr

CompIntPaid <- P * (1 + i) ^ n # total amount paid

return(round_r3(CompIntPaid, d = 2))

} else if (fr == "month") {

fr <- 12
n <- n * fr

i <- i / fr

CompIntPaid <- P * (1 + i) ^ n # total amount paid

return(round_r3(CompIntPaid, d = 2))

} else if (fr == "daily") {

fr <- 365
n <- n * fr

i <- i / fr

CompIntPaid <- P * (1 + i) ^ n # total amount paid

return(round_r3(CompIntPaid, d = 2))

}
}









#' Simple Interest Charged (Engineering Economics)
#'
#' Computes the total interest paid at the end of n periods using simple
#' interest
#'
#' Simple Interest Charged is expressed as
#'
#' \deqn{I = Pni}
#'
#' \describe{
#'	\item{\emph{P}}{the "principal amount (lent or borrowed)"}
#'	\item{\emph{I}}{the "simple interest"}
#'	\item{\emph{i}}{the "interest rate per interest period"}
#'	\item{\emph{n}}{the "number of interest periods"}
#' }
#'
#'
#' @param P numeric vector that contains the present value(s)
#' @param n numeric vector that contains the period value(s)
#' @param i numeric vector that contains the interest rate(s) as whole number
#'    or decimal
#'
#' @return SimpIntCharg numeric vector that contains the simple interest amount
#'    paid at the end of n periods rounded to 2 decimal places
#'
#'
#' @references
#' \enumerate{
#' \item Chinyere Onwubiko, \emph{An Introduction to Engineering}, Mission, Kansas: Schroff Development Corporation, 1997, page 205-206.
#' \item William G. Sullivan, Elin M. Wicks, and C. Patrick Koelling, \emph{Engineering Economy}, Fourteenth Edition, Upper Saddle River, New Jersey: Pearson/Prentice Hall, 2009, page 116.
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
#'
#' @examples
#' 
#' library(iemisc)
#'
#' # Example for equation 4-1 from the Sullivan Reference text (page 116)
#' # Modified example to provide the simple interest amount paid only
#' 
#' SimpIntCharg(P = 1000, n = 3, i = 10) # the interest rate is 10%
#'
#'
#'
#' @importFrom round round_r3
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#'
#'
#' @export
SimpIntCharg <- function (P, n, i) {

checks <- c(P, n, i)

# Check
assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either P, n, or i is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails


i <- i / 100

I <- P * n * i # total interest using simple interest

return(round_r3(I, d = 2)) # total interest paid (accumulated)
}










#' Compound Interest Charged (Engineering Economics)
#'
#' Computes the total interest paid at the end of n periods using compound
#' interest
#'
#' Compound Interest Charged is expressed as
#'
#' \deqn{I = P\left(1 + i\right)^n - P}
#'
#' \describe{
#'	\item{\emph{P}}{the "principal amount (lent or borrowed)"}
#'	\item{\emph{\eqn{I}}}{the "total interest paid"}
#'	\item{\emph{i}}{the "interest rate per interest period"}
#'	\item{\emph{n}}{the "number of interest periods"}
#' }
#'
#'
#' @param P numeric vector that contains the present value(s)
#' @param n numeric vector that contains the period value(s)
#' @param i numeric vector that contains the interest rate(s) as a percent
#' @param frequency character vector that contains the frequency used to
#'    obtain the number of periods [annual (1), semiannual (2), quarter (4),
#'    bimonth (6), month (12), daily (365)]
#'
#' @return CompIntCharg numeric vector that contains the total interest paid at
#'    the end of n periods rounded to 2 decimal places
#'
#'
#' @references
#' \enumerate{
#'  \item \emph{SFPE Handbook of Fire Protection Engineering}. 3rd Edition, DiNenno, P. J.; Drysdale, D.; Beyler, C. L.; Walton, W. D., Editor(s), page 5-94, 2002. Chapter 7; Section 5; NFPA HFPE-02. See \url{https://web.archive.org/web/20180127185316/http://fire.nist.gov/bfrlpubs/build02/PDF/b02155.pdf}.
#'  \item William G. Sullivan, Elin M. Wicks, and C. Patrick Koelling, \emph{Engineering Economy}, Fourteenth Edition, Upper Saddle River, New Jersey: Pearson/Prentice Hall, 2009, page 120.
#' \item Chinyere Onwubiko, \emph{An Introduction to Engineering}, Mission, Kansas: Schroff Development Corporation, 1997, page 205-206.
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
#' # Compound Interest example from SFPE Reference text
#' # Modified example to provide the compounded interest amount paid only
#' 
#' CompIntCharg(100, 5, 10, frequency = "annual") # the interest rate is 10\%
#'
#'
#' @importFrom round round_r3
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#'
#'
#' @export
CompIntCharg <- function (P, n, i, frequency = c("annual", "semiannual", "quarter", "bimonth", "month", "daily")) {

frequency <- frequency


checks <- c(P, n, i)

# Check
assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either P, n, or i is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
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

CompIntCharg <- P * (1 + i) ^ n # total interest paid

CompIntCharg <- CompIntCharg - P

return(round_r3(CompIntCharg, d = 2))

} else if (fr == "semiannual") {

fr <- 2
n <- n * fr

i <- i / fr

CompIntCharg <- P * (1 + i) ^ n # total interest paid

CompIntCharg <- CompIntCharg - P

return(round_r3(CompIntCharg, d = 2))

} else if (fr == "quarter") {

fr <- 4
n <- n * fr

i <- i / fr

CompIntCharg <- P * (1 + i) ^ n # total interest paid

CompIntCharg <- CompIntCharg - P

return(round_r3(CompIntCharg, d = 2))

} else if (fr == "bimonth") {

fr <- 6
n <- n * fr

i <- i / fr

CompIntCharg <- P * (1 + i) ^ n # total interest paid

CompIntCharg <- CompIntCharg - P

return(round_r3(CompIntCharg, d = 2))

} else if (fr == "month") {

fr <- 12
n <- n * fr

i <- i / fr

CompIntCharg <- P * (1 + i) ^ n # total interest paid

CompIntCharg <- CompIntCharg - P

return(round_r3(CompIntCharg, d = 2))

} else if (fr == "daily") {

fr <- 365
n <- n * fr

i <- i / fr

CompIntCharg <- P * (1 + i) ^ n # total interest paid

CompIntCharg <- CompIntCharg - P

return(round_r3(CompIntCharg, d = 2))

}
}
