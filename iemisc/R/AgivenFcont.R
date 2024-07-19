#' Annual value given Future value [continuous] (Engineering Economics)
#'
#' Compute A given F with interest compounded continuously
#'
#' A is expressed as
#'
#' 	\deqn{A = F\left[\frac{e^{r} - 1}{e^{rn} - 1}\right]}
#'
#' \describe{
#'	\item{\emph{A}}{the "annual equivalent amount (occurs at the end of
#'     each year)"}
#'	\item{\emph{F}}{the "future equivalent"}
#'	\item{\emph{r}}{the "nominal annual interest rate, compounded
#'     continuously"}
#'	\item{\emph{n}}{the "number of periods (years)"}
#' }
#'
#'
#' @param F numeric vector that contains the future value(s)
#' @param n numeric vector that contains the period value(s)
#' @param r numeric vector that contains the continuously compounded nominal
#'    annual interest rate(s) as a percent
#'
#' @return AgivenFcont numeric vector that contains the annual value(s)
#'    rounded to 2 decimal places
#'
#'
#'
#' @references
#' William G. Sullivan, Elin M. Wicks, and C. Patrick Koelling, \emph{Engineering Economy}, Fourteenth Edition, Upper Saddle River, New Jersey: Pearson/Prentice Hall, 2009, page 169.
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
#' AgivenFcont(300, 2, 11) # 11\% interest
#'
#'
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom round round_r3
#'
#' @export
AgivenFcont <- function (F, n, r) {


checks <- c(F, n, r)

# check on F, n, and r
assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either F, n, or r is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails


r <- r / 100

AgivenFcont <- F * ((exp(r) - 1) / (exp(r * n) - 1))

return(round_r3(AgivenFcont, d = 2))
}







#' Present value given Future value [continuous] (Engineering Economics)
#'
#' Compute P given F with interest compounded continuously
#'
#' P is expressed as
#'
#' 	\deqn{P = Fe^{-rn}}
#'
#' \describe{
#'	\item{\emph{P}}{the "present equivalent"}
#'	\item{\emph{F}}{the "future equivalent"}
#'	\item{\emph{r}}{the "nominal annual interest rate, compounded
#'     continuously"}
#'	\item{\emph{n}}{the "number of periods (years)"}
#' }
#'
#'
#' @param F numeric vector that contains the future value(s)
#' @param n numeric vector that contains the period value(s)
#' @param r numeric vector that contains the continuously compounded nominal
#'    annual interest rate(s) as a percent
#'
#' @return PgivenFcont numeric vector that contains the present value(s)
#'    rounded to 2 decimal places
#'
#'
#'
#'
#' @references
#' William G. Sullivan, Elin M. Wicks, and C. Patrick Koelling, \emph{Engineering Economy}, Fourteenth Edition, Upper Saddle River, New Jersey: Pearson/Prentice Hall, 2009, page 169.
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
#' PgivenFcont(1000, 9, 7) # the interest rate is 7%
#'
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#' @export
PgivenFcont <- function (F, n, r) {

checks <- c(F, n, r)

# check on F, n, and r
assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either F, n, or r is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails



r <- r / 100

PgivenFcont <- F * exp(-r * n)

return(round_r3(PgivenFcont, d = 2))
}






#' Present value given Annual value [continuous] (Engineering Economics)
#'
#' Compute P given A with interest compounded continuously
#'
#' P is expressed as
#'
#' 	\deqn{P = A\left[\frac{e^{rn} - 1}{e^{rn}\left(e^{r} - 1\right)}\right]}
#'
#' \describe{
#'	\item{\emph{P}}{the "present equivalent"}
#'	\item{\emph{A}}{the "annual equivalent amount (occurs at the end of
#'     each year)"}
#'	\item{\emph{r}}{the "nominal annual interest rate, compounded
#'     continuously"}
#'	\item{\emph{n}}{the "number of periods (years)"}
#' }
#'
#'
#' @param A numeric vector that contains the annual value(s)
#' @param n numeric vector that contains the period value(s)
#' @param r numeric vector that contains the continuously compounded nominal
#'    annual interest rate(s) as a percent
#'
#' @return PgivenAcont numeric vector that contains the present value(s)
#'    rounded to 2 decimal places
#'
#'
#'
#' @references
#' William G. Sullivan, Elin M. Wicks, and C. Patrick Koelling, \emph{Engineering Economy}, Fourteenth Edition, Upper Saddle River, New Jersey: Pearson/Prentice Hall, 2009, page 169.
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
#' PgivenAcont(2000, 3, 12) # the interest rate is 12%
#'
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#' @export
PgivenAcont <- function (A, n, r) {


checks <- c(A, n, r)

# check on A, n, and r
assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either A, n, or r is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails


r <- r / 100

PgivenAcont <- A * ((exp(r * n) - 1) / (exp(r * n) * (exp(r) -1)))

return(round_r3(PgivenAcont, d = 2))
}






#' Future value given Present value [continuous] (Engineering Economics)
#'
#' Compute F given P with interest compounded continuously
#'
#' F is expressed as
#'
#' 	\deqn{F = Pe^{rn}}
#'
#' \describe{
#'	\item{\emph{F}}{the "future equivalent"}
#'	\item{\emph{P}}{the "present equivalent"}
#'	\item{\emph{r}}{the "nominal annual interest rate, compounded
#'     continuously"}
#'	\item{\emph{n}}{the "number of periods (years)"}
#' }
#'
#'
#' @param P numeric vector that contains the present value(s)
#' @param n numeric vector that contains the period value(s)
#' @param r numeric vector that contains the continuously compounded nominal
#'    annual interest rate(s) as a percent
#'
#' @return FgivenPcont numeric vector that contains the future value(s)
#'    rounded to 2 decimal places
#'
#'
#'
#' @references
#' William G. Sullivan, Elin M. Wicks, and C. Patrick Koelling, \emph{Engineering Economy}, Fourteenth Edition, Upper Saddle River, New Jersey: Pearson/Prentice Hall, 2009, page 169-170.
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
#' # Example 4-33 from the Reference text (page 170)
#' FgivenPcont(10000, 2, 5) # the interest rate is 5%
#'
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#' @export
FgivenPcont <- function (P, n, r) {


checks <- c(P, n, r)

# check on P, n, and r
assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either P, n, or r is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails


r <- r / 100

FgivenPcont <- P * exp(r * n)

return(round_r3(FgivenPcont, d = 2))
}






#' Future value given Annual value [continuous] (Engineering Economics)
#'
#' Compute F given A with interest compounded continuously
#'
#' F is expressed as
#'
#' 	\deqn{F = A\left[\frac{e^{rn} - 1}{e^{r} - 1}\right]}
#'
#' \describe{
#'	\item{\emph{F}}{the "future equivalent"}
#'	\item{\emph{A}}{the "annual equivalent amount (occurs at the end of
#'     each year)"}
#'	\item{\emph{r}}{the "nominal annual interest rate, compounded
#'     continuously"}
#'	\item{\emph{n}}{the "number of periods (years)"}
#' }
#'
#'
#' @param A numeric vector that contains the annual value(s)
#' @param n numeric vector that contains the period value(s)
#' @param r numeric vector that contains the continuously compounded nominal
#'    annual interest rate(s) as a percent
#'
#' @return FgivenAcont numeric vector that contains the future value(s)
#'    rounded to 2 decimal places
#'
#'
#'
#' @references
#' William G. Sullivan, Elin M. Wicks, and C. Patrick Koelling, \emph{Engineering Economy}, Fourteenth Edition, Upper Saddle River, New Jersey: Pearson/Prentice Hall, 2009, page 169.
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
#' FgivenAcont(2100, 13, 7) # the interest rate is 7%
#'
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#' @export
FgivenAcont <- function (A, n, r) {


checks <- c(A, n, r)

# check on A, n, and r
assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either A, n, or r is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails


r <- r / 100

FgivenAcont <- A * ((exp(r * n) - 1) / (exp(r) - 1))

return(round_r3(FgivenAcont, d = 2))
}






#' Annual value given Present value [continuous] (Engineering Economics)
#'
#' Compute A given P with interest compounded continuously
#'
#' A is expressed as
#'
#' 	\deqn{A = P\left[\frac{e^{rn}\left(e^{r} - 1\right)}{e^{rn} - 1}\right]}
#'
#' \describe{
#'	\item{\emph{A}}{the "annual equivalent amount (occurs at the end of
#'     each year)"}
#'	\item{\emph{P}}{the "present equivalent"}
#'	\item{\emph{r}}{the "nominal annual interest rate, compounded
#'     continuously"}
#'	\item{\emph{n}}{the "number of periods (years)"}
#' }
#'
#'
#' @param P numeric vector that contains the present value(s)
#' @param n numeric vector that contains the period value(s)
#' @param r numeric vector that contains the continuously compounded nominal
#'    annual interest rate(s) as a percent
#'
#' @return AgivenPcont numeric vector that contains the annual value(s)
#'    rounded to 2 decimal places
#'
#'
#'
#' @references
#' William G. Sullivan, Elin M. Wicks, and C. Patrick Koelling, \emph{Engineering Economy}, Fourteenth Edition, Upper Saddle River, New Jersey: Pearson/Prentice Hall, 2009, page 169-170.
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
#' # Example for equation 4-34 from the Reference text (page 170)
#' AgivenPcont(1000, 10, 20) # 20\% interest
#'
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#' @export
AgivenPcont <- function (P, n, r) {


checks <- c(P, n, r)

# check on P, n, and r
assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either P, n, or r is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails


r <- r / 100

AgivenPcont <- P * ((exp(r * n) * (exp(r) - 1)) / (exp(r * n) - 1))

return(round_r3(AgivenPcont, d = 2))
}
