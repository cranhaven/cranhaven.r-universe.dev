#' Future value given Annual value (Engineering Economics)
#'
#' Compute F given A
#'
#' F is expressed as
#'
#' 	\deqn{F = A\left[\frac{\left(1 + i\right)^n - 1}{i}\right]}
#'
#' \describe{
#'	\item{\emph{F}}{the "future equivalent"}
#'	\item{\emph{A}}{the "uniform series amount (occurs at the end of each
#'     interest period)"}
#'	\item{\emph{i}}{the "effective interest rate per interest period"}
#'	\item{\emph{n}}{the "number of interest periods"}
#' }
#'
#'
#' @param A numeric vector that contains the annual value(s)
#' @param n numeric vector that contains the period value(s)
#' @param i numeric vector that contains the interest rate(s) as a percent
#' @param frequency character vector that contains the frequency used to
#'    obtain the number of periods [annual (1), semiannual (2), quarter (4),
#'    bimonth (6), month (12), daily (365)]
#'
#' @return FgivenA numeric vector that contains the future value(s) rounded to
#'    2 decimal places
#' @return FA data.frame of both n (0 to n) and the resulting future values
#'    rounded to 2 decimal places
#'
#'
#'
#' @references
#' William G. Sullivan, Elin M. Wicks, and C. Patrick Koelling, \emph{Engineering Economy}, Fourteenth Edition, Upper Saddle River, New Jersey: Pearson/Prentice Hall, 2009, page 131-132, 142, 164.
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
#' # Example 4-7 from the Reference text (page 131-132)
#' FgivenA(23000, 40, 6, "annual") # the interest rate is 6\%
#'
#' FA(23000, 40, 6, "annual") # the interest rate is 6\%
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom data.table data.table setnames setDF
#' @importFrom round round_r3
#'
#' @name FgivenA
NULL

#' @export
#' @rdname FgivenA
FgivenA <- function (A, n, i, frequency = c("annual", "semiannual", "quarter", "bimonth", "month", "daily")) {


frequency <- frequency

checks <- c(A, n, i)

# Check
assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either A, n, or i is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
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

FgivenA <- A * (((1 + i) ^ n - 1) / i)

return(round_r3(FgivenA, d = 2))

} else if (fr == "semiannual") {

fr <- 2
n <- n * fr

i <- i / fr

FgivenA <- A * (((1 + i) ^ n - 1) / i)

return(round_r3(FgivenA, d = 2))

} else if (fr == "quarter") {

fr <- 4
n <- n * fr

i <- i / fr

FgivenA <- A * (((1 + i) ^ n - 1) / i)

return(round_r3(FgivenA, d = 2))

} else if (fr == "bimonth") {

fr <- 6
n <- n * fr

i <- i / fr

FgivenA <- A * (((1 + i) ^ n - 1) / i)

return(round_r3(FgivenA, d = 2))

} else if (fr == "month") {

fr <- 12
n <- n * fr

i <- i / fr

FgivenA <- A * (((1 + i) ^ n - 1) / i)

return(round_r3(FgivenA, d = 2))

} else if (fr == "daily") {

fr <- 365
n <- n * fr

i <- i / fr

FgivenA <- A * (((1 + i) ^ n - 1) / i)

return(round_r3(FgivenA, d = 2))

}
}



#' @export
#' @rdname FgivenA
FA <- function (A, n, i, frequency = c("annual", "semiannual", "quarter", "bimonth", "month", "daily")) {

frequency <- frequency


# check on A, n, i, and frequency
assert_that(!any(qtest(A, "N==1(,)") == FALSE), msg = "A is NA, NaN, Inf, -Inf, empty, or a string. Or, A has more than 1 value. Please check the A value and try again.")
# only process with enough known variables and provide an error message if the check fails

assert_that(!any(qtest(n, "N==1(,)") == FALSE), msg = "n is NA, NaN, Inf, -Inf, empty, or a string. Or, n has more than 1 value. Please check the n value and try again.")
# only process with enough known variables and provide an error message if the check fails

assert_that(!any(qtest(i, "N==1(,)") == FALSE), msg = "i is NA, NaN, Inf, -Inf, empty, or a string. Or, i has more than 1 value. Please check the i value and try again.")
# only process with enough known variables and provide an error message if the check fails


assert_that(qtest(frequency, "S==1"), msg = "There is not a frequency type or more than 1 frequency type.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("annual", "semiannual", "quarter", "bimonth", "month", "daily") %in% frequency)), msg = "The unit system has not been identified correctly as either 'annual', 'semiannual', 'quarter', 'bimonth', 'month', or 'daily'. Please try again.")
# only process with a specified unit and provide a stop warning if not


i <- i / 100

fr <- frequency

if (fr == "annual") {
fr <- 1
n <- n * fr

FA <- vector("list", length(1:n))
## Source 1 and 2 / pre-allocate the list since it is being used in a for loop

for (y in 1:length(n)) {
FA[[y]] <- A * (((1 + i) ^ seq(n) - 1) / i)
}

FA <- data.table(seq(n), unlist(FA))

F0 <- NA
F0 <- data.table(0, F0)
F0 <- setnames(F0, 2, "V2")

FA <- rbind(F0, FA)

setnames(FA, c("n (periods)", "Future Worth ($US)"))


# Round the numeric values to 2 decimal places
cols <- "Future Worth ($US)"

for (col in cols) {

idx <- which(!is.na(FA[[col]]))

data.table::set(FA, i = idx, j = col, value = round_r3(FA[[col]][idx], d = 2))

}



col.names <- c("n (periods)", "Future Worth ($US)")

# code block below modified from data.table function
setattr(FA, "col.names", setnames(FA, col.names))
setattr(FA, "class", c("data.table", "data.frame"))
FA


} else if (fr == "semiannual") {

fr <- 2
n <- n * fr

i <- i / fr

FA <- vector("list", length(1:n))
## Source 1 and 2 / pre-allocate the list since it is being used in a for loop

for (y in 1:length(n)) {
FA[[y]] <- A * (((1 + i) ^ seq(n) - 1) / i)
}

FA <- data.table(seq(n), unlist(FA))

F0 <- NA
F0 <- data.table(0, F0)
F0 <- setnames(F0, 2, "V2")

FA <- rbind(F0, FA)

setnames(FA, c("n (periods)", "Future Worth ($US)"))


# Round the numeric values to 2 decimal places
cols <- "Future Worth ($US)"

for (col in cols) {

idx <- which(!is.na(FA[[col]]))

data.table::set(FA, i = idx, j = col, value = round_r3(FA[[col]][idx], d = 2))

}



col.names <- c("n (periods)", "Future Worth ($US)")

# code block below modified from data.table function
setattr(FA, "col.names", setnames(FA, col.names))
setattr(FA, "class", c("data.table", "data.frame"))
FA


} else if (fr == "quarter") {

fr <- 4
n <- n * fr

i <- i / fr

FA <- vector("list", length(1:n))
## Source 1 and 2 / pre-allocate the list since it is being used in a for loop

for (y in 1:length(n)) {
FA[[y]] <- A * (((1 + i) ^ seq(n) - 1) / i)
}

FA <- data.table(seq(n), unlist(FA))

F0 <- NA
F0 <- data.table(0, F0)
F0 <- setnames(F0, 2, "V2")

FA <- rbind(F0, FA)

setnames(FA, c("n (periods)", "Future Worth ($US)"))


# Round the numeric values to 2 decimal places
cols <- "Future Worth ($US)"

for (col in cols) {

idx <- which(!is.na(FA[[col]]))

data.table::set(FA, i = idx, j = col, value = round_r3(FA[[col]][idx], d = 2))

}



col.names <- c("n (periods)", "Future Worth ($US)")

# code block below modified from data.table function
setattr(FA, "col.names", setnames(FA, col.names))
setattr(FA, "class", c("data.table", "data.frame"))
FA


} else if (fr == "bimonth") {

fr <- 6
n <- n * fr

i <- i / fr

FA <- vector("list", length(1:n))
## Source 1 and 2 / pre-allocate the list since it is being used in a for loop

for (y in 1:length(n)) {
FA[[y]] <- A * (((1 + i) ^ seq(n) - 1) / i)
}

FA <- data.table(seq(n), unlist(FA))

F0 <- NA
F0 <- data.table(0, F0)
F0 <- setnames(F0, 2, "V2")

FA <- rbind(F0, FA)

setnames(FA, c("n (periods)", "Future Worth ($US)"))


# Round the numeric values to 2 decimal places
cols <- "Future Worth ($US)"

for (col in cols) {

idx <- which(!is.na(FA[[col]]))

data.table::set(FA, i = idx, j = col, value = round_r3(FA[[col]][idx], d = 2))

}



col.names <- c("n (periods)", "Future Worth ($US)")

# code block below modified from data.table function
setattr(FA, "col.names", setnames(FA, col.names))
setattr(FA, "class", c("data.table", "data.frame"))
FA


} else if (fr == "month") {

fr <- 12
n <- n * fr

i <- i / fr

FA <- vector("list", length(1:n))
## Source 1 and 2 / pre-allocate the list since it is being used in a for loop

for (y in 1:length(n)) {
FA[[y]] <- A * (((1 + i) ^ seq(n) - 1) / i)
}

FA <- data.table(seq(n), unlist(FA))

F0 <- NA
F0 <- data.table(0, F0)
F0 <- setnames(F0, 2, "V2")

FA <- rbind(F0, FA)

setnames(FA, c("n (periods)", "Future Worth ($US)"))


# Round the numeric values to 2 decimal places
cols <- "Future Worth ($US)"

for (col in cols) {

idx <- which(!is.na(FA[[col]]))

data.table::set(FA, i = idx, j = col, value = round_r3(FA[[col]][idx], d = 2))

}



col.names <- c("n (periods)", "Future Worth ($US)")

# code block below modified from data.table function
setattr(FA, "col.names", setnames(FA, col.names))
setattr(FA, "class", c("data.table", "data.frame"))
FA


} else if (fr == "daily") {

fr <- 365
n <- n * fr

i <- i / fr

FA <- vector("list", length(1:n))
## Source 1 and 2 / pre-allocate the list since it is being used in a for loop

for (y in 1:length(n)) {
FA[[y]] <- A * (((1 + i) ^ seq(n) - 1) / i)
}

FA <- data.table(seq(n), unlist(FA))

F0 <- NA
F0 <- data.table(0, F0)
F0 <- setnames(F0, 2, "V2")

FA <- rbind(F0, FA)

setnames(FA, c("n (periods)", "Future Worth ($US)"))


# Round the numeric values to 2 decimal places
cols <- "Future Worth ($US)"

for (col in cols) {

idx <- which(!is.na(FA[[col]]))

data.table::set(FA, i = idx, j = col, value = round_r3(FA[[col]][idx], d = 2))

}



col.names <- c("n (periods)", "Future Worth ($US)")

# code block below modified from data.table function
setattr(FA, "col.names", setnames(FA, col.names))
setattr(FA, "class", c("data.table", "data.frame"))
FA

}
}
