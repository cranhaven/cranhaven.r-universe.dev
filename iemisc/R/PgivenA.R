#' Present value given Annual value (Engineering Economics)
#'
#' Compute P given A
#'
#' P is expressed as
#'
#' 	\deqn{P = A\left[\frac{\left(1 + i\right)^n - 1}{i\left(1 + i\right)^n}\right]}
#'
#' \describe{
#'	\item{\emph{P}}{the "present equivalent"}
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
#' @return PgivenA numeric vector that contains the present value(s) rounded
#'    to 2 decimal places
#' @return PA data.table of both n (0 to n) and the resulting present values
#'    rounded to 2 decimal places
#'
#'
#'
#' @source
#' \enumerate{
#'    \item r - Convert column classes in data.table - Stack Overflow answered by Matt Dowle on Dec 27 2013. See https://stackoverflow.com/questions/7813578/convert-column-classes-in-data-table.
#'    \item r - foreach loop not replicating traditional loop - Stack Overflow answered by F. Privé on Oct 19 2019. See https://stackoverflow.com/questions/58459665/r-foreach-loop-not-replicating-traditional-loop.
#' }
#'
#'
#' @references
#' \enumerate{
#'    \item William G. Sullivan, Elin M. Wicks, and C. Patrick Koelling, \emph{Engineering Economy}, Fourteenth Edition, Upper Saddle River, New Jersey: Pearson/Prentice Hall, 2009, page 133-134, 142, 164.
#'    \item Dave Bruns, Exceljet: "Calculate original loan amount", \url{https://exceljet.net/formulas/calculate-original-loan-amount}.
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
#' # Example 1 -- Example 4-9 from the Sullivan Reference text (page 133-134)
#' PgivenA(A = 20000, n = 5, i = 15, frequency = "annual") # the interest rate is 15%
#'
#' PA(20000, 5, 15, "annual") # the interest rate is 15%
#'
#'
#' # Example 2
#'
#' PgivenA(A = 93.22, n = 5, i = 4.50, frequency = "month")
#'
#' # Using LibreOffice Calc 6.1.5.2 version
#' # A1 4.50%
#' # A2 -93.22
#' # A3 60
#' # A4 12
#' # A5 =PV(A1/A4,A3,A2) = $5,000.26
#'
#'
#'
#'
#'
#' @importFrom data.table data.table setnames setattr rbindlist
#' @importFrom foreach foreach %do%
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom round round_r3
#'
#' @name PgivenA
NULL

#' @export
#' @rdname PgivenA
PgivenA <- function (A, n, i, frequency = c("annual", "semiannual", "quarter", "bimonth", "month", "daily")) {

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

PgivenA <- A * (((1 + i) ^ n - 1) / (i * ((1 + i) ^ n)))

return(round_r3(PgivenA, d = 2))

} else if (fr == "semiannual") {

fr <- 2
n <- n * fr

i <- i / fr

PgivenA <- A * (((1 + i) ^ n - 1) / (i * ((1 + i) ^ n)))

return(round_r3(PgivenA, d = 2))

} else if (fr == "quarter") {

fr <- 4
n <- n * fr

i <- i / fr

PgivenA <- A * (((1 + i) ^ n - 1) / (i * ((1 + i) ^ n)))

return(round_r3(PgivenA, d = 2))

} else if (fr == "bimonth") {

fr <- 6
n <- n * fr

i <- i / fr

PgivenA <- A * (((1 + i) ^ n - 1) / (i * ((1 + i) ^ n)))

return(round_r3(PgivenA, d = 2))

} else if (fr == "month") {

fr <- 12
n <- n * fr

i <- i / fr

PgivenA <- A * (((1 + i) ^ n - 1) / (i * ((1 + i) ^ n)))

return(round_r3(PgivenA, d = 2))

} else if (fr == "daily") {

fr <- 365
n <- n * fr

i <- i / fr

PgivenA <- A * (((1 + i) ^ n - 1) / (i * ((1 + i) ^ n)))

return(round_r3(PgivenA, d = 2))

}
}



#' @export
#' @rdname PgivenA
PA <- function (A, n, i, frequency = c("annual", "semiannual", "quarter", "bimonth", "month", "daily")) {

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


# create list using foreach loop
# Source 2 begins
PA <- foreach(n = seq(n), .combine = 'c') %do% {

A * (((1 + i) ^ n - 1) / (i * ((1 + i) ^ n)))

}
# Source 2 ends


PA <- data.table(seq(n), PA)

P0 <- NA
P0 <- data.table(0, P0)
P0 <- setnames(P0, 2, "PA")

Plist <- list(P0, PA)

PA <- rbindlist(Plist)

setnames(PA, c("n (periods)", "Present Worth ($US)"))

PAprint <- copy(PA)


change_class <- "Present Worth ($US)"

# Source 1 begin
# round numbers to 2 decimal places
for (col in change_class) {

idx <- which(!is.na(PAprint[[col]]))

data.table::set(PAprint, i = idx, j = col, value = round_r3(PAprint[[col]][idx], d = 2))
}

# Source 1 end


col.names <- c("n (periods)", "Present Worth ($US)")

# code block below modified from data.table function
setattr(PAprint, "col.names", setnames(PAprint, col.names))
setattr(PAprint, "class", c("data.table", "data.frame"))
PAprint


} else if (fr == "semiannual") {

fr <- 2
n <- n * fr

i <- i / fr


# create list using foreach loop
# Source 2 begins
PA <- foreach(n = seq(n), .combine = 'c') %do% {

A * (((1 + i) ^ n - 1) / (i * ((1 + i) ^ n)))

}
# Source 2 ends


PA <- data.table(seq(n), PA)

P0 <- NA
P0 <- data.table(0, P0)
P0 <- setnames(P0, 2, "PA")

Plist <- list(P0, PA)

PA <- rbindlist(Plist)

setnames(PA, c("n (periods)", "Present Worth ($US)"))

PAprint <- copy(PA)


change_class <- "Present Worth ($US)"

# Source 1 begin
# round numbers to 2 decimal places
for (col in change_class) {

idx <- which(!is.na(PAprint[[col]]))

data.table::set(PAprint, i = idx, j = col, value = round_r3(PAprint[[col]][idx], d = 2))
}

# Source 1 end


col.names <- c("n (periods)", "Present Worth ($US)")

# code block below modified from data.table function
setattr(PAprint, "col.names", setnames(PAprint, col.names))
setattr(PAprint, "class", c("data.table", "data.frame"))
PAprint


} else if (fr == "quarter") {

fr <- 4
n <- n * fr

i <- i / fr


# create list using foreach loop
# Source 2 begins
PA <- foreach(n = seq(n), .combine = 'c') %do% {

A * (((1 + i) ^ n - 1) / (i * ((1 + i) ^ n)))

}
# Source 2 ends


PA <- data.table(seq(n), PA)

P0 <- NA
P0 <- data.table(0, P0)
P0 <- setnames(P0, 2, "PA")

Plist <- list(P0, PA)

PA <- rbindlist(Plist)

setnames(PA, c("n (periods)", "Present Worth ($US)"))

PAprint <- copy(PA)


change_class <- "Present Worth ($US)"

# Source 1 begin
# round numbers to 2 decimal places
for (col in change_class) {

idx <- which(!is.na(PAprint[[col]]))

data.table::set(PAprint, i = idx, j = col, value = round_r3(PAprint[[col]][idx], d = 2))
}

# Source 1 end


col.names <- c("n (periods)", "Present Worth ($US)")

# code block below modified from data.table function
setattr(PAprint, "col.names", setnames(PAprint, col.names))
setattr(PAprint, "class", c("data.table", "data.frame"))
PAprint


} else if (fr == "bimonth") {

fr <- 6
n <- n * fr

i <- i / fr


# create list using foreach loop
# Source 2 begins
PA <- foreach(n = seq(n), .combine = 'c') %do% {

A * (((1 + i) ^ n - 1) / (i * ((1 + i) ^ n)))

}
# Source 2 ends


PA <- data.table(seq(n), PA)

P0 <- NA
P0 <- data.table(0, P0)
P0 <- setnames(P0, 2, "PA")

Plist <- list(P0, PA)

PA <- rbindlist(Plist)

setnames(PA, c("n (periods)", "Present Worth ($US)"))

PAprint <- copy(PA)


change_class <- "Present Worth ($US)"

# Source 1 begin
# round numbers to 2 decimal places
for (col in change_class) {

idx <- which(!is.na(PAprint[[col]]))

data.table::set(PAprint, i = idx, j = col, value = round_r3(PAprint[[col]][idx], d = 2))
}

# Source 1 end


col.names <- c("n (periods)", "Present Worth ($US)")

# code block below modified from data.table function
setattr(PAprint, "col.names", setnames(PAprint, col.names))
setattr(PAprint, "class", c("data.table", "data.frame"))
PAprint



} else if (fr == "month") {

fr <- 12
n <- n * fr

i <- i / fr

# create list using foreach loop
# Source 2 begins
PA <- foreach(n = seq(n), .combine = 'c') %do% {

A * (((1 + i) ^ n - 1) / (i * ((1 + i) ^ n)))

}
# Source 2 ends


PA <- data.table(seq(n), PA)

P0 <- NA
P0 <- data.table(0, P0)
P0 <- setnames(P0, 2, "PA")

Plist <- list(P0, PA)

PA <- rbindlist(Plist)

setnames(PA, c("n (periods)", "Present Worth ($US)"))

PAprint <- copy(PA)


change_class <- "Present Worth ($US)"

# Source 1 begin
# round numbers to 2 decimal places
for (col in change_class) {

idx <- which(!is.na(PAprint[[col]]))

data.table::set(PAprint, i = idx, j = col, value = round_r3(PAprint[[col]][idx], d = 2))
}

# Source 1 end


col.names <- c("n (periods)", "Present Worth ($US)")

# code block below modified from data.table function
setattr(PAprint, "col.names", setnames(PAprint, col.names))
setattr(PAprint, "class", c("data.table", "data.frame"))
PAprint


} else if (fr == "daily") {

fr <- 365
n <- n * fr

i <- i / fr

# create list using foreach loop
# Source 2 begins
PA <- foreach(n = seq(n), .combine = 'c') %do% {

A * (((1 + i) ^ n - 1) / (i * ((1 + i) ^ n)))

}
# Source 2 ends


PA <- data.table(seq(n), PA)

P0 <- NA
P0 <- data.table(0, P0)
P0 <- setnames(P0, 2, "PA")

Plist <- list(P0, PA)

PA <- rbindlist(Plist)

setnames(PA, c("n (periods)", "Present Worth ($US)"))

PAprint <- copy(PA)


change_class <- "Present Worth ($US)"

# Source 1 begin
# round numbers to 2 decimal places
for (col in change_class) {

idx <- which(!is.na(PAprint[[col]]))

data.table::set(PAprint, i = idx, j = col, value = round_r3(PAprint[[col]][idx], d = 2))
}

# Source 1 end


col.names <- c("n (periods)", "Present Worth ($US)")

# code block below modified from data.table function
setattr(PAprint, "col.names", setnames(PAprint, col.names))
setattr(PAprint, "class", c("data.table", "data.frame"))
PAprint

}
}
