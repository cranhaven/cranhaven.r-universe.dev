#' Present value given Future value (Engineering Economics)
#'
#' Compute P given F
#'
#' P is expressed as
#'
#' 	\deqn{P = F\left[\frac{1}{\left(1 + i\right)^n}\right]}
#'
#' \describe{
#'	\item{\emph{P}}{the "present equivalent"}
#'	\item{\emph{F}}{the "future equivalent"}
#'	\item{\emph{i}}{the "effective interest rate per interest period"}
#'	\item{\emph{n}}{the "number of interest periods"}
#' }
#'
#'
#' @param F numeric vector that contains the future value(s)
#' @param n numeric vector that contains the period value(s)
#' @param i numeric vector that contains the interest rate(s) as a percent
#' @param frequency character vector that contains the frequency used to
#'    obtain the number of periods [annual (1), semiannual (2), quarter (4),
#'    bimonth (6), month (12), daily (365)]
#'
#' @return PgivenF numeric vector that contains the present value(s) rounded
#'    to 2 decimal places
#' @return PF data.frame of both n (0 to n) and the resulting present values
#'    rounded to 2 decimal places
#'
#'
#'
#' @references
#' William G. Sullivan, Elin M. Wicks, and C. Patrick Koelling, \emph{Engineering Economy}, Fourteenth Edition, Upper Saddle River, New Jersey: Pearson/Prentice Hall, 2009, page 128, 142, 164.
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
#' # Example 4-4 from the Reference text (page 128)
#' PgivenF(10000, 6, 8, "annual") # the interest rate is 8%
#'
#' PF(10000, 6, 8, "annual") # the interest rate is 8%
#'
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom data.table data.table setnames setDF
#' @importFrom round round_r3
#'
#' @name PgivenF
NULL

#' @export
#' @rdname PgivenF
PgivenF <- function (F, n, i, frequency = c("annual", "semiannual", "quarter", "bimonth", "month", "daily")) {

frequency <- frequency

checks <- c(F, n, i)

# Check
assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either F, n, or i is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
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

PgivenF <- F * ((1 / (1 + i)) ^ n)

return(round_r3(PgivenF, d = 2))

} else if (fr == "semiannual") {

fr <- 2
n <- n * fr

i <- i / fr

PgivenF <- F * ((1 / (1 + i)) ^ n)

return(round_r3(PgivenF, d = 2))

} else if (fr == "quarter") {

fr <- 4
n <- n * fr

i <- i / fr

PgivenF <- F * ((1 / (1 + i)) ^ n)

return(round_r3(PgivenF, d = 2))

} else if (fr == "bimonth") {

fr <- 6
n <- n * fr

i <- i / fr

PgivenF <- F * ((1 / (1 + i)) ^ n)

return(round_r3(PgivenF, d = 2))

} else if (fr == "month") {

fr <- 12
n <- n * fr

i <- i / fr

PgivenF <- F * ((1 / (1 + i)) ^ n)

return(round_r3(PgivenF, d = 2))

} else if (fr == "daily") {

fr <- 365
n <- n * fr

i <- i / fr

PgivenF <- F * ((1 / (1 + i)) ^ n)

return(round_r3(PgivenF, d = 2))

}
}




#' @export
#' @rdname PgivenF
PF <- function (F, n, i, frequency = c("annual", "semiannual", "quarter", "bimonth", "month", "daily")) {


frequency <- frequency

checks <- c(F, n, i)

# Check
assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either F, n, or i is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
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

PF <- vector("list", length(1:n))
## Source 1 and 2 / pre-allocate the list since it is being used in a for loop

for (y in 1:length(n)) {
PF[[y]] <- F * ((1 / (1 + i)) ^ seq(n))
}

PF <- data.table(seq(n), unlist(PF))

P0 <- F * ((1 / (1 + i)) ^ 0)
P0 <- data.table(0, P0)
P0 <- setnames(P0, 2, "V2")

PF <- rbind(P0, PF)

setnames(PF, c("n (periods)", "Present Worth ($US)"))


# Round the numeric values to 2 decimal places
cols <- "Present Worth ($US)"

for (col in cols) {

idx <- which(!is.na(PF[[col]]))

data.table::set(PF, i = idx, j = col, value = round_r3(PF[[col]][idx], d = 2))

}



col.names <- c("n (periods)", "Present Worth ($US)")

# code block below modified from data.table function
setattr(PF, "col.names", setnames(PF, col.names))
setattr(PF, "class", c("data.table", "data.frame"))
PF

} else if (fr == "semiannual") {

fr <- 2
n <- n * fr

i <- i / fr

PF <- vector("list", length(1:n))
## Source 1 and 2 / pre-allocate the list since it is being used in a for loop

for (y in 1:length(n)) {
PF[[y]] <- F * ((1 / (1 + i)) ^ seq(n))
}

PF <- data.table(seq(n), unlist(PF))

P0 <- F * ((1 / (1 + i)) ^ 0)
P0 <- data.table(0, P0)
P0 <- setnames(P0, 2, "V2")

PF <- rbind(P0, PF)

setnames(PF, c("n (periods)", "Present Worth ($US)"))


# Round the numeric values to 2 decimal places
cols <- "Present Worth ($US)"

for (col in cols) {

idx <- which(!is.na(PF[[col]]))

data.table::set(PF, i = idx, j = col, value = round_r3(PF[[col]][idx], d = 2))

}



col.names <- c("n (periods)", "Present Worth ($US)")

# code block below modified from data.table function
setattr(PF, "col.names", setnames(PF, col.names))
setattr(PF, "class", c("data.table", "data.frame"))
PF

} else if (fr == "quarter") {

fr <- 4
n <- n * fr

i <- i / fr

PF <- vector("list", length(1:n))
## Source 1 and 2 / pre-allocate the list since it is being used in a for loop

for (y in 1:length(n)) {
PF[[y]] <- F * ((1 / (1 + i)) ^ seq(n))
}

PF <- data.table(seq(n), unlist(PF))

P0 <- F * ((1 / (1 + i)) ^ 0)
P0 <- data.table(0, P0)
P0 <- setnames(P0, 2, "V2")

PF <- rbind(P0, PF)

setnames(PF, c("n (periods)", "Present Worth ($US)"))


# Round the numeric values to 2 decimal places
cols <- "Present Worth ($US)"

for (col in cols) {

idx <- which(!is.na(PF[[col]]))

data.table::set(PF, i = idx, j = col, value = round_r3(PF[[col]][idx], d = 2))

}



col.names <- c("n (periods)", "Present Worth ($US)")

# code block below modified from data.table function
setattr(PF, "col.names", setnames(PF, col.names))
setattr(PF, "class", c("data.table", "data.frame"))
PF

} else if (fr == "bimonth") {

fr <- 6
n <- n * fr

i <- i / fr

PF <- vector("list", length(1:n))
## Source 1 and 2 / pre-allocate the list since it is being used in a for loop

for (y in 1:length(n)) {
PF[[y]] <- F * ((1 / (1 + i)) ^ seq(n))
}

PF <- data.table(seq(n), unlist(PF))

P0 <- F * ((1 / (1 + i)) ^ 0)
P0 <- data.table(0, P0)
P0 <- setnames(P0, 2, "V2")

PF <- rbind(P0, PF)

setnames(PF, c("n (periods)", "Present Worth ($US)"))


# Round the numeric values to 2 decimal places
cols <- "Present Worth ($US)"

for (col in cols) {

idx <- which(!is.na(PF[[col]]))

data.table::set(PF, i = idx, j = col, value = round_r3(PF[[col]][idx], d = 2))

}



col.names <- c("n (periods)", "Present Worth ($US)")

# code block below modified from data.table function
setattr(PF, "col.names", setnames(PF, col.names))
setattr(PF, "class", c("data.table", "data.frame"))
PF

} else if (fr == "month") {

fr <- 12
n <- n * fr

i <- i / fr

PF <- vector("list", length(1:n))
## Source 1 and 2 / pre-allocate the list since it is being used in a for loop

for (y in 1:length(n)) {
PF[[y]] <- F * ((1 / (1 + i)) ^ seq(n))
}

PF <- data.table(seq(n), unlist(PF))

P0 <- F * ((1 / (1 + i)) ^ 0)
P0 <- data.table(0, P0)
P0 <- setnames(P0, 2, "V2")

PF <- rbind(P0, PF)

setnames(PF, c("n (periods)", "Present Worth ($US)"))


# Round the numeric values to 2 decimal places
cols <- "Present Worth ($US)"

for (col in cols) {

idx <- which(!is.na(PF[[col]]))

data.table::set(PF, i = idx, j = col, value = round_r3(PF[[col]][idx], d = 2))

}



col.names <- c("n (periods)", "Present Worth ($US)")

# code block below modified from data.table function
setattr(PF, "col.names", setnames(PF, col.names))
setattr(PF, "class", c("data.table", "data.frame"))
PF

} else if (fr == "daily") {

fr <- 365
n <- n * fr

i <- i / fr

PF <- vector("list", length(1:n))
## Source 1 and 2 / pre-allocate the list since it is being used in a for loop

for (y in 1:length(n)) {
PF[[y]] <- F * ((1 / (1 + i)) ^ seq(n))
}

PF <- data.table(seq(n), unlist(PF))

P0 <- F * ((1 / (1 + i)) ^ 0)
P0 <- data.table(0, P0)
P0 <- setnames(P0, 2, "V2")

PF <- rbind(P0, PF)

setnames(PF, c("n (periods)", "Present Worth ($US)"))


# Round the numeric values to 2 decimal places
cols <- "Present Worth ($US)"

for (col in cols) {

idx <- which(!is.na(PF[[col]]))

data.table::set(PF, i = idx, j = col, value = round_r3(PF[[col]][idx], d = 2))

}



col.names <- c("n (periods)", "Present Worth ($US)")

# code block below modified from data.table function
setattr(PF, "col.names", setnames(PF, col.names))
setattr(PF, "class", c("data.table", "data.frame"))
PF

}
}
