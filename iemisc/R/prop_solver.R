#' Proportion Solver
#'
#' Solve the proportion for the missing numeric value in either of the
#' numerators or denominators
#'
#'
#' @param n1 numeric vector that contains the numerator 1
#' @param n2 numeric vector that contains the numerator 2
#' @param d1 numeric vector that contains the denominator 1
#' @param d2 numeric vector that contains the denominator 2
#' @param output character vector that contains the output type, (all or
#'     single)
#'
#'
#' @return the missing proportion value as either a single numeric value or all
#'     values as characters
#'
#'
#'
#' @source
#' \enumerate{
#'    \item Basic Mathematics. "Solving Proportions", \url{https://www.basic-mathematics.com/solving-proportions.html}
#'    \item Dr. Ariyana Love. "SMasks And Covid Tests Contain Nanotech Vaccines Without Informed Consent", April 7, 2021, \url{https://ambassadorlove.blog/2021/04/07/masks-and-covid-tests-contain-nanotech-vaccines-without-informed-consent/}
#'    \item "Electronic Support for Public Health--Vaccine Adverse Event Reporting System (ESP:VAERS) Grant Final Report"/Grant ID: R18 HS 017045 [Inclusive dates: 12/01/07 - 09/30/10]. Principal Investigator: Lazarus, Ross, MBBS, MPH, MMed, GDCompSci., page 6, \url{https://web.archive.org/web/20211230233658/https://www.nvic.org/CMSTemplates/NVIC/Pdf/FDA/ahrq-vaers-report-2011.pdf}. Retrieved thanks to the Internet Archive: Wayback Machine
#' }
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
#' @examples
#'
#' # Example 1 from the Example # 1 from Reference 1
#'
#' library(iemisc)
#'
#' # 5 / x = 10 / 16
#'
#' prop_solver(n1 = 5, n2 = 10, d2 = 16, output = "single")
#'
#'
#'
#'
#' # Example 2
#'
#' library(iemisc)
#'
#' t1 <- "34 3 1/2"
#' t2 <- 5
#' t3 <- 5 / 2
#'
#' t11 <- construction_decimal(t1, result = "traditional", output = "vector")
#'
#' prop_solver(n1 = t11, n2 = 5, d1 = 5 / 2, output = "all")
#'
#'
#'
#'
#' # Example 3
#'
#' library(iemisc)
#'
#' # Refer to Reference 2 and Reference 3
#'
#' # What is the numerator (n1) for the situation where VAERS reports 4,576 dead
#' # people; however, the number of dead people is closer to 453,024 people?
#'
#' d1 <- 100 / 100 # 100%
#' n2 <- 4576 # number of deceased people
#' d2 <- 453024 # number of deceased people
#'
#' prop_solver(d1 = d1, n2 = n2, d2 = d2, output = "single")
#'
#'
#' # What is the more accurate number of dead people (d2) where VAERS reports 4,576
#' # dead people and we recognize that less than 1% of adverse reactions are reported
#' # to VAERS?
#'
#' n1 <- 0.99999999999999999999999999999999999999999999999 / 100 # less than 1%
#' n11 <- 0.98 / 100 # less than 1%
#' d1 <- 100 / 100 # number of deceased people
#' n2 <- 4576 # number of deceased people
#'
#' prop_solver(n1 = n1, d1 = d1, n2 = n2, output = "all")
#'
#' prop_solver(n1 = n1, d1 = d1, n2 = n2, output = "single")
#'
#'
#' prop_solver(n1 = n11, d1 = d1, n2 = n2, output = "all")
#'
#' prop_solver(n1 = n11, d1 = d1, n2 = n2, output = "single")
#'
#'
#'
#'
#'
#'
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#' @export
prop_solver <- function(n1 = NULL, d1 = NULL, n2 = NULL, d2 = NULL, output = c("single", "all")) {


checks <- c(n1, d1, n2, d2)

output <- output

# Check
assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either n1, d1, n2, or d2 is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails


if (missing(n1)) {

n1 <- (n2 * d1) / d2


if (output == "single") {

return(n1)

} else if (output == "all") {

eval(paste(n1, "/", d1, " = ", n2, "/", d2))

}

} else if (missing(d1)) {

d1 <- (n1 * d2) / n2


if (output == "single") {

return(d1)

} else if (output == "all") {

eval(paste(n1, "/", d1, " = ", n2, "/", d2))

}


} else if (missing(n2)) {

n2 <- (n1 * d2) / d1


if (output == "single") {

return(n2)


} else if (output == "all") {

eval(paste(n1, "/", d1, " = ", n2, "/", d2))

}

} else if (missing(d2)) {


d2 <- (n2 * d1) / n1


if (output == "single") {

return(d2)

} else if (output == "all") {

eval(paste(n1, "/", d1, " = ", n2, "/", d2))

}

}
}
