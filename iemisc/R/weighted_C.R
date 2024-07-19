#' Calculate the Weighted C factor
#'
#' This function computes the weighted C factor using the user-supplied unit
#' or the default unit of an acre for use in the Rational Formula.
#' 
#' @param C numeric vector containing dimensionless C factor(s)
#' @param area numeric vector containing the surface land area
#' @param area_pct numeric vector containing the surface land area, as a
#'    percent (decimal or whole number)
#' @param area_units character vector containing the units for area
#'    (default = "acre"). The other possible units are "square feet",
#'    "square mile", "hectare", or "square kilometer". The units should
#'    be consistent and not mixed.
#' @param C_area_table data.frame/data.table/tibble, list, or matrix
#'    containing the C in column 1 and the area in column 2
#' @param C_area_pct_table data.frame/data.table/tibble, list, or matrix
#'    containing the C in column 1 and the area_pct in column 2
#'
#' @return Weighted C factor as a single numeric vector, in the range [0, 1]
#'
#'
#'
#'
#'
#'
#'
#' @references
#' Engineering Hydrology Training Series Module 104 - Runoff Curve Number Computations Study Guide, United States Department of Agriculture Soil Conservation Service National Employee Development Staff, September 1989, page 21 \url{https://web.archive.org/web/20210414043852/https://www.wcc.nrcs.usda.gov/ftpref/wntsc/H&H/training/runoff-curve-numbers1.pdf}. Retrieved thanks to the Internet Archive: Wayback Machine
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
#' @examples
#'
#' # Note: the default area unit is acre
#'
#' # Example 1
#'
#' library(iemisc)
#'
#' area1 <- c(220, 150, 30)
#' C1 <- c(75, 89, 80)
#' weighted_C(C = C1, area = area1)
#'
#'
#' # Example 2
#'
#' library(iemisc)
#'
#' area2 <- c(220, 150, 30)
#' area_pct2 <- area2 / sum(area2)
#' C2 <- c(80, 95, 80)
#' C_area_pct_table2 <- data.frame(C2, area_pct2)
#' weighted_C(C_area_pct_table = C_area_pct_table2)
#'
#'
#' # Example 3
#'
#' install.load::load_package("iemisc", "data.table")
#'
#' C_area_table3 <- data.table(C = c(98, 100, 45), area = c(2.53, 453.00, 0.21))
#' weighted_C(C_area_table = C_area_table3)
#'
#'
#' # Example 4
#'
#' library(iemisc)
#'
#' C4 <- c(98, 100, 45)
#' area_pct4 <- c(0.15, 0.23, 0.62)
#' weighted_C(C = C4, area_pct = area_pct4)
#'
#'
#' # Example 5
#'
#' library(iemisc)
#'
#' data_matrix5a <- matrix(c(98, 30, 40, 43, 57, 3.24, 1, 30, 50, 123), nrow = 5,
#' ncol = 2, dimnames = list(rep("", 5), c("C", "Area")))
#' weighted_C(C_area_table = data_matrix5a)
#'
#'
#' # using ramify to create the matrix
#'
#' import::from(ramify, mat)
#'
#' data_matrix5b <- mat("98 30 40 43 57;3.24 1 30 50 123", rows = FALSE, sep = " ",
#' dimnames = list(rep("", 5), c("C", "Area")))
#' weighted_C(C_area_table = data_matrix5b)
#'
#'
#' # Example 6 - using area in square feet
#'
#' library(iemisc)
#'
#' data_list6 <- list(C = c(77, 29, 68), Area = c(43560, 56893, 345329.32))
#' weighted_C(C_area_table = data_list6, area_units = "square feet")
#'
#'
#' # Example 7
#'
#' install.load::load_package("iemisc", "data.table")
#'
#' # Impervious area - 3.04 acre
#' # 45% of total area
#' # 0.80 C Factor
#'
#' # Pervious area - 4.67 acre
#' # 55% of total area
#' # 0.20 C factor
#'
#' C_area_table7 <- data.table(C = c(0.80, 0.20), area = c(3.04, 4.67))
#' weighted_C(C_area_table = C_area_table7)
#'
#'
#' # Example 8
#' 
#' # Impervious area - 2.44 acre
#' # 32% of total area
#' # 0.80 C Factor
#'
#' # Pervious area - 5.03 acre
#' # 68% of total area
#' # 0.20 C factor
#'
#' C8 <- c(0.80, 0.20)
#' area_pct8 <- c(0.32, 0.68)
#' weighted_C(C = C8, area_pct = area_pct8)
#'
#'
#' # Example 9
#' 
#' library(iemisc)
#'
#' # Medium density residential area - 30 hectares (75.0% of total area),
#' # 0.31 - 0.40 C factor
#' # High density residential area - 3 hectares (7.50% of total area),
#' # 0.49 - 0.60 C factor
#' # Agricultural area - 7 hectares (17.5% of total area), 0.15 - 0.21 C factor
#'
#' C3 <- c(mean(seq(0.31, 0.40, by = 0.01)), mean(seq(0.49, 0.60, by = 0.01)),
#' mean(seq(0.15, 0.21, by = 0.01)))
#' area3 <- c(30, 3, 7)
#' weighted_C(C = C3, area = area3, area_units = "hectare")
#'
#'
#'
#'
#'
#'
#' @importFrom data.table as.data.table
#' @importFrom units set_units make_units drop_units
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest testDataTable
#' @importFrom stringi stri_replace_all_fixed
#' @importFrom fpCompare %==%
#' @importFrom round round_r3
#'
#' @export
weighted_C <- function (C = NULL, area = NULL, area_pct = NULL, area_units = c("acre", "square feet", "square mile", "hectare", "square kilometer"), C_area_table = NULL, C_area_pct_table = NULL) {


ft <- acre <- mi <- hectare <- km <- NULL
# due to NSE notes in R CMD check


if (missing(C) & missing(area) & missing(area_pct) & missing(C_area_pct_table)) {

# convert data.frame/data.table/tibble, list, matrix to data.table & then to numeric vector
# assume column 1 is C and column 2 is area

C_area_table <- as.data.table(C_area_table)

assert_that(testDataTable(C_area_table, types = "numeric", any.missing = FALSE, all.missing = FALSE, min.rows = 1, min.cols = 1, ncols = 2), msg = "Any row of C_area_table contains a value that is NA, NaN, empty, or a string. Please try again.")
# only process enough known C factor values and provide a stop warning if not enough

C <- C_area_table[, 1][[1]]

area <- C_area_table[, 2][[1]]


} else if (missing(C) & missing(area) & missing(area_pct) & missing(C_area_table)) {

# convert data.frame/data.table/tibble, list, matrix to data.table & then to numeric vector
# assume column 1 is C and column 2 is area_pct

C_area_pct_table <- as.data.table(C_area_pct_table)

assert_that(testDataTable(C_area_pct_table, types = "numeric", any.missing = FALSE, all.missing = FALSE, min.rows = 1, min.cols = 1, ncols = 2), msg = "Any row of C_area_pct_table contains a value that is NA, NaN, empty, or a string. Please try again.")
# only process enough known C factor values and provide a stop warning if not enough

C <- C_area_pct_table[, 1][[1]]

area_pct <- C_area_pct_table[, 2][[1]]

}


checks <- c(C, area, area_pct)

area_units <- area_units


# Check for checks
assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either C, area, or area_pct is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

# Check for C
assert_that(!any(qtest(C, "N>=2(0,)") == FALSE), msg = "C is 0, NA, NaN, Inf, -Inf, empty, or a string. Or, there are not at least 2 C factor values. Please try again.")
# only process enough known C factor values and provide a stop warning if not enough


area <- as.numeric(stri_replace_all_fixed(area, ",", ""))

ifelse(length(area_units) > 1, area_units <- "acre", area_units <- area_units)

ifelse(missing(area_units), area_units <- "acre", area_units <- area_units)



# check for area_units
assert_that(qtest(area_units, "S==1"), msg = "area_units should only be a single character vector. Please specify either 'acre', 'square feet', 'square mile', 'hectare', or 'square kilometer'. Please try again.")
# only process with a single string value and provide a stop warning if not

assert_that(isTRUE(any(c("acre", "square feet", "square mile", "hectare", "square kilometer") %in% area_units)), msg = "Incorrect unit selection. The only possible area_units are 'acre', 'square feet', 'square mile', 'hectare', and 'square kilometer'. Please try again.")
# only process with a specified unit and provide a stop warning if not
# Source 1




if (area_units == "acre") {

area <- area


} else if (area_units == "square feet") {

area <- set_units(area, ft^2) # ft^2

units(area) <- make_units(acre) # acres

area <- drop_units(area)


} else if (area_units == "square mile") {

area <- set_units(area, mi^2) # mi^2

units(area) <- make_units(acre) # acres

area <- drop_units(area)


} else if (area_units == "hectare") {

area <- set_units(area, hectare) # hectare

units(area) <- make_units(acre) # acres

area <- drop_units(area)


} else if (area_units == "square kilometer") {

area <- set_units(area, km^2) # km^2

units(area) <- make_units(acre) # acres

area <- drop_units(area)

}


if (missing(area_pct)) {

weighted_C <- round_r3(sum(C * area) / sum(area), d = 2)

return(weighted_C)


} else if (!missing(area_pct)) {

ifelse(area_pct < 1, area_pct_type <- "decimal", area_pct_type <- "whole")

if(area_pct_type == "decimal") {

ifelse(sum(area_pct) %==% 1, weighted_C <- weighted_C, stop("The area sum does not equal 100%."))
# Source 1 / only process enough known variables and provide a stop warning if not enough

weighted_C <- round_r3(sum(C * area_pct) / sum(area_pct), d = 2)

return(weighted_C)


} else if(area_pct_type == "whole") {

ifelse(sum(area_pct) %==% 100, weighted_C <- weighted_C, stop("The area sum does not equal 100%."))
# Source 1 / only process enough known variables and provide a stop warning if not enough

weighted_C <- round_r3(sum(C * area_pct) / sum(area_pct), d = 2)

return(weighted_C)
}
}
}
