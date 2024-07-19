#' Construction Decimal
#'
#' Convert a construction measurement in US Customary Units (foot + inch) with
#' or without a fraction into its equivalent as a decimal
#'
#'
#' @param measurement character vector that contains the construction
#'     measurement (foot + inch)
#' @param result character vector that contains the decimal type [options are
#'     traditional (ex. 1.203125 = 1'-2 7/16\"] where the whole number is the
#'     value in ft and the decimal is the value in inches & librecad
#'     (ex. 14.43112 = 1'-2 7/16\"), whereby LibreCAD defines its decimal unit as
#'     "integer part separated from the fractional part of a number by a
#'     decimal". Thus, both the whole number and the decimal is the value in
#'     inches.
#' @param output character vector that contains the type of output. The
#'     options are vector (just the single value as a decimal) and table [the
#'     decimal value in inch (in), feet (ft), yard (yd), millimeters (mm),
#'     centimeters (cm), and meters (m)].
#'
#'
#'
#' @return the construction measurement value as a numeric \code{\link[base]{vector}}
#'     as a decimal or as a table (depends on the output parameters)
#'     
#'
#' @note
#' If you only have a measurement in inches, then use \code{\link{frac_to_numeric}}
#' instead.
#' 
#'
#'
#'
#' @source
#' \enumerate{
#'    \item removing all non-numeric characters from a string, but not "." - R help on nabble.com answered by David Winsemius on Jul 26, 2016. See \url{https://web.archive.org/web/20190730141421/http://r.789695.n4.nabble.com/removing-all-non-numeric-characters-from-a-string-but-not-quot-quot-td4723146.html}. Retrieved thanks to the Internet Archive: Wayback Machine
#'    \item r - How to not run an example using roxygen2? - Stack Overflow answered and edited by samkart on Jul 9 2017. (Also see the additional comments in response to the answer.) See \url{https://stackoverflow.com/questions/12038160/how-to-not-run-an-example-using-roxygen2}.
#'    \item devtools - Issues in R package after CRAN asked to replace dontrun by donttest - Stack Overflow answered by Hong Ooi on Sep 1 2020. (Also see the additional comments in response to the answer.) See \url{https://stackoverflow.com/questions/63693563/issues-in-r-package-after-cran-asked-to-replace-dontrun-by-donttest}.
#'    \item regex - Replace single backslash in R - Stack Overflow answered and edited by Hong Ooi on Aug 21, 2014. (Also see the additional comments in response to the answer.) See \url{https://stackoverflow.com/questions/25424382/replace-single-backslash-in-r}.
#' }
#'
#'
#'
#'
#' @references
#' \enumerate{
#'    \item LibreCAD v2.2.0 - User Manual - Fundamentals: Units, 7 May 2022, \url{https://librecad-docs-dev.readthedocs.io/en/latest/ref/fundamentals.html#units}.
#'    \item Spike, 1 January 2022, "Foot and Inch to Decimal Format Conversion", \url{https://www.spikevm.com/calculators/fraction-decimal-calculators.php}.
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
#' @examples
#' 
#' # Please refer to the iemisc: Construction Measurement Examples vignette for
#' # additional examples
#' 
#' # Example 1
#'
#' library(iemisc)
#' 
#' construction_decimal("2'-0\"", result = "traditional", output = "vector")
#'
#' construction_decimal("1'-2 7/16\"", result = "librecad", output = "vector")
#'
#'
#'
#' # Example 2
#'
#' library(iemisc)
#' 
#' construction_decimal("0 6", result = "traditional", output = "vector")
#' # read as 0 feet 6 inches
#' 
#' construction_decimal("0 6", result = "librecad", output = "vector")
#' # read as 0 feet 6 inches
#'
#'
#' # Example 3
#'
#' library(iemisc)
#' 
#' tss1 <- "48'-0 1/2\""
#' tss2 <- "56-9 1/2\""
#' 
#' sum(construction_decimal(tss1, result = "traditional", output = "vector"),
#' construction_decimal(tss2, result = "traditional", output = "vector"))
#'
#'
#' \donttest{
#' # See Source 2 and Source 3
#'
#' # Example 4
#'
#' library(iemisc)
#' 
#' try(construction_decimal(5, result = "traditional", output =
#' "vector")) # please see the error message
#'
#' ex_error <- character(0)
#' try(construction_decimal(ex_error, result = "traditional",
#' output = "vector")) # please see the error message
#'
#' try(construction_decimal(NA, result = "traditional", output =
#' "vector")) # please see the error message
#'
#' try(construction_decimal("feet", result = "traditional", output =
#' "vector")) # please see the error message
#' }
#'
#'
#'
#' # Example 5
#'
#' library(iemisc)
#'
#' app1 <- "5' 2\""
#'
#' app2 <- "6' 3\""
#'
#' app3 <- construction_decimal(app1, result = "traditional", output = "vector") *
#' construction_decimal(app2, result = "traditional", output = "vector")
#' app3
#'
#' # If you want to have the fractional value using 16ths, do the following:
#'
#' construction_fraction(app3, type = "traditional", result = "traditional",
#' fraction = 16)
#'
#'
#'
#'
#'
#' @importFrom stringi stri_detect_fixed stri_detect_regex stri_replace_all_fixed stri_trim_both
#' @importFrom units set_units make_units drop_units
#' @importFrom data.table setnames setattr
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom fpCompare %==%
#' @importFrom mgsub mgsub
#'
#' @export
construction_decimal <- function (measurement, result = c("traditional", "librecad"), output = c("vector", "table")) {


inch <- yd <- mm <- cm <- m <- ft <- NULL
# due to NSE notes in R CMD check


result <- result

output <- output


# Check
assert_that(qtest(result, "S==1"), msg = "There is not a result type or more than 1 result type. Please specify either 'traditional' or 'librecad'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("traditional", "librecad") %in% result)), msg = "The result type has not been identified correctly as either 'traditional' or 'librecad'. Please try again.")
# only process with a specified result and provide a stop warning if not

assert_that(qtest(output, "S==1"), msg = "There is not a output or more than 1 output. Please specify either 'vector' or 'table'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("vector", "table") %in% output)), msg = "The output has not been identified correctly as either 'vector' or 'table'. Please try again.")
# only process with a specified output and provide a stop warning if not

assert_that(!any(testString(measurement, min.chars = 1, pattern = "[0-9]", ignore.case = TRUE) == FALSE), msg = "measurement is a numeric vector or a character vector without any numeric values. measurement should be a character vector that contains numeric values. Please try again.")
# only process with string values with numbers and provide an error message if the check fails


if (result == "traditional") {

# remove all non-numeric characters at once
change1 <- stri_trim_both(mgsub(measurement, c("-", "[A-Za-z]", "\"", "'"), c(" ", "", "", " ")))

assert_that(all(stri_detect_regex(change1, "[0-9]")), msg = "measurement does not include numeric values in all parts. Please try again.")
# only process with a numeric value present in all parts and provide a stop warning if change1 does not include a numeric value in all parts (measurement in error message since only that parameter has been defined)


# split the string at 2 spaces to have 2 parts
change2 <- stri_split_fixed(change1, " ", n = 2, omit_empty = TRUE)

# trim the left and right white space
change3 <- lapply(change2, stri_trim_both)

# change to a numeric vector
change3_part1 <- as.numeric(change3[[1]][1])

# get the 2nd part of change3
change3_part2 <- change3[[1]][2]

# convert to a numeric vector
change4 <- frac_to_numeric(change3_part2)

# divide by 12
change4_use <- change4 / 12

# get the sum (the final numeric vector)
change <- sum(change3_part1, change4_use)


if (output == "vector") {

return(change)


} else if (output == "table") {

measure_ft <- set_units(change, "ft")

measure_in <- measure_ft

measure_yd <- measure_ft

measure_mm <- measure_ft

measure_cm <- measure_ft

measure_m <- measure_ft

units(measure_in) <- make_units(inch)

units(measure_yd) <- make_units(yd)

units(measure_mm) <- make_units(mm)

units(measure_cm) <- make_units(cm)

units(measure_m) <- make_units(m)


# create data.table for displaying the results
result_table <- data.table(Measurement = c(drop_units(measure_in), drop_units(measure_ft), drop_units(measure_yd), drop_units(measure_mm), drop_units(measure_cm), drop_units(measure_m)), Units = c("in", "ft", "yd", "mm", "cm", "m"))
setnames(result_table, c("Measurement", "Units"))

col.names <- c("Measurement", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table

}


} else if (result == "librecad") {

# remove all non-numeric characters at once
change1 <- stri_trim_both(mgsub(measurement, c("-", "[A-Za-z]", "\"", "'"), c(" ", "", "", " ")))

assert_that(all(stri_detect_regex(change1, "[0-9]")), msg = "measurement does not include numeric values in all parts. Please try again.")
# only process with a numeric value present in all parts and provide a stop warning if change1 does not include a numeric value in all parts (measurement in error message since only that parameter has been defined)


# split the string at 2 spaces to have 2 parts
change2 <- stri_split_fixed(change1, " ", n = 2, omit_empty = TRUE)

# trim the left and right white space
change3 <- lapply(change2, stri_trim_both)

# change to a numeric vector
change3_part1 <- as.numeric(change3[[1]][1])

# get the 2nd part of change3
change3_part2_pre <- change3[[1]][2]


# only 1 number is detected
if(stri_count_regex(change3_part2_pre, "\\d") == 1) {

ifelse(stri_detect_fixed(change3_part2_pre, "\""), change3_part2 <- gsub("\"", "", change3_part2_pre, fixed = TRUE), change3_part2 <- change3_part2_pre) # Source 5 for the gsub

change3_part2 <- as.numeric(change3_part2)

change <- sum(change3_part1 * 12, change3_part2)


# more than 1 number is detected
} else if(stri_count_regex(change3_part2_pre, "\\d") != 1) {

ifelse(stri_detect_fixed(change3_part2_pre, "\""), change3_part2 <- gsub("\"", "", change3_part2_pre, fixed = TRUE), change3_part2 <- change3_part2_pre) # Source 5 for the gsub

change3_part3 <- unlist(strsplit(change3_part2, split = " ", fixed = TRUE))

change3_part3b <- unlist(stri_split_fixed(change3_part3[2], "/", n = 2))

change3_part3b_a <- as.numeric(change3_part3b[1])

change3_part3b_b <- as.numeric(change3_part3b[2])

change <- sum(change3_part1 * 12, as.numeric(change3_part3[1]), change3_part3b_a / change3_part3b_b)

}


if (output == "vector") {

return(change)


} else if (output == "table") {

measure_in <- set_units(change, "inch")

measure_ft <- measure_in

measure_yd <- measure_in

measure_mm <- measure_in

measure_cm <- measure_in

measure_m <- measure_in

units(measure_ft) <- make_units(ft)

units(measure_yd) <- make_units(yd)

units(measure_mm) <- make_units(mm)

units(measure_cm) <- make_units(cm)

units(measure_m) <- make_units(m)


# create data.table for displaying the results
result_table <- data.table(Measurement = c(drop_units(measure_in), drop_units(measure_ft), drop_units(measure_yd), drop_units(measure_mm), drop_units(measure_cm), drop_units(measure_m)), Units = c("in", "ft", "yd", "mm", "cm", "m"))
setnames(result_table, c("Measurement", "Units"))

col.names <- c("Measurement", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table

}

}
}













#' Construction Fraction
#'
#' Convert a construction measurement in US Customary Units as a decimal into
#' its nearest equivalent as foot + inch with or without a fraction
#'
#'
#' @param measurement numeric vector that contains the construction
#'     measurement as a decimal
#' @param type character vector that contains the decimal type [options are
#'   traditional (ex. 1.203125 = 1'-2 7/16\"] where the whole number is the
#'   value in ft and the decimal is the value in inches & librecad
#'   (ex. 14.4375 = 1'-2 7/16\"), whereby LibreCAD defines its decimal unit as
#'   "integer part separated from the fractional part of a number by a
#'   decimal". Thus, both the whole number and the decimal is the value in
#'   inches.
#' @param result character vector that contains the resulting fraction type
#'   (options are traditional (ex. 1.203125 = 1 ft 2 7/16 in) & inch
#'   (ex. 14.4375 = 14 7/16 in)). This is the same as fractional (fractional
#'   inch) in LibreCAD.
#' @param fraction numeric vector that contains the fractional part to return.
#'     The options are 0, 2, 4, 8, 16, 32, 64, 100, 128, or 256.
#'
#'
#'
#' @return the construction measurement value as a character \code{\link[base]{vector}} as
#'     foot + fraction of an inch or as inch + fraction of an inch (depends on
#'     the parameters)
#'     
#'
#'
#'
#'
#'
#'
#'
#'
#' @references
#' \enumerate{
#'    \item Spike, 7 May 2022, "How to Convert Feet in Decimal Format to Foot, Inch and Fraction Values", \url{https://www.spikevm.com/construction-math/convert-decimal-fraction.php}.
#'    \item myCarpentry, 7 May 2022, "Online Fraction Calculator", \url{https://www.mycarpentry.com/online-fraction-calculator.html}.
#'    \item LibreCAD, User Manual - Fundamentals: Units: Architectural and Decimal, 7 May 2022, \url{https://librecad-docs-dev.readthedocs.io/en/latest/ref/fundamentals.html#units}.
#'    \item Inch Calculator. Inch Fraction Calculator – Convert Decimal to Inches, 9 May 2022, \url{https://www.inchcalculator.com/inch-fraction-calculator/}.
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
#' @examples
#' 
#' # Please refer to the iemisc: Construction Measurement Examples vignette for
#' # additional examples
#' 
#' library(iemisc)
#'
#' # Example 1 from the Spike Reference
#'
#' check1 <- 18.649 # decimal feet
#'
#' construction_fraction(check1, type = "traditional", result =
#' "traditional", fraction = 16)
#'
#' # Reverse the calculation to check out the absolute error
#'
#' check2 <- construction_decimal(construction_fraction(check1,
#' type = "traditional", result = "traditional", fraction = 16),
#' result = "traditional", output = "vector")
#'
#' fracture::fracture(check2 - check1) # difference in inches
#'
#' # by approximate error
#'
#' approxerror(check2, check1) # answer as a percent (\%)
#' 
#' 
#' # check all other fraction levels
#' 
#' construction_fraction(check1, type = "traditional", result =
#' "traditional", fraction = 0)
#' 
#' construction_fraction(check1, type = "traditional", result =
#' "traditional", fraction = 2)
#' 
#' construction_fraction(check1, type = "traditional", result =
#' "traditional", fraction = 4)
#' 
#' construction_fraction(check1, type = "traditional", result =
#' "traditional", fraction = 8)
#' 
#' construction_fraction(check1, type = "traditional", result =
#' "traditional", fraction = 32)
#' 
#' construction_fraction(check1, type = "traditional", result =
#' "traditional", fraction = 64)
#' 
#' construction_fraction(check1, type = "traditional", result =
#' "traditional", fraction = 100)
#' 
#' construction_fraction(check1, type = "traditional", result =
#' "traditional", fraction = 128)
#' 
#' 
#' 
#'
#'
#' # Example 2
#'
#' library(iemisc)
#' import::from(fpCompare, "%==%")
#' 
#'
#' x1 <- construction_fraction(1.203125, type = "traditional", result =
#' "traditional", fraction = 16)
#'
#' x2 <- construction_fraction(14.4375, type = "librecad", result =
#' "inch", fraction = 16)
#'
#' x3 <- construction_fraction(14.4375, type = "librecad", result =
#' "traditional", fraction = 16)
#' 
#' x4 <- construction_fraction(14.43112, type = "librecad", result =
#' "traditional", fraction = 16)
#'
#' x5 <- construction_fraction(14.43112, type = "librecad", result =
#' "inch", fraction = 16)
#'
#' 
#' ex1 <- frac_to_numeric(x2)
#' 
#' ex2 <- construction_decimal(x1, result = "librecad", output = "vector")
#' 
#' ex3 <- construction_decimal(x3, result = "librecad", output = "vector")
#' 
#' ex4 <- construction_decimal(x4, result = "librecad", output = "vector")
#' 
#' ex5 <- frac_to_numeric(x5)
#' 
#' 
#' # check if ex1, ex2, ex3, ex4, and ex5 are equivalent
#' ex1 %==% ex2
#' 
#' ex1 %==% ex3
#' 
#' ex1 %==% ex4
#' 
#' ex1 %==% ex5
#' 
#' ex2 %==% ex3
#' 
#' ex2 %==% ex4
#' 
#' ex2 %==% ex5
#' 
#' ex3 %==% ex4
#' 
#' ex3 %==% ex5
#' 
#' ex4 %==% ex5
#' 
#' 
#'
#'
#' # Example 3 (from the Inch Calculator Reference)
#'
#' library(iemisc)
#'
#' construction_fraction(2.695, type = "librecad", result = "traditional",
#' fraction = 16)
#'
#' construction_fraction(2.695, type = "librecad", result = "inch",
#' fraction = 16)
#' 
#' 
#'
#'
#' # Example 4
#'
#' library(iemisc)
#'
#' construction_fraction(17.71354, type = "traditional", result = "traditional",
#' fraction = 16)
#'
#' construction_fraction(17.71354, type = "traditional", result = "inch",
#' fraction = 16)
#' 
#' 
#'
#'
#'
#'
#' 
#' 
#' 
#'
#'
#'
#'
#'
#'
#' @importFrom units set_units make_units drop_units
#' @importFrom round round_r3
#' @importFrom stringi stri_split_fixed stri_count_regex
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest testString
#'
#' @export
construction_fraction <- function (measurement, type = c("traditional", "librecad"), result = c("traditional", "inch"), fraction = c(0, 2, 4, 8, 16, 32, 64, 100, 128, 256)) {


inch <- ft <- NULL
# due to NSE notes in R CMD check


type <- type

result <- result

fraction <- fraction


# Check
assert_that(qtest(fraction, "N==1"), msg = "fraction should only be a single numeric value. Please try again.")
# only process with a single numeric value and provide an error message if the check fails

assert_that(isTRUE(any(c(0, 2, 4, 8, 16, 32, 64, 100, 128, 256) %in% fraction)), msg = "The fraction has not been identified correctly as either 0, 2, 4, 8, 16, 32, 64, 100, or 128. Please try again.")
# only process with a specified fraction and provide a stop warning if not

assert_that(qtest(type, "S==1"), msg = "There is not a type type or more than 1 type type. Please specify either 'traditional' or 'librecad'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("traditional", "librecad") %in% type)), msg = "The type type has not been identified correctly as either 'traditional' or 'librecad'. Please try again.")
# only process with a specified type and provide a stop warning if not

assert_that(qtest(result, "S==1"), msg = "There is not a result type or more than 1 result type. Please specify either 'traditional' or 'inch'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("traditional", "inch") %in% result)), msg = "The result type has not been identified correctly as either 'traditional' or 'inch'. Please try again.")
# only process with a specified result and provide a stop warning if not

assert_that(!any(qtest(measurement, "N==1(,)") == FALSE), msg = "measurement is NA, NaN, Inf, -Inf, empty, or a string. Or, measurement has less than or greater than 1 value. Please check the measurement value and try again.")
# only process with finite values and provide an error message if the check fails


if(type == "traditional") {

measurement <- measurement


} else if(type == "librecad") {

measure_in <- set_units(measurement, "inch")

measure_ft <- measure_in

units(measure_ft) <- make_units(ft)

measurement <- drop_units(measure_ft)

}


if(result == "traditional") {

change1 <- unlist(stri_split_fixed(measurement, "."))

# "The whole number of measurement is the foot value and subtract this from the decimal amount" (Spike Reference)
change2 <- measurement - as.numeric(change1[1])

# "extract the whole inches of change2 by multiplying by 12" (Spike Reference)
change3 <- change2 * 12

# "The whole number of change3 before the period is the full inch value. Subtract that amount from the decimal." (Spike Reference)
change4 <- unlist(stri_split_fixed(change3, "."))

change5 <- change3 - as.numeric(change4[1])

# "The remaining decimal of change5 is a fraction of an inch. As a fraction, you write it as change5/1000." Pick a denominator to use: 16, 32, 64, 100. "Multiply change5 by the chosen fraction and round the result to the nearest whole number to get the numerator of the fraction." (Spike Reference)


if(fraction == 0) {

change6 <- round_r3(change5 * 0, d = 0)

# "Collect all the results and place them together." (Spike Reference)
fraction_result <- paste0(change1[1], " ft ", change4[1], " in")

return(fraction_result)


} else if(fraction == 2) {

change6 <- round_r3(change5 * 2, d = 0)

# "Collect all the results and place them together." (Spike Reference)
fraction_result <- paste0(change1[1], " ft ", change4[1], " ", change6, "/2", " in")

return(fraction_result)


} else if(fraction == 4) {

change6 <- round_r3(change5 * 4, d = 0)

# "Collect all the results and place them together." (Spike Reference)
fraction_result <- paste0(change1[1], " ft ", change4[1], " ", change6, "/4", " in")

return(fraction_result)



} else if(fraction == 8) {

change6 <- round_r3(change5 * 8, d = 0)

# "Collect all the results and place them together." (Spike Reference)
fraction_result <- paste0(change1[1], " ft ", change4[1], " ", change6, "/8", " in")

return(fraction_result)



} else if(fraction == 16) {

change6 <- round_r3(change5 * 16, d = 0)

# "Collect all the results and place them together." (Spike Reference)
fraction_result <- paste0(change1[1], " ft ", change4[1], " ", change6, "/16", " in")

return(fraction_result)


} else if(fraction == 32) {

change6 <- round_r3(change5 * 32, d = 0)

# "Collect all the results and place them together." (Spike Reference)
fraction_result <- paste0(change1[1], " ft ", change4[1], " ", change6, "/32", " in")

return(fraction_result)


} else if(fraction == 64) {

change6 <- round_r3(change5 * 64, d = 0)

# "Collect all the results and place them together." (Spike Reference)
fraction_result <- paste0(change1[1], " ft ", change4[1], " ", change6, "/64", " in")

return(fraction_result)


} else if(fraction == 100) {

change6 <- round_r3(change5 * 100, d = 0)

# "Collect all the results and place them together." (Spike Reference)
fraction_result <- paste0(change1[1], " ft ", change4[1], " ", change6, "/100", " in")

return(fraction_result)


} else if(fraction == 128) {

change6 <- round_r3(change5 * 128, d = 0)

# "Collect all the results and place them together." (Spike Reference)
fraction_result <- paste0(change1[1], " ft ", change4[1], " ", change6, "/128", " in")

return(fraction_result)


} else if(fraction == 256) {

change6 <- round_r3(change5 * 256, d = 0)

# "Collect all the results and place them together." (Spike Reference)
fraction_result <- paste0(change1[1], " ft ", change4[1], " ", change6, "/256", " in")

return(fraction_result)

}


} else if(result == "inch") {

measure_ft <- measurement

units(measure_ft) <- make_units(ft)

measure_in <- measure_ft

units(measure_in) <- make_units(inch)

measurement <- drop_units(measure_in)


change1 <- unlist(stri_split_fixed(measurement, "."))

# "The whole number of measurement is the foot value and subtract this from the decimal amount" (Spike Reference)
change2 <- measurement - as.numeric(change1[1])


if(fraction == 0) {

change3 <- round_r3(change2 * 0, d = 0)

# "Collect all the results and place them together."
fraction_result <- paste0(change1[1], " in")

return(fraction_result)


} else if(fraction == 2) {

change3 <- round_r3(change2 * 2, d = 0)

# "Collect all the results and place them together."
fraction_result <- paste0(change1[1], " ", change3, "/2", " in")

return(fraction_result)


} else if(fraction == 4) {

change3 <- round_r3(change2 * 4, d = 0)

# "Collect all the results and place them together."
fraction_result <- paste0(change1[1], " ", change3, "/4", " in")

return(fraction_result)



} else if(fraction == 8) {

change3 <- round_r3(change2 * 8, d = 0)

# "Collect all the results and place them together."
fraction_result <- paste0(change1[1], " ", change3, "/8", " in")

return(fraction_result)



} else if(fraction == 16) {

change3 <- round_r3(change2 * 16, d = 0)

# "Collect all the results and place them together."
fraction_result <- paste0(change1[1], " ", change3, "/16", " in")

return(fraction_result)


} else if(fraction == 32) {

change3 <- round_r3(change2 * 32, d = 0)

# "Collect all the results and place them together."
fraction_result <- paste0(change1[1], " ", change3, "/32", " in")

return(fraction_result)


} else if(fraction == 64) {

change3 <- round_r3(change2 * 64, d = 0)

# "Collect all the results and place them together."
fraction_result <- paste0(change1[1], " ", change3, "/64", " in")

return(fraction_result)


} else if(fraction == 100) {

change3 <- round_r3(change2 * 100, d = 0)

# "Collect all the results and place them together."
fraction_result <- paste0(change1[1], " ", change3, "/100", " in")

return(fraction_result)


} else if(fraction == 128) {

change3 <- round_r3(change2 * 128, d = 0)

# "Collect all the results and place them together."
fraction_result <- paste0(change1[1], " ", change3, "/128", " in")

return(fraction_result)


} else if(fraction == 256) {

change3 <- round_r3(change2 * 256, d = 0)

# "Collect all the results and place them together."
fraction_result <- paste0(change1[1], " ", change3, "/256", " in")

return(fraction_result)

}

}
}





















#' Construction Decimal Engineering (LibreCAD Style)
#'
#' Convert a construction measurement in US Customary Units (foot + inch) with
#' or without a fraction into its equivalent as an Engineering value (LibreCAD
#' style)
#'
#'
#' @param measurement character or numeric vector that contains the construction
#'     measurement (decimal or foot + inch)
#'
#'
#'
#' @return the engineering construction measurement value as a character \code{\link[base]{vector}}.
#'     In LibreCAD, a construction measurement of 1'-2 7/16" = 14.43112 (decimal) =
#'     1'-2.43112" (engineering). 
#'
#'     
#'
#'
#'
#'
#' @source
#' regex - Replace single backslash in R - Stack Overflow answered and edited by Hong Ooi on Aug 21, 2014. (Also see the additional comments in response to the answer.) See \url{https://stackoverflow.com/questions/25424382/replace-single-backslash-in-r}.
#'
#'
#'
#'
#' @references
#' \enumerate{
#'    \item LibreCAD, User Manual - Fundamentals: Units: Engineering and Decimal, 7 May 2022, \url{https://librecad-docs-dev.readthedocs.io/en/latest/ref/fundamentals.html#units}.
#'    \item LibreCAD rs_units.cpp code referencing how to calculate the engineering units.
#' }
#'
#'
#' @author Irucka Embry, R. van Twisk (rs_units.cpp code)
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
#' # Please refer to the iemisc: Construction Measurement Examples vignette for
#' # additional examples
#' 
#' library(iemisc)
#'
#' librecad1a <- "1 ft 2 7/16\""
#' 
#' construction_decimal_eng(librecad1a)
#' 
#'
#' librecad4a <- 14.43112
#'
#' construction_decimal_eng(librecad4a)
#' 
#'
#' librecad5a <- 14.4375
#'
#' construction_decimal_eng(librecad5a)
#' 
#'
#' librecad6a <- 17.71354
#'
#' construction_decimal_eng(librecad6a)
#' 
#'
#' librecad7a <- 86.000000
#'
#' construction_decimal_eng(librecad7a)
#' 
#'
#' checkst <- 14.43112
#' 
#' construction_decimal_eng(checkst)
#' 
#' construction_fraction(checkst, type = "librecad", result = "traditional",
#' fraction = 16)
#'
#'
#'
#'
#'
#' 
#'
#'
#' @importFrom stringi stri_detect_regex
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#' @export
construction_decimal_eng <- function (measurement) {


if (stri_detect_regex(measurement, "[:alpha:]|'| ")) {

# Check
assert_that(!any(testString(measurement, min.chars = 1, pattern = "[0-9]", ignore.case = TRUE) == FALSE), msg = "measurement is a numeric vector or a character vector without any numeric values. measurement should be a character vector that contains numeric values. Please try again.")
# only process with string values with numbers and provide an error message if the check fails

measurements <- construction_decimal(measurement, result = "librecad", output = "vector")

change1 <- floor(measurements / 12)

change2 <- measurements - (change1 * 12)

changer <- paste0(change1, "'", "-", change2, "\"")

return(changer)


} else if (!stri_detect_regex(measurement, "[:alpha:]|'| ")) {

# Check
assert_that(!any(qtest(measurement, "N==1(,)") == FALSE), msg = "measurement is NA, NaN, Inf, -Inf, empty, or a string. Or, measurement has less than or greater than 1 value. Please check the measurement value and try again.")
# only process with finite values and provide an error message if the check fails

change1 <- floor(measurement / 12)

change2 <- measurement - (change1 * 12)

changer <- paste0(change1, "'", "-", change2, "\"")

return(changer)

}
}
