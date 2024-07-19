#' Calculate the Distance between Engineering Survey Points
#'
#' Takes engineering survey points in various units (foot, US survey foot,
#' meters, or kilometers) and calculates the horizontal length in various units
#' (foot, US survey foot, US survey mile, mile, meters, or kilometers).
#'
#'
#'
#' @param station1 character vector that contains the begin engineering
#'     survey station value
#' @param station2 character vector that contains the end engineering
#'     survey station value
#' @param station_distance numeric vector that contains the horizontal distance
#'     between any 2 points along the survey, the default is 100 feet
#' @param units1 character vector that contains the system of units for the
#'     \code{station_distance} (options are \code{foot}, \code{survey_ft}
#'     (United States Customary System) [US survey foot], \code{meters} for
#'     International System of Units meters, or \code{kilometers} for International System
#'     of Units kilometers)
#' @param units2 character vector that contains the system of units for the
#'     horizontal length (options are \code{foot}, \code{survey_ft}
#'     (United States Customary System) [US survey foot], \code{survey_mile}
#'     (United States Customary System) [US survey mile], \code{mile},
#'     \code{meters} for International System of Units meters, or \code{kilometers} for
#'     International System of Units kilometers)
#'
#'
#'
#' @return the calculated horizontal distance in the chosen unit as a numeric
#'     vector with the units attached
#'
#'
#'
#' @references
#' \enumerate{
#'    \item udunits.dat, v 1.18 2006/09/20 18:59:18 steve Exp, \url{https://web.archive.org/web/20230202155021/https://www.unidata.ucar.edu/software/udunits/udunits-1/udunits.txt}. Retrieved thanks to the Internet Archive: Wayback Machine
#'    \item Ben W. Lewis, PE, July 12, 2016, "Construction Plan Reading Basics & Applications Part III Typical Calculations / Quantity Take-off's: Calculate Project Length", \url{https://web.archive.org/web/20171008121149/http://www.richlandonline.com/Portals/0/Departments/Procurement/SLBE/Plan\%20Reading\%20PowerPoint\%20Presentation.pdf}. Used the Internet Archive: Wayback Machine archived version for acceptance into CRAN.
#'    \item Georgia Department of Transportation Skills Development Series, Revised May 1, 2008, "Basic Highway Plan Reading", \url{https://web.archive.org/web/20220401015405/http://www.dot.ga.gov/PartnerSmart/Training/Documents/ESD/BasicHiwyPlanReading.pdf}. Retrieved thanks to the Internet Archive: Wayback Machine
#' }
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
#' # Please refer to the iemisc: Engineering Survey Examples vignette for
#' # additional examples
#' 
#' # Example 1
#' 
#' library(iemisc)
#'
#' engr_survey2("395+75", "397+13", station_distance = 100, units1 = "foot",
#' units2 = "foot")
#'
#'
#'
#' # Example 2
#' 
#' library(iemisc)
#'
#' station1 <- "333+03"
#' station2 <- "332+94"
#'
#' engr_survey2(station1, station2, units1 = "foot", units2 = "survey_mile")
#'
#'
#'
#' # Example 3 from Lewis Reference document page 25
#' 
#' library(iemisc)
#'
#' station3 <- "10+25.62"
#' station4 <- "189+45.72"
#'
#' engr_survey2(station3, station4, units1 = "foot", units2 = "mile")
#'
#'
#'
#' # Example 4 from Georgia reference page 27 (document page 43)
#' 
#' library(iemisc)
#'
#' engr_survey2("701+50.00", "409+69.00", station_distance = 100,
#' units1 = "survey_ft", units2 = "foot")
#'
#'
#'
#'
#'
#'
#' @importFrom stringi stri_split_fixed
#' @importFrom units make_units
#' @importFrom checkmate qtest testString
#' @importFrom assertthat assert_that
#'
#' @export
engr_survey2 <- function (station1, station2, station_distance = 100, units1 = c("foot", "survey_ft", "meters", "kilometers"), units2 = c("foot", "survey_ft", "survey_mile", "mile", "meters", "kilometers")) {


US_survey_foot <- m <- km <- ft <- US_survey_mile <- international_mile <- NULL
# due to NSE notes in R CMD check


units1 <- units1

units2 <- units2

station_distance <- station_distance


# Check station1, station2, station_distance, units1, and units2
assert_that(qtest(units1, "S==1"), msg = "There is not an unit type selected for the station distance (units1). Please specify either 'foot', 'survey_ft', 'meters', or 'kilometers'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(qtest(units2, "S==1"), msg = "There is not an unit type selected for the horizontal length (units2). Please specify either 'foot', 'survey_ft', 'survey_mile', 'mile', 'meters', or 'kilometers'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(!any(testString(station1, min.chars = 1, pattern = "[0-9]|+") == FALSE), msg = "station1 is a numeric vector or a character vector without any numeric values. station1 should be a character vector that contains numeric values. Please try again.")
# only process with string values with numbers and provide an error message if the check fails

assert_that(!any(testString(station2, min.chars = 1, pattern = "[0-9]|+") == FALSE), msg = "station2 is a numeric vector or a character vector without any numeric values. station2 should be a character vector that contains numeric values. Please try again.")
# only process with string values with numbers and provide an error message if the check fails

assert_that(!any(qtest(station_distance, "N==1(0,)") == FALSE), msg = "station_distance is 0, NA, NaN, Inf, -Inf, a string, or empty. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(isTRUE(any(c("foot", "survey_ft", "meters", "kilometers") %in% units1)), msg = "Incorrect unit selection for units1 (station_distance). The only possible units are foot, survey_ft, meters, and kilometers. Please try again.")
# only process with a specified unit and provide a stop warning if not

assert_that(isTRUE(any(c("foot", "survey_ft", "survey_mile", "mile", "meters", "kilometers") %in% units2)), msg = "Incorrect unit selection for units2 (horizontal length). The only possible units are foot, survey_ft, survey_mile, mile, meters, and kilometers. Please try again.")
# only process with a specified unit and provide a stop warning if not


# adjust station distance units as needed
if (units1 == "foot") {

station_distance <- station_distance


} else if (units1 == "survey_ft") {

units(station_distance) <- make_units(US_survey_foot)


} else if (units1 == "meters") {

units(station_distance) <- make_units(m)


} else if (units1 == "kilometers") {

units(station_distance) <- make_units(km)

}


station_distance2 <- as.numeric(station_distance)


# decompose station 1
station1a <- stri_split_fixed(station1, "+")

station1a_use <- as.numeric(station1a[[1]][1])

station1b <- as.numeric(station1a[[1]][2])

station1_use <- sum(c(station1a_use * station_distance2, station1b))



# decompose station 2
station2a <- stri_split_fixed(station2, "+")

station2a_use <- as.numeric(station2a[[1]][1])

station2b <- as.numeric(station2a[[1]][2])

station2_use <- sum(c(station2a_use * station_distance2, station2b))



horizontal_length <- station2_use - station1_use

if (horizontal_length < 0) {

horizontal_length <- horizontal_length * -1

}

units(horizontal_length) <- make_units(ft)


if (units2 == "foot") {

horiztonal_length <- horizontal_length


} else if (units2 == "survey_ft") {

units(horizontal_length) <- make_units(US_survey_foot)


} else if (units2 == "survey_mile") {

units(horizontal_length) <- make_units(US_survey_mile)


} else if (units2 == "mile") {

units(horizontal_length) <- make_units(international_mile)


} else if (units2 == "meters") {

units(horizontal_length) <- make_units(m)


} else if (units2 == "kilometers") {

units(horizontal_length) <- make_units(km)

}

return(horizontal_length)

}
