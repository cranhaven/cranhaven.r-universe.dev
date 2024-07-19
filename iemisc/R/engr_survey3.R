#' Calculate the Distance between Engineering Survey Points (Length or Number of Stations)
#'
#' Takes engineering survey points in various units (foot, US survey foot,
#' meters, or kilometers) and calculates the horizontal length in various units
#' (foot, US survey foot, US survey mile, mile, meters, or kilometers).
#'
#'
#'
#' @param length1 character vector that contains the beginning engineering
#'     survey station value
#' @param station_distance numeric vector that contains the horizontal distance
#'     between any 2 points along the survey, the default is 100 feet
#' @param units character vector that contains the system of units for the
#'     \code{station_distance} (options are \code{foot}, \code{survey_ft}
#'     (United States Customary System) [US survey foot], \code{meters} for
#'     International System of Units meters, or \code{kilometers} for International System
#'     of Units kilometers)
#' @param output character vector that contains the system of units for the
#'     horizontal length (options are \code{foot}, \code{survey_ft}
#'     (United States Customary System) [US survey foot], \code{survey_mile}
#'     (United States Customary System) [US survey mile], \code{mile},
#'     \code{meters} for International System of Units meters, or \code{kilometers} for
#'     International System of Units kilometers)
#'
#'
#'
#' @return horizontal length as a numeric vector (ex. 1214.402) or as a
#'     character vector with the word stations after the number (ex. 1214.402
#'     stations)
#'
#'
#'
#' @references
#' \enumerate{
#'    \item udunits.dat, v 1.18 2006/09/20 18:59:18 steve Exp, \url{https://web.archive.org/web/20230202155021/https://www.unidata.ucar.edu/software/udunits/udunits-1/udunits.txt}. Retrieved thanks to the Internet Archive: Wayback Machine

#'    \item Engineer Boards: Transportation. "Stationing. Dumb question?" Question asked by By NIKE, August 31, 2013 and answered by ptatohed on September 1, 2013. See \url{https://engineerboards.com/threads/stationing-dumb-question.21935/}.
#' }
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
#' # "What the others said is correct. 1 station is equal to 100 feet. So when
#' # asked how many stations are in (3.2mi x 5280ft/mi = ) 16,896 feet, you are being
#' # asked how many 100 foot-segments are in 16,896 feet? The answer of course is
#' # 16,896ft / 100ft/sta = 168.96 sta." Source: Reference 2
#'
#' length1 <- "16,896" # feet
#'
#' engr_survey3(length1, station_distance = 100, units = "foot", output = "numeric")
#' engr_survey3(length1, station_distance = 100, units = "foot", output = "string")
#' # the answer provides the number of stations
#'
#' # Note: Both answers should be the same as 3.2 miles = 16,896 feet.
#'
#' length2 <- 3.2 # mile
#'
#' engr_survey3(length2, station_distance = 100, units = "mile", output = "numeric")
#' engr_survey3(length2, station_distance = 100, units = "mile", output = "string")
#' # the answer provides the number of stations
#'
#'
#'
#'
#'
#'
#' @importFrom stringi stri_replace_all_fixed stri_replace_first_fixed
#' @importFrom units set_units make_units
#' @importFrom checkmate qtest
#' @importFrom assertthat assert_that
#' @importFrom round round_r3
#'
#' @export
engr_survey3 <- function (length1, station_distance = 100, units = c("foot", "survey_ft", "survey_mile", "mile", "meters", "kilometers"), output = c("numeric", "string")) {


US_survey_foot <- m <- km <- ft <- US_survey_mile <- international_mile <- NULL
# due to NSE notes in R CMD check


units <- units

station_distance <- station_distance

output <- output


# Check length1, station_distance, units, and output
assert_that(!any(qtest(length1, c("N==1(0,)", "S==1")) == FALSE), msg = "length1 is a numeric vector or a character vector without any numeric values. length1 should be a character vector that contains numeric values. Please try again.")
# only process with string values with numbers and provide an error message if the check fails

assert_that(!any(qtest(station_distance, "N==1(0,)") == FALSE), msg = "station_distance is 0, NA, NaN, Inf, -Inf, a string, or empty. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(isTRUE(any(c("foot", "survey_ft", "survey_mile", "mile", "meters", "kilometers") %in% units)), msg = "Incorrect unit selection. The only possible units are 'foot', 'survey_ft', 'survey_mile', 'mile', 'meters', 'kilometers'. Please try again.")
# only process with a specified unit and provide a stop warning if not

assert_that(isTRUE(any(c("numeric", "string") %in% output)), msg = "Incorrect unit selection. The only possible output are 'foot', 'survey_ft', 'survey_mile', 'mile', 'meters', 'kilometers'. Please try again.")
# only process with a specified unit and provide a stop warning if not



length1 <- as.numeric(stri_replace_all_fixed(length1, ",", ""))


if (units == "foot") {

station <- as.numeric(length1 / station_distance)

station_string <- paste0(station, " stations")


} else if (units == "survey_ft") {

units(length1) <- make_units(US_survey_foot)

units(length1) <- make_units(ft)

station <- as.numeric(length1 / station_distance)

station_string <- paste0(station, " stations")


} else if (units == "survey_mile") {

units(length1) <- make_units(US_survey_mile)

units(length1) <- make_units(ft)

station <- as.numeric(length1 / station_distance)

station_string <- paste0(station, " stations")


} else if (units == "mile") {

units(length1) <- make_units(international_mile)

units(length1) <- make_units(ft)

station <- as.numeric(length1 / station_distance)

station_string <- paste0(station, " stations")


} else if (units == "meters") {

units(length1) <- make_units(m)

units(length1) <- make_units(ft)

station <- as.numeric(length1 / station_distance)

station_string <- paste0(station, " stations")


} else if (units == "kilometers") {

units(length1) <- make_units(km)

units(length1) <- make_units(ft)

station <- as.numeric(length1 / station_distance)

station_string <- paste0(station, " stations")

}

if (output == "numeric") {

return(station)

} else {

return(station_string)

}
}











#' Calculate the Station Distance between Engineering Survey Points
#'
#' Takes engineering survey points in various units (foot, US survey foot,
#' meters, or kilometers) and determines the upstream station location.
#'
#'
#' @param object numeric vector that contains the upstream length
#' @param ds_station character vector that contains the ending engineering
#'     survey station value
#' @param station_distance numeric vector that contains the horizontal distance
#'     between any 2 points along the survey, the default is 100 feet
#' @param units character vector that contains the system of units for the
#'     \code{station_distance} (options are \code{foot}, \code{survey_ft}
#'     (United States Customary System) [US survey foot], \code{meters} for
#'     International System of Units meters, or \code{kilometers} for International System
#'     of Units kilometers)
#'
#'
#'
#' @return the final engineering survey position (ex. Sta. 5+55)
#'
#'
#'
#' @source
#'     r - Insert a character at a specific location in a string - Stack Overflow answered and edited by Justin on Dec 13 2012. See \url{https://stackoverflow.com/questions/13863599/insert-a-character-at-a-specific-location-in-a-string}.
#'
#'
#'
#' @references
#' \enumerate{
#'    \item udunits.dat, v 1.18 2006/09/20 18:59:18 steve Exp, \url{https://web.archive.org/web/20230202155021/https://www.unidata.ucar.edu/software/udunits/udunits-1/udunits.txt}. Retrieved thanks to the Internet Archive: Wayback Machine

#'    \item Engineer Boards: Transportation. "Stationing. Dumb question?" Question asked by By NIKE, August 31, 2013 and answered by ptatohed on September 1, 2013. See \url{https://engineerboards.com/threads/stationing-dumb-question.21935/}.
#' }
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
#' library(iemisc)
#'
#' # Example 1
#'
#' # "Conversely, if you were asked at what station a manhole 555 ft upstream of
#' # Sta 0+00 is, your answer would be 0.00 sta + 555 ft / 100 ft/sta = 5.55 sta =
#' # Sta 5+55." Source: Reference 2
#'
#' engr_survey4(555, "0+00", units = "foot")
#'
#'
#'
#'
#'
#'
#' @importFrom stringi stri_split_fixed stri_replace_first_fixed stri_detect_regex
#' @importFrom units set_units make_units
#' @importFrom checkmate qtest testString
#' @importFrom assertthat assert_that
#' @importFrom round round_r3
#'
#' @export
engr_survey4 <- function (object, ds_station, station_distance = 100, units = c("foot", "survey_ft", "survey_mile", "mile", "meters", "kilometers")) {


US_survey_foot <- m <- km <- ft <- US_survey_mile <- international_mile <- NULL
# due to NSE notes in R CMD check


units <- units

station_distance <- station_distance


# Check object, station_distance, units, and output
assert_that(!any(qtest(object, "N==1(0,)") == FALSE), msg = "object is 0, NA, NaN, Inf, -Inf, a string, or empty. Please try again.")
# only process with finite values and provide an error message if the check fails

# check again to makre sure in proper format
assert_that(!any(testString(ds_station, min.chars = 1, pattern = "[0-9]+[0-9]", ignore.case = TRUE) == FALSE), msg = "ds_station is a numeric vector or a character vector without any numeric values. ds_station should be a character vector that contains numeric values. Please try again.")
# only process with string values with numbers and provide an error message if the check fails

assert_that(!any(qtest(station_distance, "N==1(0,)") == FALSE), msg = "station_distance is 0, NA, NaN, Inf, -Inf, a string, or empty. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(isTRUE(any(c("foot", "survey_ft", "survey_mile", "mile", "meters", "kilometers") %in% units)), msg = "Incorrect unit selection. The only possible units are 'foot', 'survey_ft', 'survey_mile', 'mile', 'meters', and 'kilometers'. Please try again.")
# only process with a specified unit and provide a stop warning if not


object <- as.numeric(stri_replace_all_fixed(object, ",", ""))


if (units == "foot") {

ds_stationa <- stri_split_fixed(ds_station, "+")

ds_stationa_use <- as.numeric(ds_stationa[[1]][1])

ds_stationb <- as.numeric(ds_stationa[[1]][2])

ds_station_use <- sum(c(ds_stationa_use * station_distance, ds_stationb))

us_station <- round_r3(sum(c(ds_station_use, object / station_distance)), d = 4)

us_station_use <- as.character(us_station)


if(stri_detect_regex(us_station_use, "\\.(\\d{3}+)"))
{
us_station_use1 <- stri_replace_first_fixed(us_station_use, ".", "+")

split <- stri_split_fixed(us_station_use1, "+")

split1 <- split[[1]][[1]]

split2 <- split[[1]][[2]]

split2a <- sub("(?<=(\\d{2}))", ".", split2, perl = TRUE) # Source 1

return_split <- paste0("Sta. ", split1, "+", split2a)

us_station_use1 <- return_split

} else {

us_station_use1 <- stri_replace_first_fixed(us_station_use, ".", "+")

us_station_use1 <- paste0("Sta. ", us_station_use1)
}

us_station_use1 <- ifelse(stri_detect_fixed(us_station_use1, "+"), us_station_use1, paste0(us_station_use1, "+00"))



} else if (units == "survey_ft") {

units(object) <- make_units(US_survey_foot)

units(object) <- make_units(ft)


ds_stationa <- stri_split_fixed(ds_station, "+")

ds_stationa_use <- as.numeric(ds_stationa[[1]][1])

ds_stationb <- as.numeric(ds_stationa[[1]][2])

ds_station_use <- sum(c(ds_stationa_use * station_distance, ds_stationb))

us_station <- round_r3(sum(c(ds_station_use, object / station_distance)), d = 4)

us_station_use <- as.character(us_station)


if(stri_detect_regex(us_station_use, "\\.(\\d{3}+)"))
{
us_station_use1 <- stri_replace_first_fixed(us_station_use, ".", "+")

split <- stri_split_fixed(us_station_use1, "+")

split1 <- split[[1]][[1]]

split2 <- split[[1]][[2]]

split2a <- sub("(?<=(\\d{2}))", ".", split2, perl = TRUE) # Source 1

return_split <- paste0("Sta. ", split1, "+", split2a)

us_station_use1 <- return_split

} else {

us_station_use1 <- stri_replace_first_fixed(us_station_use, ".", "+")

us_station_use1 <- paste0("Sta. ", us_station_use1)
}

us_station_use1 <- ifelse(stri_detect_fixed(us_station_use1, "+"), us_station_use1, paste0(us_station_use1, "+00"))


} else if (units == "survey_mile") {

units(object) <- make_units(US_survey_mile)

units(object) <- make_units(ft)


ds_stationa <- stri_split_fixed(ds_station, "+")

ds_stationa_use <- as.numeric(ds_stationa[[1]][1])

ds_stationb <- as.numeric(ds_stationa[[1]][2])

ds_station_use <- sum(c(ds_stationa_use * station_distance, ds_stationb))

us_station <- round_r3(sum(c(ds_station_use, object / station_distance)), d = 4)

us_station_use <- as.character(us_station)


if(stri_detect_regex(us_station_use, "\\.(\\d{3}+)")) {

us_station_use1 <- stri_replace_first_fixed(us_station_use, ".", "+")

split <- stri_split_fixed(us_station_use1, "+")

split1 <- split[[1]][[1]]

split2 <- split[[1]][[2]]

split2a <- sub("(?<=(\\d{2}))", ".", split2, perl = TRUE) # Source 1

return_split <- paste0("Sta. ", split1, "+", split2a)

us_station_use1 <- return_split

} else {

us_station_use1 <- stri_replace_first_fixed(us_station_use, ".", "+")

us_station_use1 <- paste0("Sta. ", us_station_use1)
}

us_station_use1 <- ifelse(stri_detect_fixed(us_station_use1, "+"), us_station_use1, paste0(us_station_use1, "+00"))


} else if (units == "mile") {



units(object) <- make_units(international_mile)

units(object) <- make_units(ft)


ds_stationa <- stri_split_fixed(ds_station, "+")

ds_stationa_use <- as.numeric(ds_stationa[[1]][1])

ds_stationb <- as.numeric(ds_stationa[[1]][2])

ds_station_use <- sum(c(ds_stationa_use * station_distance, ds_stationb))

us_station <- round_r3(sum(c(ds_station_use, object / station_distance)), d = 4)

us_station_use <- as.character(us_station)


if(stri_detect_regex(us_station_use, "\\.(\\d{3}+)")) {

us_station_use1 <- stri_replace_first_fixed(us_station_use, ".", "+")

split <- stri_split_fixed(us_station_use1, "+")

split1 <- split[[1]][[1]]

split2 <- split[[1]][[2]]

split2a <- sub("(?<=(\\d{2}))", ".", split2, perl = TRUE) # Source 1

return_split <- paste0("Sta. ", split1, "+", split2a)

us_station_use1 <- return_split

} else {

us_station_use1 <- stri_replace_first_fixed(us_station_use, ".", "+")

us_station_use1 <- paste0("Sta. ", us_station_use1)
}

us_station_use1 <- ifelse(stri_detect_fixed(us_station_use1, "+"), us_station_use1, paste0(us_station_use1, "+00"))


} else if (units == "meters") {

units(object) <- make_units(m)

units(object) <- make_units(ft)


ds_stationa <- stri_split_fixed(ds_station, "+")

ds_stationa_use <- as.numeric(ds_stationa[[1]][1])

ds_stationb <- as.numeric(ds_stationa[[1]][2])

ds_station_use <- sum(c(ds_stationa_use * station_distance, ds_stationb))

us_station <- round_r3(sum(c(ds_station_use, object / station_distance)), d = 4)

us_station_use <- as.character(us_station)


if(stri_detect_regex(us_station_use, "\\.(\\d{3}+)")) {

us_station_use1 <- stri_replace_first_fixed(us_station_use, ".", "+")

split <- stri_split_fixed(us_station_use1, "+")

split1 <- split[[1]][[1]]

split2 <- split[[1]][[2]]

split2a <- sub("(?<=(\\d{2}))", ".", split2, perl = TRUE) # Source 1

return_split <- paste0("Sta. ", split1, "+", split2a)

us_station_use1 <- return_split

} else {

us_station_use1 <- stri_replace_first_fixed(us_station_use, ".", "+")

us_station_use1 <- paste0("Sta. ", us_station_use1)
}

us_station_use1 <- ifelse(stri_detect_fixed(us_station_use1, "+"), us_station_use1, paste0(us_station_use1, "+00"))


} else if (units == "kilometers") {

units(object) <- make_units(km)

units(object) <- make_units(ft)


ds_stationa <- stri_split_fixed(ds_station, "+")

ds_stationa_use <- as.numeric(ds_stationa[[1]][1])

ds_stationb <- as.numeric(ds_stationa[[1]][2])

ds_station_use <- sum(c(ds_stationa_use * station_distance, ds_stationb))

us_station <- round_r3(sum(c(ds_station_use, object / station_distance)), d = 4)

us_station_use <- as.character(us_station)


if(stri_detect_regex(us_station_use, "\\.(\\d{3}+)")) { 

us_station_use1 <- stri_replace_first_fixed(us_station_use, ".", "+")

split <- stri_split_fixed(us_station_use1, "+")

split1 <- split[[1]][[1]]

split2 <- split[[1]][[2]]

split2a <- sub("(?<=(\\d{2}))", ".", split2, perl = TRUE) # Source 1

return_split <- paste0("Sta. ", split1, "+", split2a)

us_station_use1 <- return_split


} else {

us_station_use1 <- stri_replace_first_fixed(us_station_use, ".", "+")

us_station_use1 <- paste0("Sta. ", us_station_use1)
}

us_station_use1 <- ifelse(stri_detect_fixed(us_station_use1, "+"), us_station_use1, paste0(us_station_use1, "+00"))

}

return(us_station_use1)

}
