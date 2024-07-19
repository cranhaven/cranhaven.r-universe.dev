#' Conversion of Engineering Survey Measurements to Decimal Degrees (KY and TN) - Batch
#'
#' Takes Kentucky or Tennessee-based Northing and Easting engineering survey
#' measurements [based in the State Plane Coordinate System (SPCS)] in meters,
#' international foot, or US survey foot and converts those values into
#' geodetic coordinates of the World Geodetic System (WGS) (19)84 (EPSG:4326).
#' [MapTiler Reference] Each latitude [Y] and longitude [X] point is verified to be
#' located within Kentucky or Tennessee. This is the batch version of engr_survey
#' as it processes multiple pairs of Northing and Easting points at a time.
#'
#'
#'
#' @param Northing numeric vector (or character vector with numbers, commas,
#'     and decimal points) that contains the Northing engineering survey
#'     measurement in meters, international foot, or US survey foot [a minimum
#'     of 2 points each time]
#' @param Easting numeric vector (or character vector with numbers, commas,
#'     and decimal points) that contains the Easting engineering survey
#'     measurement in meters, international foot, or US survey foot [a minimum
#'     of 2 points each time]
#' @param units character vector that contains the system of units (options are
#'     \code{survey_ft} (United States Customary System) [US survey foot],
#'     \code{foot}, or \code{meters} (International System of Units) [meters]
#'     [only 1 set of units at a time])
#' @param location character vector that contains the location name ('KY' for
#'     Kentucky or 'TN' for Tennessee) [only 1 location at a time]
#' @param output character vector that contains basic for the default result
#'     using a simple \code{\link[data.table]{data.table}} or table for the result as a
#'     complex \code{\link[data.table]{data.table}} [using rbindlist therefore resulting in only
#'     1 data.table]
#'
#'
#'
#' @return the projected associated latitude [Y] and longitude [X] coordinates
#'   in Decimal Degrees using the \code{\link{sf}} system
#'
#'
#' @source
#' \enumerate{
#'    \item Win-Vector Blog. John Mount, June 11, 2018, "R Tip: use isTRUE()", \url{https://win-vector.com/2018/06/11/r-tip-use-istrue/}.
#'    \item Latitude Longitude Coordinates to State Code in R - Stack Overflow answered by Josh O'Brien on Jan 6 2012 and edited by Josh O'Brien on Jun 18, 2020. See \url{https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r}.
#'    \item r - Convert column classes in data.table - Stack Overflow answered by Matt Dowle on Dec 27 2013. See \url{https://stackoverflow.com/questions/7813578/convert-column-classes-in-data-table}.
#'    \item Excel vlook up function in R for data frame - Stack Overflow answered by Tyler Rinker on Apr 8 2013 and edited by Tyler Rinker on Feb 26 2014. See \url{https://stackoverflow.com/questions/15882743/excel-vlook-up-function-in-r-for-data-frame}.
#'    \item r - Converting geo coordinates from degree to decimal - Stack Overflow answered by Robbes on Jan 3 2018 and edited by ayaio on Jan 3 2018. See \url{https://stackoverflow.com/questions/14404596/converting-geo-coordinates-from-degree-to-decimal}.
#'    \item r - How to not run an example using roxygen2? - Stack Overflow answered and edited by samkart on Jul 9 2017. (Also see the additional comments in response to the answer.) See \url{https://stackoverflow.com/questions/12038160/how-to-not-run-an-example-using-roxygen2}.
#'    \item devtools - Issues in R package after CRAN asked to replace dontrun by donttest - Stack Overflow answered by Hong Ooi on Sep 1 2020. (Also see the additional comments in response to the answer.) See \url{https://stackoverflow.com/questions/63693563/issues-in-r-package-after-cran-asked-to-replace-dontrun-by-donttest}.
#' }
#'
#'
#'
#' @references
#' \enumerate{
#'    \item udunits.dat, v 1.18 2006/09/20 18:59:18 steve Exp, \url{https://web.archive.org/web/20230202155021/https://www.unidata.ucar.edu/software/udunits/udunits-1/udunits.txt}. Retrieved thanks to the Internet Archive: Wayback Machine

#'    \item Spatial Reference, Aug. 13, 2004, "EPSG:3088: NAD83 / Kentucky Single Zone", \url{https://spatialreference.org/ref/epsg/3088/}.
#'    \item Spatial Reference, March 7, 2000, "EPSG:32136 NAD83 / Tennessee", \url{https://spatialreference.org/ref/epsg/32136/}.
#'    \item MapTiler Team, "EPSG:4326: WGS 84 -- WGS84 - World Geodetic System 1984, used in GPS, \url{https://epsg.io/4326}.
#'    \item Tennessee Department of Transportation Design Division, Tennessee Department of Transportation Tennessee Geodetic Reference Network (TGRN) Reference Manual Second Edition Issued, page ix, \url{https://www.tn.gov/content/dam/tn/tdot/documents/TgrnComposite.pdf}.
#'    \item Earth Point, "State Plane Coordinate System - Convert, View on Google Earth", \url{https://www.earthpoint.us/StatePlane.aspx}.
#'    \item National Geodetic Survey datasheet95, version 8.12.5.3, online retrieval date July 25, 2019, Mid Valley Oil Rad Relay Twr designation, HA1363 PID, Grayson County Kentucky, Clarkson (1967) USGS Quad, \url{https://www.ngs.noaa.gov/cgi-bin/ds_mark.prl?PidBox=HA1363}.
#'    \item National Geodetic Survey datasheet95, version 8.12.5.3, online retrieval date July 25, 2019, 2006 42 07 designation, DL4005 PID, Fayette County Kentucky, Lexington West (1993) USGS Quad, \url{https://www.ngs.noaa.gov/cgi-bin/ds_mark.prl?PidBox=DL4005}.
#' }
#'
#'
#' @note
#' Please Note: If you have Kentucky North/South Zone survey measurements, then
#' please use the Kentucky Geological Survey, University of Kentucky - Kentucky
#' Single Coordinate Conversion Tool
#' (http://kgs.uky.edu/kgsweb/CoordConversionTool.asp) instead. That tool will
#' give you the geographic coordinates too. This R function, \code{engr_survey_batch}
#' will only be valid for NAD83 / Kentucky Single Zone.
#'
#' Useful Tennessee reference Web site
#' Tennessee Department of Transportation Roadway Design Survey Standards
#' \url{https://www.tn.gov/tdot/roadway-design/survey-standards.html}
#'
#' Useful Kentucky reference Web site
#' Kentucky Transportation Cabinet Survey Coordination
#' \url{https://transportation.ky.gov/Highway-Design/Pages/Survey-Coordination.aspx}
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
#' # Tennessee (TN) Northing and Easting in US Survey foot
#' 
#' library(iemisc)
#'
#' Northing1 <- c("630817.6396", "502170.6065", "562,312.2349", "574,370.7178")
#'
#' Easting1 <- c("2559599.9201", "1433851.6509", "1,843,018.4099", "1,854,896.0041")
#'
#' dt1 <- engr_survey_batch(Northing1, Easting1, "survey_ft", "TN", output = "basic")
#'
#'
#'
#' 
#' \donttest{
#' # See Source 6 and Source 7
#' 
#' # Please see the error messages
#' 
#' library(iemisc)
#'
#' Northing1 <- c("630817.6396", "502170.6065", "562,312.2349", "574,370.7178")
#'
#' Easting1 <- c("2559599.9201", "1433851.6509", "1,843,018.4099", "1,854,896.0041")
#'
#' Northing2 <- c(232489.480, 234732.431)
#' 
#' Easting2 <- c(942754.124, 903795.239)
#' 
#' 
#' dt1A <- try(engr_survey_batch(Northing1[1], Easting1[1], "survey_ft", "TN",
#' output = "basic"))
#' dt1A # first set of Northing, Easting points
#'
#' dt1B <- try(engr_survey_batch(Northing1[2], Easting1[2], "survey_ft", "TN",
#' output = "basic"))
#' dt1B # second set of Northing, Easting points
#'
#' dt1C <- try(engr_survey_batch(Northing1[3], Easting1[3], "survey_ft", "TN",
#' output = "basic"))
#' dt1C # third set of Northing, Easting points
#'
#' dt1D <- try(engr_survey_batch(Northing1[4], Easting1[4], "survey_ft", "TN",
#' output = "basic"))
#' dt1D # fourth set of Northing, Easting points
#'
#'
#' dt4A <- try(engr_survey_batch(Northing2[1], Easting2[1], "meters", "TN",
#' output = "table"))
#' dt4A
#'
#' dt4B <- try(engr_survey_batch(Northing2[2], Easting2[2], "meters", "TN",
#' output = "table"))
#' dt4B
#' }
#'
#'
#'
#'
#'
#' @importFrom data.table data.table setnames copy set setattr setDF rbindlist
#' @importFrom stringi stri_detect_fixed stri_replace_all_fixed stri_length stri_extract_first_regex
#' @importFrom sf st_as_sf st_transform st_coordinates
#' @importFrom units set_units make_units
#' @importFrom checkmate qtest
#' @importFrom assertthat assert_that
#' @importFrom measurements conv_unit
#'
#' @export
engr_survey_batch <- function (Northing, Easting, units = c("survey_ft", "foot", "meters"), location = c("KY", "TN"), output = c("basic", "table")) {


m <- NULL
# due to NSE notes in R CMD check


units <- units

location <- location

output <- output



if (stri_detect_fixed(paste(Northing, collapse = "|"), ",") == TRUE)  {

Northing <- stri_replace_all_fixed(Northing, ",", "")

} else {

Northing <- Northing

}


if (stri_detect_fixed(paste(Easting, collapse = "|"), ",") == TRUE)  {

Easting <- stri_replace_all_fixed(Easting, ",", "")

} else {

Easting <- Easting

}



# Check for Northing, Easting, units, output, and location
assert_that(!any(qtest(as.numeric(Northing), "N+[0,)") == FALSE), msg = "Northing is NA, NaN, Inf, -Inf, a string, empty. Please try again.")
# only process with finite values and provide a stop warning if not

assert_that(!any(qtest(as.numeric(Easting), "N+[0,)") == FALSE), msg = "Easting is NA, NaN, Inf, -Inf, a string, empty. Please try again.")
# only process with finite values and provide a stop warning if not

assert_that(qtest(as.numeric(Northing), "N==1") == FALSE, msg = "There is only 1 Northing point. If you wish to only obtain the coordinates for only 1 location, then please use the 'engr_survey' function instead. Please try again.")
# only process with multiple Northing points and provide a stop warning if not

assert_that(qtest(as.numeric(Easting), "N==1") == FALSE, msg = "There is only 1 Easting point. If you wish to only obtain the coordinates for only 1 location, then please use the 'engr_survey' function instead. Please try again.")
# only process with multiple Easting points and provide a stop warning if not

assert_that(qtest(location, "S==1"), msg = "There is not a location type or more than 1 location type. Please specify either 'KY' or 'TN'. Please try again.")
# only process with a single string value and provide a stop warning if not

assert_that(qtest(units, "S==1"), msg = "units should only be a single character vector. Please specify either 'survey_ft', 'foot', or 'meters'. Please try again.")
# only process with a single string value and provide a stop warning if not

assert_that(qtest(output, "S==1"), msg = "output should only be a single character vector. Please specify either 'basic' or 'table'. Please try again.")
# only process with a single string value and provide a stop warning if not

assert_that(isTRUE(any(c("survey_ft", "foot", "meters") %in% units)), msg = "Incorrect unit selection. The only possible units are 'survey_ft', 'foot', and 'meters'. Please try again.")
# only process with a specified unit and provide a stop warning if not
# Source 1

assert_that(isTRUE(any(c("KY", "TN") %in% location)), msg = "Incorrect location selection. The only possible locations are 'KY' and 'TN'. Please try again.")
# only process with a specified location and provide a stop warning if not
# Source 1

assert_that(isTRUE(any(c("basic", "table") %in% output)), msg = "Incorrect output selection. The only possible outputs are 'basic' and 'table'. Please try again.")
# only process with a specified output and provide a stop warning if not
# Source 1


Northing <- as.numeric(stri_replace_all_fixed(Northing, ",", ""))

Easting <- as.numeric(stri_replace_all_fixed(Easting, ",", ""))

# determine number of decimal places in Northing
for_decimal_places_N <- stri_split_fixed(Northing, ".", omit_empty = NA, simplify = NA)

for_decimal_places_N1 <- as.numeric(for_decimal_places_N[, 1])

for_decimal_places_N2 <- for_decimal_places_N[, 2]

decimal_places_N <- stri_length(for_decimal_places_N2)


# determine number of decimal places in Easting
for_decimal_places_E <- stri_split_fixed(Northing, ".", omit_empty = NA, simplify = NA)

for_decimal_places_E1 <- as.numeric(for_decimal_places_E[, 1])

for_decimal_places_E2 <- for_decimal_places_E[, 2]

decimal_places_E <- stri_length(for_decimal_places_E2)


if (units == "survey_ft") {

distance <- data.table(Easting = Easting, Northing = Northing)

change_class <- names(distance)

for (col in change_class) set(distance, j = col, value = set_units(distance[[col]], "US_survey_foot"))
# Source 3

units(distance$Easting) <- make_units(m)

units(distance$Northing) <- make_units(m)

for (col in change_class) set(distance, j = col, value = as.numeric(distance[[col]]))
# Source 3

data <- data.table(X = distance[, Easting], Y = distance[, Northing])
# Easting, Northing


} else if (units == "foot") {

distance <- data.table(Easting = Easting, Northing = Northing)

change_class <- names(distance)

for (col in change_class) set(distance, j = col, value = set_units(distance[[col]], "ft"))
# Source 3

units(distance$Easting) <- make_units(m)

units(distance$Northing) <- make_units(m)

for (col in change_class) set(distance, j = col, value = as.numeric(distance[[col]]))
# Source 3


data <- data.table(X = distance[, Easting], Y = distance[, Northing])
# Easting, Northing

} else if (units == "meters") {


distance <- data.table(Easting = Easting, Northing = Northing)

data <- data.table(X = distance[, Easting], Y = distance[, Northing])
# Easting, Northing

}


setnames(data, c("X", "Y"))



if (location == "KY") {

# Source 2 begins
# using sf instead of sp now
data_sf <- st_as_sf(data, coords = 1:2, crs = "+init=epsg:3088")
# where X and Y stand for Longitude / Latitude columns
# coordinates are in NAD83 / Kentucky
# Source 2 ends

data_projected <- st_transform(data_sf, "+init=epsg:4326")
# transform the coordinates

data_check <- as.data.table(st_coordinates(data_projected))
# retrieve the coordinates as a matrix and then convert to a data.table



assert_that(all(lat_long2state(data_check[, Y], data_check[, X]) == "Kentucky"), msg = "Latitude and Longitude coordinate pair is not within Kentucky. Please check the Easting and try again. Are the units in US survey foot or meters?")
# Source 1


} else if (location == "TN") {


# Source 2 begins
# using sf instead of sp now
data_sf <- st_as_sf(data, coords = 1:2, crs = "+init=epsg:32136")
# where X and Y stand for Longitude / Latitude columns
# coordinates are in NAD83 / Tennessee
# Source 2 ends

data_projected <- st_transform(data_sf, "+init=epsg:4326")
# transform the coordinates

data_check <- as.data.table(st_coordinates(data_projected))
# retrieve the coordinates as a matrix and then convert to a data.table


assert_that(all(lat_long2state(data_check[, Y], data_check[, X]) == "Tennessee"), msg = "Latitude and Longitude coordinate pair is not within Tennessee. Please check the Easting and try again. Are the units in US survey foot or meters?")

}



# populate the table fields
look_fips <- data.table(location = c("KY", "TN"), zone = c("Kentucky (Single Zone) 1600", "Tennessee 4100"))

z <- lookupQT(location, setDF(look_fips))
# Source 4

dn <- data_check$Y
dw <- data_check$X

# Source 5 begin
dmn <- conv_unit(data_check$Y, "dec_deg", "deg_dec_min")
dmw <- conv_unit(data_check$X, "dec_deg", "deg_dec_min")
dmsn <- conv_unit(data_check$Y, "dec_deg", "deg_min_sec")
dmsw <- conv_unit(data_check$X, "dec_deg", "deg_min_sec")
# Source 5 end

dn1 <- stri_extract_first_regex(dn, "^\\d*\\.\\d{5}")

dw1 <- stri_extract_first_regex(dw, "^.\\d*\\.\\d{5}")

dmn1 <- stri_extract_first_regex(dmn, "^.+\\d*\\.\\d{5}")

dmw1 <- stri_extract_first_regex(dmw, "^.+\\d*\\.\\d{5}")

dmsn1 <- stri_extract_first_regex(dmsn, "^.+\\d*\\.\\d{5}")

dmsw1 <- stri_extract_first_regex(dmsw, "^.+\\d*\\.\\d{5}")

nm <- set_units(data$Y, "m")
nm <- round(nm, digits = decimal_places_N)

em <- set_units(data$X, "m")
em <- round(em, digits = decimal_places_E)

nu <- nm
nu <- set_units(nu, "US_survey_foot")
nu <- round(nu, digits = decimal_places_N)

eu <- em
eu <- set_units(eu, "US_survey_foot")
eu <- round(eu, digits = decimal_places_E)

ni <- nm
ni <- set_units(ni, "ft")
ni <- round(ni, digits = decimal_places_N)

ei <- em
ei <- set_units(ei, "ft")
ei <- round(ei, digits = decimal_places_E)

z <- z

if(output == "basic") {

return(data_projected)


} else if (output == "table") {


section_length <- length(Northing)

ll <- vector("list", section_length)

for (i in seq_along(Northing)) {

# table inspired by Earth Point Reference
ll[[i]] <- data.table(names = c("Degrees (Latitude, Longitude)", "Degrees Minutes (Latitude, Longitude)", "Degrees Minutes Seconds (Latitude, Longitude)", "State Plane (X = East, Y = North) [meters]", "State Plane (X = East, Y = North) [US survey foot]", "State Plane (X = East, Y = North) [international foot]"), numbers = c(paste0(dn1[[i]], ", ", dw1[[i]]), paste0(dmn1[[i]], ", ", dmw1[[i]]), paste0(dmsn1[[i]], ", ", dmsw1[[i]]), paste0(z, " ", em[[i]],", ", nm[[i]]), paste0(z, " ", eu[[i]],", ", nu[[i]]), paste0(z, " ", ei[[i]],", ", ni[[i]])))

}

lle <- ll

DT_out <- rbindlist(lle)


setnames(DT_out, c("Parameters", "Value"))

col.names <- c("Parameters", "Value")
colnames(DT_out) <- col.names

# code block below modified from data.table function
setattr(DT_out, "col.names", setnames(DT_out, col.names))
setattr(DT_out, "class", c("data.table", "data.frame"))
DT_out

}
}
