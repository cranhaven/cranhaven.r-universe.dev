#' Calculate the midpoint between two coordinates (KY and TN)
#'
#' Takes Kentucky or Tennessee-based Northing and Easting engineering survey
#' measurements [based in the State Plane Coordinate System (SPCS)] in meters,
#' international foot, or US survey foot and converts those values into
#' geodetic coordinates of the World Geodetic System (WGS) (19)84 (EPSG:4326).
#' [MapTiler Reference] Each latitude [Y] and longitude [X] point is verified to be
#' located within Kentucky or Tennessee.
#'
#'
#'
#' @param Northing_begin numeric vector (or character vector with numbers, commas,
#'     and decimal points) that contains the Northing engineering survey
#'     measurement in meters, international foot, or US survey foot
#' @param Northing_end numeric vector (or character vector with numbers, commas,
#'     and decimal points) that contains the Northing engineering survey
#'     measurement in meters, international foot, or US survey foot
#' @param Easting_begin numeric vector (or character vector with numbers, commas,
#'     and decimal points) that contains the Northing engineering survey
#'     measurement in meters, international foot, or US survey foot
#' @param Easting_end numeric vector (or character vector with numbers, commas,
#'     and decimal points) that contains the Northing engineering survey
#'     measurement in meters, international foot, or US survey foot
#' @param units character vector that contains the system of units (options are
#'     \code{survey_ft} (United States Customary System) [US survey foot],
#'     \code{foot}, or \code{meters} (International System of Units) [meters]
#'     [only 1 set of units at a time])
#' @param location character vector that contains the location name ('KY' for
#'     Kentucky or 'TN' for Tennessee) [only 1 location at a time]
#' @param output character vector that contains simple for the default result
#'     using a simple \code{\link[data.table]{data.table}} or advanced for the result as a
#'     complex \code{\link[data.table]{data.table}}
#'
#'
#' @return the projected associated latitude [Y] and longitude [X] mid point
#'   coordinates in Decimal Degrees as a \code{\link[data.table]{data.table}} or as an enhanced
#'   \code{\link[data.table]{data.table}} with the Northing and Easting coordinates in US
#'   survey foot, international foot, and meters in addition to the [Y] and [X]
#'   coordinates for the begin, middle, and end points
#'      
#'
#'
#' @source
#' \enumerate{
#'    \item Win-Vector Blog. John Mount, June 11, 2018, "R Tip: use isTRUE()", \url{https://win-vector.com/2018/06/11/r-tip-use-istrue/}.
#'    \item Latitude Longitude Coordinates to State Code in R - Stack Overflow answered by Josh O'Brien on Jan 6 2012 and edited by Josh O'Brien on Jun 18, 2020. See \url{https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r}.
#'    \item r - Convert column classes in data.table - Stack Overflow answered by Matt Dowle on Dec 27 2013. See \url{https://stackoverflow.com/questions/7813578/convert-column-classes-in-data-table}.
#'    \item Excel vlook up function in R for data frame - Stack Overflow answered by Tyler Rinker on Apr 8 2013 and edited by Tyler Rinker on Feb 26 2014. See \url{https://stackoverflow.com/questions/15882743/excel-vlook-up-function-in-r-for-data-frame}.
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
#' give you the geographic coordinates too. This R function, \code{project_midpoint}
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
#' # Example 1
#' 
#' library(iemisc)
#' 
#' Northing_begin <- 283715.8495
#' Easting_begin <- 1292428.3999
#'
#' Northing_end <- 303340.6977
#' Easting_end <- 1295973.7743
#'
#' project_midpoint(Northing_begin, Easting_begin, Northing_end, Easting_end,
#' units = "survey_ft", location = "TN", output = "simple")
#'
#'
#'
#'
#'
#' 
#' \donttest{
#' # See Source 5 and Source 6
#' 
#' # Please see the error messages
#' 
#' library(iemisc)
#' 
#' # Tennessee (TN) Northing and Easting in meters
#' 
#' Northing2 <- c(232489.480, 234732.431)
#'
#' Easting2 <- c(942754.124, 903795.239)
#'
#' dt4 <- try(project_midpoint(Northing2, Easting2, units = "survey_ft",
#' location = "TN", output = "simple"))
#' }
#'
#'
#'
#'
#'
#' @importFrom data.table data.table setnames copy set setattr setDF
#' @importFrom stringi stri_detect_fixed stri_replace_all_fixed
#' @importFrom sf st_as_sf st_transform st_coordinates 
#' @importFrom units set_units make_units
#' @importFrom checkmate qtest
#' @importFrom assertthat assert_that
#' @importFrom geosphere midPoint
#'
#' @export
project_midpoint <- function (Northing_begin, Easting_begin, Northing_end, Easting_end, units = c("survey_ft", "foot", "meters"), location = c("KY", "TN"), output = c("simple", "advanced")) {


m <- NULL
# due to NSE notes in R CMD check


units <- units

location <- location


ifelse(stri_detect_fixed(Northing_begin, ",") == TRUE, Northing_begin <- stri_replace_all_fixed(Northing_begin, ",", ""), Northing_begin <- Northing_begin)

ifelse(stri_detect_fixed(Easting_begin, ",") == TRUE, Easting_begin <- stri_replace_all_fixed(Easting_begin, ",", ""), Easting_begin <- Easting_begin)

ifelse(stri_detect_fixed(Northing_end, ",") == TRUE, Northing_end <- stri_replace_all_fixed(Northing_end, ",", ""), Northing_end <- Northing_end)

ifelse(stri_detect_fixed(Easting_end, ",") == TRUE, Easting_end <- stri_replace_all_fixed(Easting_end, ",", ""), Easting_end <- Easting_end)



# Check for Northing_begin, Easting_begin, Northing_end, Easting_end, units, and location
assert_that(!any(qtest(as.numeric(Northing_begin), "N==1[0,)") == FALSE), msg = "Northing_begin is NA, NaN, Inf, -Inf, a string, empty Or contains more than 1 value. Please try again.")
# only process with finite values and provide a stop warning if not

assert_that(!any(qtest(as.numeric(Easting_begin), "N==1[0,)") == FALSE), msg = "Easting_begin is NA, NaN, Inf, -Inf, a string, empty Or contains more than 1 value. Please try again.")
# only process with finite values and provide a stop warning if not

assert_that(!any(qtest(as.numeric(Northing_end), "N==1[0,)") == FALSE), msg = "Northing_end is NA, NaN, Inf, -Inf, a string, empty Or contains more than 1 value. Please try again.")
# only process with finite values and provide a stop warning if not

assert_that(!any(qtest(as.numeric(Easting_end), "N==1[0,)") == FALSE), msg = "Easting_end is NA, NaN, Inf, -Inf, a string, empty Or contains more than 1 value. Please try again.")
# only process with finite values and provide a stop warning if not

assert_that(qtest(location, "S==1"), msg = "There is not a location type or more than 1 location type. Please specify either 'KY' or 'TN'. Please try again.")
# only process with a single string value and provide a stop warning if not

assert_that(qtest(units, "S==1"), msg = "units should only be a single character vector. Please specify either 'survey_ft', 'foot', or 'meters'. Please try again.")
# only process with a single string value and provide a stop warning if not

assert_that(qtest(output, "S==1"), msg = "output should only be a single character vector. Please specify either 'simple' or 'advanced'. Please try again.")
# only process with a single string value and provide a stop warning if not

assert_that(isTRUE(any(c("KY", "TN") %in% location)), msg = "Incorrect location selection. The only possible locations are 'KY' and 'TN'. Please try again.")
# only process with a specified location and provide a stop warning if not
# Source 1

assert_that(isTRUE(any(c("survey_ft", "foot", "meters") %in% units)), msg = "Incorrect unit selection. The only possible units are 'survey_ft', 'foot', and 'meters'. Please try again.")
# only process with a specified unit and provide a stop warning if not
# Source 1

assert_that(isTRUE(any(c("simple", "advanced") %in% output)), msg = "Incorrect output selection. The only possible outputs are 'simple' and 'advanced'. Please try again.")
# only process with a specified output and provide a stop warning if not
# Source 1



Northing_begin <- as.numeric(stri_replace_all_fixed(Northing_begin, ",", ""))

Easting_begin <- as.numeric(stri_replace_all_fixed(Easting_begin, ",", ""))


if (units == "survey_ft") {

distance_begin <- data.table(Easting_begin = Easting_begin, Northing_begin = Northing_begin)

change_class <- names(distance_begin)

for (col in change_class) set(distance_begin, j = col, value = set_units(distance_begin[[col]], "US_survey_foot"))
# Source 3

units(distance_begin$Easting_begin) <- make_units(m)

units(distance_begin$Northing_begin) <- make_units(m)

for (col in change_class) set(distance_begin, j = col, value = as.numeric(distance_begin[[col]]))
# Source 3

data_begin <- data.table(X = distance_begin[, Easting_begin], Y = distance_begin[, Northing_begin])
# Easting_begin, Northing_begin


} else if (units == "foot") {

distance_begin <- data.table(Easting_begin = Easting_begin, Northing_begin = Northing_begin)

change_class <- names(distance_begin)

for (col in change_class) set(distance_begin, j = col, value = set_units(distance_begin[[col]], "ft"))
# Source 3

units(distance_begin$Easting_begin) <- make_units(m)

units(distance_begin$Northing_begin) <- make_units(m)

for (col in change_class) set(distance_begin, j = col, value = as.numeric(distance_begin[[col]]))
# Source 3


data_begin <- data.table(X = distance_begin[, Easting_begin], Y = distance_begin[, Northing_begin])
# Easting_begin, Northing_begin

} else if (units == "meters") {


distance_begin <- data.table(Easting_begin = Easting_begin, Northing_begin = Northing_begin)

data_begin <- data.table(X = distance_begin[, Easting_begin], Y = distance_begin[, Northing_begin])
# Easting_begin, Northing_begin

}


setnames(data_begin, c("X", "Y"))

data_out_begin <- copy(data_begin)



Northing_end <- as.numeric(stri_replace_all_fixed(Northing_end, ",", ""))

Easting_end <- as.numeric(stri_replace_all_fixed(Easting_end, ",", ""))


if (units == "survey_ft") {

distance_end <- data.table(Easting_end = Easting_end, Northing_end = Northing_end)

change_class <- names(distance_end)

for (col in change_class) set(distance_end, j = col, value = set_units(distance_end[[col]], "US_survey_foot"))
# Source 3

units(distance_end$Easting_end) <- make_units(m)

units(distance_end$Northing_end) <- make_units(m)

for (col in change_class) set(distance_end, j = col, value = as.numeric(distance_end[[col]]))
# Source 3

data_end <- data.table(X = distance_end[, Easting_end], Y = distance_end[, Northing_end])
# Easting_end, Northing_end


} else if (units == "foot") {

distance_end <- data.table(Easting_end = Easting_end, Northing_end = Northing_end)

change_class <- names(distance_end)

for (col in change_class) set(distance_end, j = col, value = set_units(distance_end[[col]], "ft"))
# Source 3

units(distance_end$Easting_end) <- make_units(m)

units(distance_end$Northing_end) <- make_units(m)

for (col in change_class) set(distance_end, j = col, value = as.numeric(distance_end[[col]]))
# Source 3


data_end <- data.table(X = distance_end[, Easting_end], Y = distance_end[, Northing_end])
# Easting_end, Northing_end

} else if (units == "meters") {


distance_end <- data.table(Easting_end = Easting_end, Northing_end = Northing_end)

data_end <- data.table(X = distance_end[, Easting_end], Y = distance_end[, Northing_end])
# Easting_end, Northing_end

}


setnames(data_end, c("X", "Y"))

data_out_end <- copy(data_end)


# check location

if (location == "KY") {

# Source 2 begins
# using sf instead of sp now
data_begin_sf <- st_as_sf(data_begin, coords = 1:2, crs = "+init=epsg:3088")
# where X and Y stand for Longitude / Latitude columns
# coordinates are in NAD83 / Kentucky
# Source 2 ends

data_projected_begin <- st_transform(data_begin_sf, "+init=epsg:4326")
# transform the coordinates

data_check_begin <- as.data.table(st_coordinates(data_projected_begin))
# retrieve the coordinates as a matrix and then convert to a data.table


assert_that(isTRUE(lat_long2state(data_check_begin[, Y], data_check_begin[, X]) == "Kentucky"), msg = "Latitude and Longitude coordinate pair is not within Kentucky. Please check the Easting and try again. Are the units in US survey foot or meters?")
# Source 1



# Source 2 begins
# using sf instead of sp now
data_end_sf <- st_as_sf(data_end, coords = 1:2, crs = "+init=epsg:3088")
# where X and Y stand for Longitude / Latitude columns
# coordinates are in NAD83 / Kentucky
# Source 2 ends

data_projected_end <- st_transform(data_end_sf, "+init=epsg:4326")
# transform the coordinates

data_check_end <- as.data.table(st_coordinates(data_projected_end))
# retrieve the coordinates as a matrix and then convert to a data.table


assert_that(isTRUE(lat_long2state(data_check_end[, Y], data_check_end[, X]) == "Kentucky"), msg = "Latitude and Longitude coordinate pair is not within Kentucky. Please check the Easting and try again. Are the units in US survey foot or meters?")
# Source 1


} else if (location == "TN") {

# Source 2 begins
# using sf instead of sp now
data_begin_sf <- st_as_sf(data_begin, coords = 1:2, crs = "+init=epsg:32136")
# where X and Y stand for Longitude / Latitude columns
# coordinates are in NAD83 / Tennessee
# Source 2 ends

data_projected_begin <- st_transform(data_begin_sf, "+init=epsg:4326")
# transform the coordinates

data_check_begin <- as.data.table(st_coordinates(data_projected_begin))
# retrieve the coordinates as a matrix and then convert to a data.table


assert_that(isTRUE(lat_long2state(data_check_begin[, Y], data_check_begin[, X]) == "Tennessee"), msg = "Latitude and Longitude coordinate pair is not within Tennessee. Please check the Easting and try again. Are the units in US survey foot or meters?")
# Source 1



# Source 2 begins
# using sf instead of sp now
data_end_sf <- st_as_sf(data_end, coords = 1:2, crs = "+init=epsg:32136")
# where X and Y stand for Longitude / Latitude columns
# coordinates are in NAD83 / Tennessee
# Source 2 ends

data_projected_end <- st_transform(data_end_sf, "+init=epsg:4326")
# transform the coordinates

data_check_end <- as.data.table(st_coordinates(data_projected_end))
# retrieve the coordinates as a matrix and then convert to a data.table


assert_that(isTRUE(lat_long2state(data_check_end[, Y], data_check_end[, X]) == "Tennessee"), msg = "Latitude and Longitude coordinate pair is not within Tennessee. Please check the Easting and try again. Are the units in US survey foot or meters?")
# Source 1
}


# populate the table fields
look_fips <- data.table(location = c("KY", "TN"), zone = c("Kentucky (Single Zone) 1600", "Tennessee 4100"))

z <- lookupQT(location, setDF(look_fips))
# Source 4


# for conversions
# meters
nm_begin <- set_units(data_out_begin$Y, "m")

em_begin <- set_units(data_out_begin$X, "m")


# US survey foof
nu_begin <- nm_begin
nu_begin <- set_units(nu_begin, "US_survey_foot")

eu_begin <- em_begin
eu_begin <- set_units(eu_begin, "US_survey_foot")


# meters
nm_end <- set_units(data_out_end$Y, "m")

em_end <- set_units(data_out_end$X, "m")


# US survey foot
nu_end <- nm_end
nu_end <- set_units(nu_end, "US_survey_foot")

eu_end <- em_end
eu_end <- set_units(eu_end, "US_survey_foot")


# foot
nf_begin <- nm_begin
nf_begin <- set_units(nf_begin, "foot")

ef_begin <- em_begin
ef_begin <- set_units(ef_begin, "foot")


nf_end <- nm_end
nf_end <- set_units(nf_end, "foot")

ef_end <- em_end
ef_end <- set_units(ef_end, "foot")



# for display
dn_begin <- sprintf("%.4f", data_check_begin$Y)
dw_begin <- sprintf("%.4f", data_check_begin$X)

dn_end <- sprintf("%.4f", data_check_end$Y)
dw_end <- sprintf("%.4f", data_check_end$X)

nm_begin <- sprintf("%.4f", nm_begin)
em_begin <- sprintf("%.4f", em_begin)

nu_begin <- sprintf("%.4f", nu_begin)
eu_begin <- sprintf("%.4f", eu_begin)

nm_end <- sprintf("%.4f", nm_end)
em_end <- sprintf("%.4f", em_end)

nu_end <- sprintf("%.4f", nu_end)
eu_end <- sprintf("%.4f", eu_end)

nf_begin <- sprintf("%.4f", nf_begin)
ef_begin <- sprintf("%.4f", ef_begin)

nf_end <- sprintf("%.4f", nf_end)
ef_end <- sprintf("%.4f", ef_end)


# return the midpoint coordinates
mp <- midPoint(data_check_begin, data_check_end)

mp <- as.data.table(mp)

setcolorder(mp, c(2, 1))

setnames(mp, c("Latitude", "Longitude"))


change_class_mp <- c("Latitude", "Longitude")

for (col in change_class_mp) set(mp, j = col, value = sprintf("%.4f", mp[[col]]))
# Source 3


mp_out <- copy(mp)

col.names <- c("Latitude", "Longitude")
colnames(mp_out) <- col.names


if(output == "simple") {

# code block below modified from data.table function
setattr(mp_out, "col.names", setnames(mp_out, col.names))
setattr(mp_out, "class", c("data.table", "data.frame"))
mp_out


} else if (output == "advanced") {

dn_mp <- mp$Latitude
dw_mp <- mp$Longitude

# table inspired by Earth Point Reference
# without the UTM information
mp_out <- data.table(names = c("Begin Project (X = East, Y = North) [US survey foot]", "End Project (X = East, Y = North) [US survey foot]", "Begin Project (X = East, Y = North) [international foot]", "End Project (X = East, Y = North) [international foot]", "Begin Project (X = East, Y = North) [meters]", "End Project (X = East, Y = North) [meters]", "Begin Project Degrees (Latitude, Longitude)", "Midpoint Project Degrees (Latitude, Longitude)", "End Project Degrees (Latitude, Longitude)"), numbers = c(paste0(z, " ", eu_begin,", ", nu_begin), paste0(z, " ", eu_end,", ", nu_end), paste0(z, " ", ef_begin,", ", nf_begin), paste0(z, " ", ef_end,", ", nf_end), paste0(z, " ", em_begin,", ", nm_begin), paste0(z, " ", em_end,", ", nm_end), paste0(dn_begin, ", ", dw_begin), paste0(dn_mp, ", ", dw_mp), paste0(dn_end, ", ", dw_end)))


setnames(mp_out, c("Parameters", "Value"))

col.names <- c("Parameters", "Value")
colnames(mp_out) <- col.names

# code block below modified from data.table function
setattr(mp_out, "col.names", setnames(mp_out, col.names))
setattr(mp_out, "class", c("data.table", "data.frame"))
mp_out

}
}
