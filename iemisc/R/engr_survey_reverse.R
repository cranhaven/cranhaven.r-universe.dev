#' Conversion of Latitude/Longitude Coordinates to Engineering Survey Measurements (KY and TN)
#'
#' Takes geodetic coordinates of the World Geodetic System (WGS) (19)84
#' (EPSG:4326) [MapTiler Reference] and converts those values into Kentucky or
#' Tennessee-based Northing and Easting engineering survey measurements
#' [based in the State Plane Coordinate System (SPCS)] in meters, international
#' foot, or US survey foot. Each latitude [Y] and longitude [X] point is
#' verified to be located within Kentucky or Tennessee.
#'
#'
#' @param latitude numeric vector [or character vector with spaces, degree
#'    symbol, single quotation mark, and/or escaped quotation mark (\")] that
#'    contains the latitude coordinate point. The following possibilities are
#'    valid: -25.02, "25\\U00B056'50.2068\"N"; "15\\U00B056'58.7068"; "37'1'54.3'N";
#'    "35 8 46.44496",  "35'8'46.44496". If the North designation is not
#'    provided, then it will be added. The latitude/longitude coordinate pair
#'    has to be either a numeric or character vector (no mixing).
#' @param longitude numeric vector [or character vector with spaces, degree
#'    symbol, single quotation mark, and/or escaped quotation mark (\")] that
#'    contains the latitude coordinate point. The following possibilities are
#'    valid: 09.83, "25\\U00B056'50.2068\"W"; "15\\U00B056'58.7068"; "37'1'54.3'E";
#'    "35 8 46.44496",  "35'8'46.44496". If the West designation is not
#'    provided, then it will be added. The latitude/longitude coordinate pair
#'    has to be either a numeric or character vector (no mixing).
#' @param units character vector that contains the system of units (options are
#'     \code{survey_ft} (United States Customary System) [US survey foot],
#'     \code{foot}, or \code{meters} (International System of Units) [meters])
#' @param location character vector that contains the location name ('KY' for
#'     Kentucky or 'TN' for Tennessee)
#' @param output character vector that contains basic for the default result
#'     using a simple \code{\link[data.table]{data.table}} or table for the result as a
#'     complex \code{\link[data.table]{data.table}}
#' @param utm numeric vector that contains 0 or 1 only. 0 represents do not
#'     provide the utm coordinates and 1 is to provide the utm coordinates
#'
#'
#'
#' @return the geodetic coordinates as projected SPCS Northing and Easting
#'    coordinates as a \code{\link[data.table]{data.table}}
#'
#'
#' @source
#' \enumerate{
#'    \item Win-Vector Blog. John Mount, June 11, 2018, "R Tip: use isTRUE()", \url{https://win-vector.com/2018/06/11/r-tip-use-istrue/}.
#'    \item Latitude Longitude Coordinates to State Code in R - Stack Overflow answered by Josh O'Brien on Jan 6 2012 and edited by Josh O'Brien on Jun 18, 2020. See \url{https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r}.
#'    \item r - Convert column classes in data.table - Stack Overflow answered by Matt Dowle on Dec 27 2013. See \url{https://stackoverflow.com/questions/7813578/convert-column-classes-in-data-table}.
#'    \item Excel vlook up function in R for data frame - Stack Overflow answered by Tyler Rinker on Apr 8 2013 and edited by Tyler Rinker on Feb 26 2014. See \url{https://stackoverflow.com/questions/15882743/excel-vlook-up-function-in-r-for-data-frame}.
#'    \item r - Converting geo coordinates from degree to decimal - Stack Overflow answered by Robbes on Jan 3 2018 and edited by ayaio on Jan 3 2018. See \url{https://stackoverflow.com/questions/14404596/converting-geo-coordinates-from-degree-to-decimal}.
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
#' give you the geographic coordinates too. This R function, \code{engr_survey_reverse}
#' will only be valid for NAD83 / Kentucky Single Zone.
#'
#' Useful Tennessee reference Web site
#' Tennessee Department of Transportation Roadway Design Survey Standards
#' (https://www.tn.gov/tdot/roadway-design/survey-standards.html)
#'
#' Useful Kentucky reference Web site
#' Kentucky Transportation Cabinet Survey Coordination
#' (https://transportation.ky.gov/Highway-Design/Pages/Survey-Coordination.aspx)
#'
#'
#'
#'
#'
#'
#' @author Irucka Embry, Berry Boessenkool (a couple of functions are sourced from OSMscale)
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
#' # Test against TGRN Manual (Reference 5)
#' # using the 1983 (1995) DATUM
#' # GPS 60 is the station name with these coordinates
#' # latitude (North) = 35 8 46.44496
#' # longitude (West) = 89 54 24.04763
#'
#' # Northing is 97296.815 # provided in TGRN Manual
#' # Easting is 244089.427 # provided in TGRN Manual
#'
#' library(iemisc)
#'
#' latitude <- "35 8 46.44496"
#' longitude <- "89 54 24.04763"
#'
#' Northing_test2 <- 97296.815 # provided in TGRN Manual
#' Easting_test2 <- 244089.427 # provided in TGRN Manual
#'
#' tgrn2A <- engr_survey_reverse(latitude, longitude, "meters", "TN", output = "table",
#' utm = 0)
#' tgrn2A
#'
#' tgrn2B <- engr_survey(Northing_test2, Easting_test2, "meters", "TN", output = "table",
#' utm = 0)
#' tgrn2B
#'
#'
#'
#'
#' # Example 1
#'
#' # Tennessee
#'
#' library(iemisc)
#'
#' lat <- 35.8466965
#'
#' long <- -88.9206794
#'
#' dt1B <- engr_survey_reverse(lat, long, units = "survey_ft", location = "TN", output =
#' "basic", utm = 1)
#' dt1B
#'
#'
#'
#'
#' # Example 2
#' 
#' # Kentucky
#'
#' library(iemisc)
#'
#' lats <- "37'50'21.5988''N"
#' longs <- "84'16'12.0720'W"
#'
#' dt2A <- engr_survey_reverse(lats, longs, "foot", "KY", output = "basic", utm = 0)
#' dt2A
#'
#'
#'
#'
#'
#' @importFrom data.table setnames setattr copy setDF setDT as.data.table
#' @importFrom stringi stri_replace_all_fixed stri_detect_fixed stri_extract_first_regex
#' @importFrom sf st_as_sf st_transform st_coordinates st_crs
#' @importFrom units set_units make_units drop_units
#' @importFrom checkmate qtest
#' @importFrom assertthat assert_that
#' @importFrom round round_r3
#' @importFrom berryFunctions l2df traceCall getColumn
#' @importFrom measurements conv_unit
#'
#' @export
engr_survey_reverse <- function (latitude, longitude, units = c("survey_ft", "foot", "meters"), location = c("KY", "TN"), output = c("basic", "table"), utm = c(0, 1)) {


m <- ft <- cm <- US_survey_foot <- NULL
# due to NSE notes in R CMD check


units <- units

location <- location

output <- output

utm <- utm



ifelse(stri_detect_fixed(latitude, " ") == TRUE, latitude <- stri_replace_all_fixed(latitude, " ", "'"), latitude <- latitude)

ifelse(stri_detect_fixed(longitude, " ") == TRUE, longitude <- stri_replace_all_fixed(longitude, " ", "'"), longitude <- longitude)


# Check for latitude, longitude, units, output, and location
assert_that(!any(qtest(as.character(latitude), "S==1") == FALSE), msg = "There is more than 1 latitude point. If you wish to obtain the coordinates for more than 1 location, then please use the 'engr_survey_reverse_batch' function instead. Please try again.")
# only process with a single latitude point and provide a stop warning if not

assert_that(!any(qtest(as.character(longitude), "S==1") == FALSE), msg = "There is more than 1 longitude value. If you wish to obtain the coordinates for more than 1 location, then please use the 'engr_survey_reverse_batch' function instead. Please try again.")
# only process with a single longitude value and provide a stop warning if not

assert_that(qtest(location, "S==1"), msg = "There is not a location type or more than 1 location type. Please specify either 'symmetrical' or 'non-symmetrical'.")
# only process with a single string value and provide a stop warning if not

assert_that(qtest(units, "S==1"), msg = "units should only be a single character vector. Please try again.")
# only process with a single string value and provide a stop warning if not

assert_that(qtest(output, "S==1"), msg = "output should only be a single character vector. Please try again.")
# only process with a single string value and provide a stop warning if not

assert_that(isTRUE(any(c("survey_ft", "foot", "meters") %in% units)), msg = "Incorrect unit selection. The only possible units are 'survey_ft', 'foot', and 'meters'. Please try again.")
# only process with a specified unit and provide a stop warning if not
# Source 1

assert_that(isTRUE(any(c("KY", "TN") %in% location)), msg = "Incorrect location selection. The only possible locations are 'KY' and 'TN'. Please try again.")
# only process with a specified location and provide a stop warning if not
# Source 1

assert_that(isTRUE(any(c("basic", "table") %in% output)), msg = "Incorrect output selection. The only possible locations are 'basic' and 'table'. Please try again.")
# only process with a specified output and provide a stop warning if not
# Source 1

assert_that(qtest(utm, "N==1[0,1]"), msg = "utm should only be a single numeric value of 0 for do not provide the utm coordinates or 1 to provide the utm coordinates. Please try again.")
# only process with a single numeric value of 0 or 1 and provide an error message if the check fails


# check if latitude is valid for degree_coord function or not. If not, then make it valid.
if (is.numeric(latitude) == TRUE) {

latitude <- latitude

} else {

ifelse (stri_detect_fixed(latitude, "degrees") | stri_detect_fixed(latitude, "'") & stri_detect_fixed(latitude, "\"N") | stri_detect_fixed(latitude, "'N") == TRUE, latitude_check <- TRUE, latitude_check <- FALSE)

ifelse(latitude_check == TRUE, latitude <- latitude, latitude <- latitude %+% "'N")

}



# check if longitude is valid for degree_coord function or not. If not, then make it valid.
if (is.numeric(longitude) == TRUE) {

longitude <- longitude

} else {

ifelse (stri_detect_fixed(longitude, "degrees") | stri_detect_fixed(longitude, "'") & stri_detect_fixed(longitude, "\"W") | stri_detect_fixed(longitude, "'W") == TRUE, longitude_check <- TRUE, longitude_check <- FALSE)

ifelse(longitude_check == TRUE, longitude <- longitude, longitude <- longitude %+% "'W")

}


# change unit representation for the projection
if (units == "survey_ft") {

uniter <- "us-ft"

} else if (units == "foot") {

uniter <- "ft"

} else if (units == "meters") {

uniter <- "m"

}




if (is.numeric(longitude) & is.numeric(latitude) == TRUE) {

data_pre <- data.table(lat = latitude, long = longitude)

} else {

data_pre <- setDT(degree_coord(latitude, longitude, digits = 15))

}


data <- copy(data_pre)

data <- setcolorder(data,  c("long", "lat"))
# where X = Easting (Longitude), Y  = Northing (Latitude)

setnames(data, c("X", "Y"))


if (location == "KY") {

assert_that(isTRUE(lat_long2state(data[, Y], data[, X]) == "Kentucky"), msg = "Latitude and Longitude coordinate pair is not within Kentucky. Please check the coordinates again.")
# Source 1

# Source 2 begins
# using sf instead of sp now
data_sf <- st_as_sf(data, coords = 1:2, crs = "+init=epsg:4326")
# where X and Y stand for Longitude / Latitude columns
# coordinates are in NAD83 / Kentucky
# Source 2 ends

data_projected <- st_transform(data_sf, paste0("+init=epsg:3088",  " +units=", uniter))
# transform the coordinates

data_check <- as.data.table(st_coordinates(data_projected))
# retrieve the coordinates as a matrix and then convert to a data.table


easting <- data_check[, X]
northing <-  data_check[, Y]

distance <- data.table(Easting = easting, Northing = northing)

data_place <- data.table(X = distance[, Easting], Y = distance[, Northing])
# Easting, Northing


} else if (location == "TN") {

assert_that(isTRUE(lat_long2state(data[, Y], data[, X]) == "Tennessee"), msg = "Latitude and Longitude coordinate pair is not within Tennessee. Please check the coordinates again.")
# Source 1

# Source 2 begins
# using sf instead of sp now
data_sf <- st_as_sf(data, coords = 1:2, crs = "+init=epsg:4326")
# where X and Y stand for Longitude / Latitude columns
# coordinates are in NAD83 / Tennessee
# Source 2 ends

data_projected <- st_transform(data_sf, paste0("+init=epsg:32136",  " +units=", uniter))
# transform the coordinates

data_check <- as.data.table(st_coordinates(data_projected))
# retrieve the coordinates as a matrix and then convert to a data.table


easting <- data_check[, X]
northing <-  data_check[, Y]

distance <- data.table(Easting = easting, Northing = northing)

}


distance_survey <- copy(distance)


if (units == "meters") {

change_class <- names(distance_survey)

for (col in change_class) set(distance_survey, j = col, value = set_units(distance_survey[[col]], "m"))
# Source 3

data_out <- data.table(X = distance_survey[, Easting], Y = distance_survey[, Northing])
# Easting, Northing


} else if (units == "survey_ft") {

distance_survey_US <- copy(distance_survey)

units(distance_survey_US$Easting) <- make_units(US_survey_foot)

units(distance_survey_US$Northing) <- make_units(US_survey_foot)

distance_survey <- distance_survey_US

units(distance_survey$Easting) <- make_units(m)

units(distance_survey$Northing) <- make_units(m)

data_out <- data.table(X = distance_survey[, Easting], Y = distance_survey[, Northing])
# Easting, Northing


} else if (units == "foot") {

distance_survey_ft <- copy(distance_survey)

units(distance_survey_ft$Easting) <- make_units(ft)

units(distance_survey_ft$Northing) <- make_units(ft)

distance_survey <- distance_survey_ft

units(distance_survey$Easting) <- make_units(m)

units(distance_survey$Northing) <- make_units(m)

data_out <- data.table(X = distance_survey[, Easting], Y = distance_survey[, Northing])
# Easting, Northing

}



# populate the table fields
look_fips <- data.table(location = c("KY", "TN"), zone = c("Kentucky (Single Zone) 1600", "Tennessee 4100"))

z <- lookupQT(location, setDF(look_fips))
# Source 4

dn <- data_pre$lat

dw <- data_pre$long


# Source 5 begin
dmn <- conv_unit(data_pre$lat, "dec_deg", "deg_dec_min")
dmw <- conv_unit(data_pre$long, "dec_deg", "deg_dec_min")
dmsn <- conv_unit(data_pre$lat, "dec_deg", "deg_min_sec")
dmsw <- conv_unit(data_pre$long, "dec_deg", "deg_min_sec")
# Source 5 end

dn1 <- round_r3(dn, d = 5)

dw1 <- round_r3(dw, d = 5)

dmn1 <- stri_extract_first_regex(dmn, "^.+\\d*\\.\\d{5}")

dmw1 <- stri_extract_first_regex(dmw, "^.+\\d*\\.\\d{5}")

dmsn1 <- stri_extract_first_regex(dmsn, "^.+\\d*\\.\\d{5}")

dmsw1 <- stri_extract_first_regex(dmsw, "^.+\\d*\\.\\d{5}")


nm <- data_out$Y

em <- data_out$X

nu <- nm
nu <- set_units(nu, "US_survey_foot")
nu <- round_r3(drop_units(nu), d = 4)

eu <- em
eu <- set_units(eu, "US_survey_foot")
eu <- round_r3(drop_units(eu), d = 4)

ni <- nm
ni <- set_units(ni, "ft")
ni <- round_r3(drop_units(ni), d = 4)

ei <- em
ei <- set_units(ei, "ft")
ei <- round_r3(drop_units(ei), d = 4)


nm <- round_r3(drop_units(nm), d = 4)
em <- round_r3(drop_units(em), d = 4)

z <- z


if (utm == 1) {

# obtain the UTM zone and UTM coordinates in meters
# calls exported lat_long2utm exported function
utm <- lat_long2utm(data_pre$lat, data_pre$long, uniter, "table")

# column 2 of utm
zone <- utm[[2]]

# column 3 of utm
utm_x <- utm[[3]]

# column 4 of utm
utm_y <- utm[[4]]



if (uniter == "m") {

utm_x <- set_units(utm[[3]], "m")

utm_y <- set_units(utm[[4]], "m")


} else if (uniter == "us-ft") {

utm_x_US <- set_units(utm[[3]], "US_survey_foot")

utm_y_US <- set_units(utm[[4]], "US_survey_foot")

utm_x <- utm_x_US

utm_y <- utm_y_US

units(utm_x) <- make_units(m)

units(utm_y) <- make_units(m)


} else if (uniter == "ft") {

utm_x_ft <- set_units(utm[[3]], "ft")

utm_y_ft <- set_units(utm[[4]], "ft")

utm_x <- utm_x_ft

utm_y <- utm_y_ft

units(utm_x) <- make_units(m)

units(utm_y) <- make_units(m)

}

utm_x_cm <- utm_x
utm_x_cm <- set_units(utm_x_cm, cm)
utm_x_cm <- round_r3(drop_units(utm_x_cm), d = 4)

utm_y_cm <- utm_y
utm_y_cm <- set_units(utm_y_cm, cm)
utm_y_cm <- round_r3(drop_units(utm_y_cm), d = 4)

utm_x_ft <- utm_x
utm_x_ft <- set_units(utm_x_ft, "ft")
utm_x_ft <- round_r3(drop_units(utm_x_ft), d = 4)

utm_y_ft <- utm_y
utm_y_ft <- set_units(utm_y_ft, "ft")
utm_y_ft <- round_r3(drop_units(utm_y_ft), d = 4)

utm_x_US <- utm_x
utm_x_US <- set_units(utm_x_US, "US_survey_foot")
utm_x_US <- round_r3(drop_units(utm_x_US), d = 4)

utm_y_US <- utm_y
utm_y_US <- set_units(utm_y_US, "US_survey_foot")
utm_y_US <- round_r3(drop_units(utm_y_US), d = 4)

utm_x <- round_r3(drop_units(utm_x), d = 4)
utm_y <- round_r3(drop_units(utm_y), d = 4)


if(output == "basic") {

return(list(data_projected = data_projected, utm = utm))


} else if (output == "table") {

# table inspired by Earth Point Reference
# table with UTM information
DT_out <- data.table(names = c("Degrees (Latitude, Longitude)", "Degrees Minutes (Latitude, Longitude)", "Degrees Minutes Seconds (Latitude, Longitude)", "State Plane (X = East, Y = North) [meters]", "State Plane (X = East, Y = North) [US survey foot]", "State Plane (X = East, Y = North) [international foot]", "UTM Zone", "UTM (X = East, Y = North) [meters]", "UTM (X = East, Y = North) [centimeters]", "UTM (X = East, Y = North) [US survey foot]", "UTM (X = East, Y = North) [international foot]", "Hemisphere", "Projected CRS + Defined Units"), numbers = c(paste0(dn1, ", ", dw1), paste0(dmn1, ", ", dmw1), paste0(dmsn1, ", ", dmsw1), paste0(z, " ", em,", ", nm), paste0(z, " ", eu,", ", nu), paste0(z, " ", ei,", ", ni), zone, paste0(utm_x, " ", utm_y), paste0(utm_x_cm, " ", utm_y_cm), paste0(utm_x_US, " ", utm_y_US), paste0(utm_x_ft, " ", utm_y_ft), utm[[5]], st_crs(data_projected)$input))

setnames(DT_out, c("Parameters", "Value"))

col.names <- c("Parameters", "Value")
colnames(DT_out) <- col.names

# code block below modified from data.table function
setattr(DT_out, "col.names", setnames(DT_out, col.names))
setattr(DT_out, "class", c("data.table", "data.frame"))
DT_out

}

} else if (utm == 0) {

if(output == "basic") {

return(data_projected)


} else if (output == "table") {

# table inspired by Earth Point Reference
# without the UTM information
DT_out <- data.table(names = c("Degrees (Latitude, Longitude)", "Degrees Minutes (Latitude, Longitude)", "Degrees Minutes Seconds (Latitude, Longitude)", "State Plane (X = East, Y = North) [meters]", "State Plane (X = East, Y = North) [US survey foot]", "State Plane (X = East, Y = North) [international foot]", "Projected CRS + Defined Units"), numbers = c(paste0(dn1, ", ", dw1), paste0(dmn1, ", ", dmw1), paste0(dmsn1, ", ", dmsw1), paste0(z, " ", em,", ", nm), paste0(z, " ", eu,", ", nu), paste0(z, " ", ei,", ", ni), st_crs(data_projected)$input))

setnames(DT_out, c("Parameters", "Value"))

col.names <- c("Parameters", "Value")
colnames(DT_out) <- col.names

# code block below modified from data.table function
setattr(DT_out, "col.names", setnames(DT_out, col.names))
setattr(DT_out, "class", c("data.table", "data.frame"))
DT_out

}
}
}




# degree_coord function is the degree function found in OSMscale with the function name changed. This function is needed within the engr_survey_reverse function. Copyright license is GPL-2 | GPL-3 by Berry Boessenkool.
degree_coord <- function(
lat,
long,
data,
todms=!is.character(lat),
digits=1,
drop=FALSE
)
{
# Input coordinates:
if(!missing(data)) # get lat and long from data.frame
  {
  lat  <- getColumn(substitute(lat) , data)
  long <- getColumn(substitute(long), data)
  }
# decimal to DMS
if(todms)
{
checkLL_coord(lat, long, fun=warning)
dec2deg <- function(dec)
  {
  d <- floor(dec)
  decm <- (dec-d)*60
  m <- floor(decm)
  s <- round((decm-m)*60, digits=digits)
  s <- ifelse(s<10, paste0(0,as.character(s)), s)
  paste0(d,"\U00B0",formatC(m, width=2, flag="0"),"'",s,"\"")
  }
x <- dec2deg(abs(long))
x <- paste0(x,ifelse(long>0, "E", "W"))
y <- dec2deg(abs(lat))
y <- paste0(y,ifelse(lat>0, "N", "S"))
out <- data.frame(lat=y,long=x, stringsAsFactors=FALSE)
if(drop) out <- drop(as.matrix(out))
return(out)
}
else # DMS to decimal
# https://stackoverflow.com/questions/14404596/converting-geo-coordinates-from-degree-to-decimal
{
x <- l2df(strsplit(long, "[\U00B0'\"]+", perl=TRUE))
y <- l2df(strsplit(lat , "[\U00B0'\"]+", perl=TRUE))
x2 <- as.numeric(x[,1]) + as.numeric(x[,2])/60 + as.numeric(x[,3])/3600
y2 <- as.numeric(y[,1]) + as.numeric(y[,2])/60 + as.numeric(y[,3])/3600
x2 <- x2*ifelse(toupper(x[,4])=="W", -1, 1)
y2 <- y2*ifelse(toupper(y[,4])=="S", -1, 1)
checkLL_coord(y2, x2, fun=warning)
if(missing(digits)) digits <- 6
out <- data.frame(lat=round(y2,digits), long=round(x2, digits) )
if(drop) out <- drop(as.matrix(out))
return(out)
}
}



# checkLL_coord function is the checkLL function found in OSMscale with the function name changed. This function is needed within the degree_coord function contained in the engr_survey_reverse function. Copyright license is GPL-2 | GPL-3 by Berry Boessenkool.
checkLL_coord <- function(
lat,
long,
data,
fun=stop,
...
)
{
# Input coordinates:
if(!missing(data)) # get lat and long from data.frame
  {
  lat  <- getColumn(substitute(lat) , data)
  long <- getColumn(substitute(long), data)
  }
if(is.character(fun)) stop("fun must be unquoted. Use fun=", fun, " instead of fun='", fun,"'.")
# tracing the calling function(s):
calltrace <- traceCall()
# check coordinates:
minlat  <- min(lat, na.rm=TRUE)
maxlat  <- max(lat, na.rm=TRUE)
minlong <- min(long,na.rm=TRUE)
maxlong <- max(long,na.rm=TRUE)
error <- c(minlat < -90 , maxlat > 90, minlong < -180, maxlong > 180)
errortext <- paste0(rep(c("lat","long"),each=2), " values must be ",
                   rep(c("larger","lesser"),2), " than ", c(-90,90,-180,180),
                   ". Actual ", rep(c("min","max"),2), " is ",
                   c(minlat, maxlat, minlong, maxlong), ".")

# prepare message:
Text <- paste(errortext[error], collapse="\n")
if(max(abs(c(minlat, maxlat, minlong, maxlong))) < 180)
  Text <- paste(Text, "You may have swapped lat and long somewhere.", sep="\n")
Text <- paste(calltrace, Text)
# return message, if file nonexistent:
if(any(error)) fun(Text, ...)
return(invisible(error))
}
