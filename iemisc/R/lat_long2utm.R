#' Conversion of Latitude/Longitude to UTM Coordinates
#'
#' Takes latitude/longitude coordinates (as character or numeric vectors) and
#' transforms them into their respective UTM Easting and Northing coordinates
#' (with units of US Survey foot, foot, or meters) & UTM Zone.
#'
#'
#'
#'
#'
#' @note
#' Remember: Latitude coordinates signify North (N) or South (S) while
#' longitude coordinates signify East (E) and West (W). It is customary to
#' denote West longitude coordinates and South latitude coordinates as negative
#' (-).
#'
#' Stack Overflow user contributions are "licensed under CC BY-SA 3.0 with
#' attribution required." [Stack Overflow Reference] I have decided to make my
#' adaptions to the Stack Overflow user contributions as CC BY-SA 4.0 thereby
#' enabling me to license my adaptions to the aforementioned code as GPLv3.
#' [Creative Commons References]
#'
#'
#'
#'
#' @param latitude numeric vector (or character vector with numbers only) that
#'    contains the latitude as a decimal degree
#' @param longitude numeric vector (or character vector with numbers only) that
#'    contains the longitude as a decimal degree
#' @param units character vector that contains the system of units (options are
#'   \code{survey_ft} (United States Customary System) [US survey foot],
#'   \code{foot}, or \code{meters} (International System of Units) [meters])
#' @param output character vector that contains basic for the default result
#'     using a \code{\link[base]{list}} or table for the result as a \code{\link[data.table]{data.table}}
#'
#'
#' @return the UTM zone along with the UTM Easting and Northing coordinates (in
#'    the requested unit) as either a list or a data.table
#'
#'
#'
#'
#' @author Irucka Embry, Teodor Ciuraru (Latitude/Longitude to UTM conversion code), and Josh O'Brien (Latitude/Longitude to UTM conversion code)
#'
#'
#'
#'
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
#' @source
#' \enumerate{
#'    \item r - Converting latitude and longitude points to UTM - Stack Overflow answered and edited by Teodor Ciuraru on Feb 17 2018. See \url{https://stakoverflow.com/questions/18639967/converting-latitude-and-longitude-points-to-utm}.
#'    \item r - Converting latitude and longitude points to UTM - Stack Overflow answered by Josh O'Brien on Sep 5 2013 and edited by Josh O'Brien on Feb 21 2014. See \url{https://stakoverflow.com/questions/18639967/converting-latitude-and-longitude-points-to-utm}.
#'    \item r - data.table alternative for dplyr mutate? - Stack Overflow answered by Arun on Aug 30 2015. See \url{https://stackoverflow.com/questions/29583665/data-table-alternative-for-dplyr-mutate}.
#'    \item database design - What is the maximum length of latitude and longitude? - Stack Overflow answered by JasonM1 on May 24 2013 and edited by JasonM1 on Jul 16 2019. See \url{https://stackoverflow.com/questions/15965166/what-is-the-maximum-length-of-latitude-and-longitude}.
#'    \item r - How to not run an example using roxygen2? - Stack Overflow answered and edited by samkart on Jul 9 2017. (Also see the additional comments in response to the answer.) See \url{https://stackoverflow.com/questions/12038160/how-to-not-run-an-example-using-roxygen2}.
#'    \item devtools - Issues in R package after CRAN asked to replace dontrun by donttest - Stack Overflow answered by Hong Ooi on Sep 1 2020. (Also see the additional comments in response to the answer.) See \url{https://stackoverflow.com/questions/63693563/issues-in-r-package-after-cran-asked-to-replace-dontrun-by-donttest}.
#'    \item Latitude Longitude Coordinates to State Code in R - Stack Overflow answered by Josh O'Brien on Jan 6 2012 and edited by Josh O'Brien on Jun 18, 2020. See \url{https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r}.
#' }
#'
#'
#'
#' @references
#' \enumerate{
#'    \item MapTools, 29 May 2016, "More details about UTM grid zones", \url{https://www.maptools.com/tutorials/grid_zone_details}.
#'    \item Wikimedia Foundation, Inc. Wikipedia, 11 August 2019, "Geographic coordinate system", \url{https://en.wikipedia.org/wiki/Geographic_coordinate_system}.
#'    \item PROJ 6.2.0 documentation, 28 Oct 2019, "Cartographic projection", \url{https://proj.org/usage/projections.html}.
#'    \item Wikimedia Foundation, Inc. Wikibooks, 19 August 2018, "PROJ.4", \url{https://en.wikibooks.org/wiki/PROJ.4}.
#'    \item National Geospatial-Intelligence Agency Office of Geomatics, "Military Grid Reference System (MGRS) Grid Zone Designator (GZD's)", \url{https://vdocuments.net/military-grid-reference-system-mgrs-grid-zone-designator-gzds.html}.
#'    \item Tennessee Department of Transportation Design Division, Tennessee Department of Transportation Tennessee Geodetic Reference Network (TGRN) Reference Manual Second Edition Issued, page ix, \url{https://www.tn.gov/content/dam/tn/tdot/documents/TgrnComposite.pdf}.
#'    \item LatLong.net, "Lat Long to UTM Converter", \url{https://www.latlong.net/lat-long-utm.html}.
#'    \item NOAA’s National Geodetic Survey (NGS), "NGS Coordinate Conversion and Transformation Tool (NCAT)", \url{https://www.ngs.noaa.gov/NCAT/}.
#'    \item Creative Commons. Weblog Archives, October 8, 2015, "CC BY-SA 4.0 now one-way compatible with GPLv3: The declaration increases interoperability of the commons for games, hardware designs, and more" Posted by mike, \url{https://creativecommons.org/2015/10/08/cc-by-sa-4-0-now-one-way-compatible-with-gplv3/}.
#'    \item Stack Overflow. Public Network Terms of Service, "6. Content Permissions, Restrictions, and Creative Commons Licensing", \url{https://stackoverflow.com/legal/terms-of-service#licensing}.
#' }
#'
#'
#'
#'
#' @examples
#'
#' # Example 1
#' # Test location from TGRN Reference Manual with NCAT
#' # using the 1983 (1995) DATUM
#' # GPS 1 is the station name with these coordinates
#' # latitude (North) = 36 22 6.43923
#' # longitude (West) = 82 10 46.87679
#'
#' install.load::load_package("iemisc", "sp")
#'
#' lats <- as.numeric(char2dms("36d22'6.43923\"N"))
#' lats
#'
#' longs <- as.numeric(char2dms("82d10'46.87679\"W"))
#' longs
#'
#' latsc <- as.character(lats)
#' latsc
#'
#' longsc <- as.character(longs)
#' longsc
#'
#' lat_long2utm(latsc, longsc, units = "m", output = "basic")
#'
#' lat_long2utm(latsc, longsc, units = "m", output = "table")
#'
#' lat_long2utm(lats, longs, units = "m", output = "basic")
#'
#' lat_long2utm(lats, longs, units = "m", output = "table")
#'
#' # From https://www.ngs.noaa.gov/NCAT/
#' # Latitude: 36.3684553416667
#' # Longitude: -82.1796879972222
#' # UTM Northing (m): 4,025,462.877
#' # UTM Easting (m): 394,172.067
#' # USNG: 17SLA9417225462
#'
#'
#'
#' # Example 2
#' # Test against Grid [Reference: National Geospatial-Intelligence Agency Office of Geomatics]
#'
#' library(iemisc)
#'
#' lat_long2utm("80", "-179", units = "m", output = "basic") # = 1X
#'
#' lat_long2utm("-80", "-179", units = "m", output = "basic") # = 1C
#'
#'
#'
#' # Example 3
#' # Test with world cities
#'
#' \donttest{
#' # See Source 5 and Source 6
#'
#' install.load::load_package("iemisc", "maps", "rando", "utils", "data.table")
#'
#' import::from(sampler, rsamp)
#'
#' data(world.cities) # from maps
#'
#' set_n(200) # makes the example reproducible
#'
#' wc <- rsamp(world.cities, 2, over = 0, rep = FALSE)
#' wc
#'
#' wcutm1 <- lat_long2utm(wc$lat[1], wc$long[1], units = "m", output = "table")
#' wcutm1
#'
#' wcutm2 <- lat_long2utm(wc$lat[2], wc$long[2], units = "m", output = "table")
#' wcutm2
#'
#' l <- list(wcutm1, wcutm2)
#' ll <- rbindlist(l)
#'
#' wc_utm <- setDT(cbind(wc, ll))
#' wc_utm
#' }
#'
#'
#'
#' # Example 4
#' # Test with 2 Web sites
#'
#' library(iemisc)
#'
#' latlong1 <- lat_long2utm(6.32, 7.41, units = "m", output = "table")
#' latlong1
#'
#' latlong2 <- lat_long2utm(44.47, 19.81, units = "m", output = "table")
#' latlong2
#'
#'
#'
#' # Results from https://www.latlong.net/lat-long-utm.html
#' # Latitude: 6.32
#' # Longitude: 7.41
#' # UTM Easting: 324118.76
#' # UTM Northing: 698846.97
#' # UTM Zone: 32N
#'
#' # Latitude: 44.47
#' # Longitude: 19.81
#' # UTM Easting: 405349.04
#' # UTM Northing: 4924765.48
#' # UTM Zone: 34T
#'
#'
#' # Results from https://www.ngs.noaa.gov/NCAT/
#' # Latitude: 6.32
#' # Longitude: 7.41
#' # UTM Northing (m): 698,846.969
#' # UTM Easting (m): 324,118.758
#' # USNG: 32NLM2411898846
#'
#' # Latitude: 44.47
#' # Longitude: 19.81
#' # UTM Northing (m): 4,924,765.484
#' # UTM Easting (m): 405,349.043
#' # USNG: 34TDQ0534924765
#'
#'
#'
#'
#'
#'
#'
#'
#' @importFrom data.table data.table setnames setattr copy setcolorder :=
#' @importFrom sf st_as_sf st_transform st_coordinates
#' @importFrom checkmate qtest
#' @importFrom assertthat assert_that
#'
#' @export
lat_long2utm <- function (latitude, longitude, units = c("us-ft", "ft", "m"), output = c("basic", "table")) {

. <- NULL
# due to NSE notes in R CMD check

units <- units

output <- output


# convert latitude from character to numeric, if needed
if (is.character(latitude)) {

latitude <- as.numeric(latitude)

} else {

latitude }


# convert longitude from character to numeric, if needed
if (is.character(longitude)) {

longitude <- as.numeric(longitude)

} else {

longitude}



# Check for latitude, longitude, units, and location
assert_that(!any(qtest(latitude, "N==1[-90,90]") == FALSE), msg = "latitude is NA, NaN, Inf, -Inf, a string, or empty. Or latitude is not within the range of -90 to 90 decimal degrees. Please try again.")
# only process with finite values and provide a stop warning if not
# Source 4

assert_that(!any(qtest(longitude, "N==1[-180,180]") == FALSE), msg = "longitude is NA, NaN, Inf, -Inf, a string, or empty. Or longitude is not within the range of -180 to 180 decimal degrees. Please try again.")
# only process with finite values and provide a stop warning if not
# Source 4

assert_that(qtest(output, "S==1"), msg = "output should only be a single character vector. Please try again.")
# only process with a single string value and provide a stop warning if not

assert_that(qtest(units, "S==1"), msg = "units should only be a single character vector. Please try again.")
# only process with a single string value and provide a stop warning if not

assert_that(isTRUE(any(c("basic", "table") %in% output)), msg = "Incorrect output selection. The only possible outputs are 'basic' and 'table'. Please try again.")
# only process with a specified output and provide a stop warning if not


assert_that(isTRUE(any(c("us-ft", "ft", "m") %in% units)), msg = "Incorrect units selection. The only possible units are 'us-ft', 'ft', and 'm'. Please try again.")
# only process with a specified units and provide a stop warning if not
# PROJ 6.2.0 Reference


zone_number <- (floor((longitude + 180) / 6) %% 60) + 1


# Source 1 begin
# Special zones for Norway
cond_32 <- latitude >= 56.0 & latitude < 64.0 & longitude >= 3.0 & longitude < 12.0
zone_number[cond_32] <- 32

# Special zones for Svalbard
cond_lat <- latitude >= 72.0 & latitude < 84.0

cond_31 <- cond_lat & longitude >= 0.0 & longitude <  9.0
zone_number[cond_31] <- 31

cond_33 <- cond_lat & longitude >= 9.0 & longitude < 21.0
zone_number[cond_33] <- 33

cond_35 <- cond_lat & longitude >= 21.0 & longitude < 33.0
zone_number[cond_35] <- 35

cond_37 <- cond_lat & longitude >= 33.0 & longitude < 42.0
zone_number[cond_37] <- 37
# Source 1 end


# MapTools Reference
# the UTM zone letters begin at C and end at X while skipping I (similar to 1) and O (similar to 0)
beginlet <- which(LETTERS == "C")

skiplet1 <- which(LETTERS == "I")

skiplet2 <- which(LETTERS == "O")

endlet <- which(LETTERS == "X")

skiplet <- LETTERS[c(skiplet1, skiplet2)]

uselet1 <- LETTERS[beginlet:endlet]

utm_let <- uselet1[!uselet1 %in% skiplet]

UTM_zones <- data.table(Zone_Letter = utm_let, Latitude = seq(-80, 74, by = 8))

zonelet_use <- UTM_zones[.(floor(latitude)), on =.(Latitude), roll = Inf][[1]]

zone <- paste0(zone_number, zonelet_use, sep = "")



# Source 1 begin
hemisphere <- ifelse(latitude > 0, "North", "South")


CRSstring <- paste0("+proj=utm +zone=", zone_number, " +ellps=WGS84", " +", hemisphere, " +units=", units)
# Source 1 end

assert_that(qtest(CRSstring, "S==1"), msg = "Multiple zones and/or hemispheres detected.")
# only process with a single string value and provide a stop warning if not



dt <- data.table(id = seq_along(longitude), x = longitude, y = latitude)
# Source 2


# Source 7 begins
# using sf instead of sp now
utm_sf <- st_as_sf(dt, coords = 2:3, crs = "+proj=longlat +init=epsg:4326 +datum=WGS84")
# where x and y stand for Longitude / Latitude columns
# Source 7 ends


utm_finals <- st_transform(utm_sf, CRSstring)
# transform the coordinates

utm_finalss <- as.data.table(st_coordinates(utm_finals))
# retrieve the coordinates as a matrix and then convert to a data.table

utm_final <- utm_finalss[, c("id") := utm_finals$id]

setcolorder(utm_final, c("id", "X", "Y"))



if(output == "basic") {

return(list(zone = zone, utm_final = utm_finals))


} else {

utm_finaler <- utm_final[, c("a", "b") := list(zone, hemisphere)]
# Source 1 and Source 3

DT_out <- copy(utm_finaler)

setcolorder(DT_out, c(1, 4, 2, 3, 5))


if(units == "m") {

setnames(DT_out, 2:5, c("UTM Zone", "UTM X = East [meters]", "UTM Y = North [meters]", "Hemisphere"))

# table inspired by Earth Point Reference
col.names <- c("id", "UTM Zone", "UTM X = East [meters]", "UTM Y = North [meters]", "Hemisphere")

# code block below modified from data.table function
setattr(DT_out, "col.names", setnames(DT_out, col.names))
setattr(DT_out, "class", c("data.table", "data.frame"))
DT_out


} else if(units == "us-ft") {

setnames(DT_out, 2:5, c("UTM Zone", "UTM X = East [US survey foot]", "UTM Y = North [US survey foot]", "Hemisphere"))

# table inspired by Earth Point Reference
col.names <- c("id", "UTM Zone", "UTM X = East [US survey foot]", "UTM Y = North [US survey foot]", "Hemisphere")

# code block below modified from data.table function
setattr(DT_out, "col.names", setnames(DT_out, col.names))
setattr(DT_out, "class", c("data.table", "data.frame"))
DT_out


} else if(units == "ft") {

setnames(DT_out, 2:5, c("UTM Zone", "UTM X = East [international foot]", "UTM Y = North [international foot]", "Hemisphere"))

# table inspired by Earth Point Reference
col.names <- c("id", "UTM Zone", "UTM X = East [international foot]", "UTM Y = North [international foot]", "Hemisphere")

# code block below modified from data.table function
setattr(DT_out, "col.names", setnames(DT_out, col.names))
setattr(DT_out, "class", c("data.table", "data.frame"))
DT_out


}
}
}
