#' United States of America (USA) State Identification Using Latitude/Longitude Coordinates
#'
#' Using the provided latitude/longitude coordinates (as character or numeric
#' vectors), this function determines whether the location is within an United
#' States of America (USA) state/commonwealth, Puerto Rico, or the U.S. Virgin
#' Islands
#' 
#'
#'
#' @param latitude numeric vector (or character vector with numbers only) that
#'    contains the latitude as a decimal degree
#' @param longitude numeric vector (or character vector with numbers only) that
#'    contains the longitude as a decimal degree
#'
#'
#'
#' @return the location name as a character vector (United States of America
#'    (USA) state/commonwealth, Puerto Rico, or the U.S. Virgin Islands
#'
#' @source
#' Latitude Longitude Coordinates to State Code in R - Stack Overflow answered by Josh O'Brien on Jan 6 2012 and edited by Josh O'Brien on Jun 18, 2020. See \url{https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r}.
#'
#'
#'
#'
#'
#' @author Irucka Embry, Josh O'Brien (Stack Overflow R code)
#'
#' @encoding UTF-8
#'
#'
#' @examples
#' 
#' # Example 1
#'
#' library(iemisc)
#'
#' lat_long2state(latitude = c(36.3684553, 40), longitude = c(-82.1796880, -89))
#' lat_long2state(latitude = "36.3684553", longitude = "-82.1796880")
#'
#'
#'
#' # Example 2
#'
#' # Test the function using points in Wisconsin and Oregon (From Source 1)
#'
#' library(iemisc)
#'
#' x = c(-90, -120); y = c(44, 44)
#' lat_long2state(latitude = y, longitude = x)
#'
#'
#'
#'
#' @importFrom data.table data.table
#' @importFrom sf st_as_sf st_transform st_intersects
#' @importFrom checkmate qtest
#' @importFrom assertthat assert_that
#' @import USA.state.boundaries
#'
#' @export
lat_long2state <- function (latitude, longitude) {


# copy the dataset as states
states <- USA.state.boundaries::state_boundaries_wgs84


# convert latitude from character to numeric, if needed
if (is.character(latitude)) {

latitude <- as.numeric(latitude)

} else {

latitude }


# convert longitude from character to numeric, if needed
if (is.character(longitude)) {

longitude <- as.numeric(longitude)

} else {

longitude }


# Check for latitude, longitude, units, and location
assert_that(!any(qtest(latitude, "N+[0,90]") == FALSE), msg = "latitude is NA, NaN, Inf, -Inf, a string, or empty. Or latitude is not within the range of 0 to 90 decimal degrees. Please try again.")
# only process with finite values and provide a stop warning if not

assert_that(!any(qtest(longitude, "N+[-180,0]") == FALSE), msg = "longitude is NA, NaN, Inf, -Inf, a string, or empty. Or longitude is not within the range of -180 to 0 decimal degrees. Please try again.")
# only process with finite values and provide a stop warning if not


# Source 1 begins
# create pointsDT data.table
pointsDT <- data.table(x = longitude, y = latitude)

# Convert pointsDT to a sf points object
pointsSP <- st_as_sf(pointsDT, coords = 1:2, crs = "+proj=longlat +datum=WGS84")




# transform both pointsSP and states to a planar coordinate system (e.g. Web Mercator) as required for geometric operations
states2 <- st_transform(states, crs = 3857)
points <- st_transform(pointsSP, crs = 3857)


# find the state names that are intersected by each point
state_names <- states2$NAME
index <- as.integer(st_intersects(points, states2))

# Return the state names of the Polygons object containing each point
stateNames <- state_names[index]
# Source 1 ends

stateNames

}
