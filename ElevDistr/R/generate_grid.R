#' Generate a grid around the input point
#'
#' @description Generate a grid around a longitude and latitude, with a defined square size and step size.
#' @usage generate_grid(lon, lat, squareSize = 10, stepSize = 0.0025)
#' @param lon Longitude of the grid center (in degrees; WGS 84). One value, data type "numeric" and finite.
#' @param lat Latitude of the grid center (in degrees; WGS 84). One value, data type "numeric" and finite.
#' @param squareSize Square size (in km). One value, data type "numeric" and finite.
#' @param stepSize Step size for the square sampling (in degree). One value, data type "numeric" and finite.
#' @return A list containing a data frame with longitude and latitude of the grid and a vector containing the length of the longitudinal and latitudinal sequence.
#' @author Livio Bätscher, Jurriaan M. de Vos
#' @examples
#' #Generate a 10x10 km grid with a step size of 0.0025 degrees
#' temp <- generate_grid(lon = 8.728898, lat = 46.93756, squareSize = 10, stepSize = 0.0025)
#'
#' #Part of the generated coordinates
#' temp$df[105:115,]
#' @export

generate_grid <- function(lon, lat, squareSize = 10, stepSize = 0.0025) {
  #Error handling
  if (length(lon) != 1) {stop("lon must be of length 1")} else if (!is.finite(lon)) {stop("lon must be numeric and finite")}
  if (length(lat) != 1) {stop("lat must be of length 1")} else if (!is.finite(lat)) {stop("lat must be numeric and finite")}
  if (length(squareSize) != 1) {stop("squareSize must be of length 1")} else if (!is.finite(squareSize)) {stop("squareSize must be numeric and finite")}
  if (length(stepSize) != 1) {stop("stepSize must be of length 1")} else if (!is.finite(stepSize)) {stop("stepSize must be numeric and finite")}

  #Calculate the degrees
  lonRange <- round(squareSize / (cos(lat * pi / 180) * 111.699), 2) #Longitude length of the square (in degree)
  latRange <- squareSize / 111 #Latitude length of the square (in degree)

  #Create a range of x and y coordinates
  lonSeq <- seq(from = lon-(lonRange/2), to = lon+(lonRange/2), by = stepSize)
  latSeq <- seq(from = lat-(latRange/2), to = lat+(latRange/2), by = stepSize)

  #Get the length of both sequences
  lonSeqLength <- length(lonSeq)
  latSeqLength <- length(latSeq)

  lon <- rep(lonSeq, each = latSeqLength) #Repeat every number
  lat <- rep(latSeq, times = lonSeqLength) #Repeat the series

  #Create a point matrix containing longitude, latitude
  coords <- as.data.frame(cbind("longitude" = lon, "latitude" = lat))

  #Generate a output for the return variable
  output <- list(df = coords, lonLength = lonSeqLength, latLength = latSeqLength)

  return(output) #Return the coordinate data frame
}
