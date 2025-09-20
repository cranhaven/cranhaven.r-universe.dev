#' Search the nearest point in a data frame
#'
#' @description Search for the nearest point in a data frame using a k-dimensional tree and a nearest neighbor search.
#' The aim of this function is to get the nearest point above the treeline given the chosen \code{lon} and \code{lat}.
#' @usage get_nearest_point(lon, lat, pointDf)
#' @param lon Longitude of a point (in degrees; WGS 84). One value, data type "numeric" from -180 until 180 and finite.
#' @param lat Latitude of a point (in degrees; WGS 84). One value, data type "numeric" from -90 until 90 and finite.
#' @param pointDf Data frame that contains coordinates (WGS 84) of points above the treeline.
#' The first column must contain the longitude, the second the latitude.
#' The values must be of the data type "numeric" and finite.
#' @return A list containing the longitude and the latitude of the nearest point.
#' @author Livio Bätscher, Jurriaan M. de Vos
#' @examples
#' #Create a dummy data frame.
#' longitude <- seq(0, 10)
#' latitude <- seq(40, 50)
#' temp <- data.frame(longitude, latitude)
#' get_nearest_point(lon = 8.65, lat = 46.87, pointDf = temp)
#'
#' #Use the data that is included in the package.
#' get_nearest_point(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline)
#' @export

get_nearest_point <- function(lon, lat, pointDf) {
  #Error handling
  if (length(lon) != 1) {stop("lon must be of length 1")} else if (!is.finite(lon)) {stop("lon must be numeric and finite")} else if (lon > 180  || lon < -180) {stop("If we have to use coordinates,let's use proper coordinates... lon must be from -180 to 180")}
  if (length(lat) != 1) {stop("lat must be of length 1")} else if (!is.finite(lat)) {stop("lat must be numeric and finite")} else if (lat > 90 || lat < -90) {stop("If we have to use coordinates, let's use proper coordinates... lat must be from -90 to 90")}
  if (!is.data.frame(pointDf)) {stop("pointDf must be a data frame")} else if (ncol(pointDf) < 2) {stop("pointDf needs to have at least two columns")} else if (sum(!is.finite(as.matrix(pointDf))) != 0) {stop("pointDf must be numeric and finite")}

  #Get the nearest neighbor corner
  nn <- RANN::nn2(pointDf, query = data.frame(lon, lat), k = min(1, nrow(utils::data)))

  #Extract the coordinate from the nn-search
  lonCenter <- as.numeric(pointDf[nn$nn.idx,][1]) #Latitude center of the calculation (in degree)
  latCenter <- as.numeric(pointDf[nn$nn.idx,][2]) #Longitude center of the calculation (in degree)

  #Generate a output for the return variable
  output <- list(lon = lonCenter, lat = latCenter)

  return(output) #Return the longitude and latitude
}
