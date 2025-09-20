#' Generate a data frame with points above the treeline
#'
#' @description A data frame is generated containing only points that are at or above the treeline.
#' The calculation of the treeline (when using default thresholds) is based on Paulsen and Körner,
#' Alp. Bot. 124: 1-12 (2014).
#' @usage generate_point_df(gstRaster, gslRaster, stepSize = 0.0416666,
#'                         gstTreshold = 6.4, gslTreshold = 94)
#' @param gstRaster Climatic raster that contains the growing season temperature. Data type "SpatRaster".
#' @param gslRaster Climatic raster that contains the growing season length. Data type "SpatRaster".
#' @param stepSize Step size for the sampling (in degree). This defines how fare the coordinates are apart. One value, data type "numeric" and finite.
#' @param gstTreshold Growing season temperature threshold for tree growth (in degree Celsius). One value, data type "numeric" and finite.
#' @param gslTreshold Growing season length threshold for tree growth (days). One value, data type "integer" and finite.
#' @return Data frame that contains coordinates of points above the treeline.
#' @author Livio Bätscher, Jurriaan M. de Vos
#' @examples
#' #Get raster layer from CHELSA
#' gstURL <- paste0("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/",
#'                  "climatologies/1981-2010/bio/CHELSA_gst_1981-2010_V.2.1.tif")
#' gslURL <- paste0("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/",
#'                  "climatologies/1981-2010/bio/CHELSA_gsl_1981-2010_V.2.1.tif")
#' \donttest{
#' gst <- terra::rast(gstURL, vsi = TRUE)
#' gsl <- terra::rast(gslURL, vsi = TRUE)
#'
#' #Now generate a example data frame
#' temp <- generate_point_df(gstRaster = gst, gslRaster = gsl, stepSize = 10,
#'                          gstTreshold = 6.4, gslTreshold = 94)
#' }
#' @export

generate_point_df <- function(gstRaster, gslRaster, stepSize = 0.0416666, gstTreshold = 6.4, gslTreshold = 94) {
  #Error handling
  if (class(gstRaster)[1] != "SpatRaster") {stop("gstRaster must be from class SpatRaster")}
  if (class(gslRaster)[1] != "SpatRaster") {stop("gslRaster must be from class SpatRaster")}
  if (length(stepSize) != 1) {stop("stepSize must be of length 1")} else if (!is.finite(stepSize)) {stop("stepSize must be numeric and finite")}
  if (length(gstTreshold) != 1) {stop("gstTreshold must be of length 1")} else if (!is.finite(gstTreshold)) {stop("gstTreshold must be numeric and finite")}
  if (length(gslTreshold) != 1) {stop("gslTreshold must be of length 1")} else if (!is.finite(gslTreshold) || gslTreshold %% 1 != 0) {stop("gslTreshold must be a integer and finite")}

  #Create a range of x and y coordinates; representing the extremes and the resolution of the raster
  lon_seq <- (seq(-180.0208, 179.9786, stepSize))
  lat_seq <- (seq(-59.47894, 83.64583, stepSize))

  lon <- rep(lon_seq, each = length(lat_seq))
  lat <- rep(lat_seq, times = length(lon_seq))

  #Create a point matrix containing longitude, latitude
  point <- as.data.frame(cbind("longitude" = lon, "latitude" = lat))

  #Now get the raster value that matches the coordinate from the df point
  gstValues <- terra::extract(gstRaster, point)[,2]
  gslValues <- terra::extract(gslRaster, point)[,2]
  point <- cbind(point, "gst" = gstValues, "gsl"= gslValues)

  #Create vectors containing "NA" for the maximum number of points
  longitude <- rep(NA, nrow(point))
  latitude <- rep(NA, nrow(point))

  #Loop through the data frame
  for (i in 1:nrow(point)) {
    #Filter for points that are above the treeline
    if ((!is.na(point$gst[i]) && !is.na(point$gsl[i])) &&
        (point$gst[i] < gstTreshold || point$gsl[i] < gslTreshold)) {
    #Add the elements to the filter because this is much faster
    longitude[i] <- point$longitude[i]
    latitude[i] <- point$latitude[i]
    }
  }

  #Remove the remaining NAs
  longitude <- longitude[!is.na(longitude)]
  latitude <- latitude[!is.na(latitude)]

  #Combine vectors to a data frame
  pointsAboveTreeline <- data.frame(cbind(longitude, latitude))

  #Return the object
  return(pointsAboveTreeline)
}
