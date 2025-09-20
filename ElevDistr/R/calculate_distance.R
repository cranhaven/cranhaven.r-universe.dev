#' Sample and calculate the distance to the local treeline
#'
#' @description Points are uniformly drawn along polygons that specify the treeline. The elevation of each point is then extracted
#' and the median elevation of all points is calculated. Finally the median treeline elevation is subtracted from the
#' \code{pointElevation} to get its distance to the local treeline.
#' @usage calculate_distance(treeline, elevationRaster, pointElevation, treelineSampling = 10,
#'                          plot = FALSE)
#' @param treeline A data frame containing line-shaped polygons. Each row containing: a identifier, a start latitude and
#' longitude, an end latitude and longitude. All longitude and latitude (WGS 84) parameters must be of the data type "numeric" and finite.
#' @param elevationRaster Raster that contains a digital elevation model. Data type "SpatRaster".
#' @param pointElevation Elevation of the point of interest (in meters above the sea level). One value, data type "numeric" and finite.
#' @param treelineSampling A constant number of samples taken from one "treeline piece". One value, data type "integer", finite and not zero.
#' @param plot Boolean that defines if a histogram of the sampled elevation is plotted.
#' The plot will only be shown if the value is \code{TRUE}.
#' @return Returns a numeric representing the vertical distance to the local treeline in meters.
#' @author Livio Bätscher, Jurriaan M. de Vos
#' @examples \dontrun{
#' calculate_distance(treeline = dfTreeline, elevationRaster = GTOPO30, pointElevation = 512,
#'                    treelineSampling = 10, plot = FALSE)
#' }
#' @export

calculate_distance <- function(treeline, elevationRaster, pointElevation, treelineSampling = 10, plot = FALSE) {
  #Error handling
  if (!is.data.frame(treeline)) {stop("treeline must be a data frame")} else if (ncol(treeline) != 5) {stop("treeline needs to have five columns")} else if (sum(is.na(unlist(treeline))) != 0) {stop("treeline can not contain NA")} else if (sum(!is.finite(unlist(treeline[,2:5]))) != 0) {stop("treeline[,2:5] must contain only numeric and finite elements")}
  if (class(elevationRaster)[1] != "SpatRaster") {stop("elevationRaster must be from class SpatRaster")}
  if (length(pointElevation) != 1) {stop("pointElevation must be of length 1")} else if (!is.finite(pointElevation)) {stop("pointElevation must be numeric and finite")}
  if (length(treelineSampling) != 1) {stop("treelineSampling must be of length 1")} else if (!is.finite(treelineSampling) || treelineSampling %% 1 != 0) {stop("treelineSampling must be a integer and finite")} else if (treelineSampling == 0) {stop("treelineSampling can not be zero")}
  if (!is.logical(plot)) {stop("plot must be a boolean")}

  #Change names to be sure that I can use them
  names(treeline) <- c("id", "lat1", "lon1", "lat2", "lon2")

  #Crate a empty variable
  point <- data.frame()

  #Loop through all the lines in treeline
  for (i in 1:length(treeline$id)) {
    #Check if the line is horizontal
    if (treeline$lat1[i] == treeline$lat2[i]) {
      #Sample x coordinate
      lonSeq <- seq(from = treeline$lon1[i], to = treeline$lon2[i], by = (abs(treeline$lon2[i] - treeline$lon1[i])) / treelineSampling)
      lonSeq <- lonSeq[-(treelineSampling+1)] #Remove the last value to avoid double plotting

      lat <- treeline$lat1[i] #Load y coordinate
      latSeq <- rep(lat, treelineSampling) #Repeat the y coordinate
      #Check if else the line is vertical
    } else if (treeline$lon1[i] == treeline$lon2[i]) {
      #Sample y coordinate
      latSeq <- seq(from = treeline$lat1[i], to = treeline$lat2[i], by = (abs(treeline$lat2[i] - treeline$lat1[i])) / treelineSampling)
      latSeq <- latSeq[-(treelineSampling+1)]

      lon <- treeline$lon1[i] #Load the x coordinate
      lonSeq <- rep(lon, treelineSampling) #Remove the last value to avoid double plotting
    }

    #Now make a data frame with the two sequences
    pointTemp <- data.frame(lonSeq, latSeq)

    #Add the point to the existing data frame
    point <- rbind(point, pointTemp)

  }

  #Extract the elevation
  sampledTreelineHeight <- terra::extract(elevationRaster, point)[,2]


  #Calculate the mean elevation
  estimatetElevation <- stats::median(sampledTreelineHeight, na.rm = TRUE)

  #Evaluate the plot bolean
  if (plot == TRUE) {
    #Plot the distribution
    graphics::hist(sampledTreelineHeight, breaks = 50, main = "Histogram of the estimated treeline")
    graphics::abline(v = estimatetElevation, col = "red", lwd = 3)
  }

  #Estimated distance to the hypothetical treeline
  distanceToTreeline <- pointElevation - estimatetElevation

  return(distanceToTreeline)
}
