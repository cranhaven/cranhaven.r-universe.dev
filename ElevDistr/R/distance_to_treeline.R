#' Wrapper that calculates the distance relative to the nearest local treeline
#'
#' @description Calculate the distance to the treeline in meters. Positive values indicate that the sample is above the treeline.
#' Negative values for samples below the treeline.
#' @usage distance_to_treeline(lon, lat, gstRaster, gslRaster, elevationRaster, elevation,
#'                             pointDf , gridSize = 10,  gridStepSize = 0.0025, plot = FALSE,
#'                             plotZoom = NULL, treelineSamplingSize = 10, plotHist = FALSE,
#'                             gstMin = 6.4, gslMin = 94)
#' @param lon Longitude of a point (in degrees; WGS 84). One value or a vector, data type "numeric" and finite.
#' @param lat Latitude of a point (in degrees; WGS 84). One value or a vector, data type "numeric" and finite.
#' @param gstRaster Climatic raster that contains the growing season temperature. Data type "SpatRaster".
#' @param gslRaster Climatic raster that contains the growing season length. Data type "SpatRaster".
#' @param elevationRaster Raster that contains a digital elevation model. Data type "SpatRaster".
#' @param elevation Elevation of the point of interest (in meters above the sea level). One value or a vector, data type "numeric" and finite.
#' @param pointDf Data frame that contains coordinates (WGS 84) of points above the treeline.
#' The first column must contain the longitude, the second the latitude.
#' The values must be of the data type "numeric" and finite.
#' @param gridSize Square size (in km) of the grid. One value, data type "numeric" and finite.
#' @param gridStepSize Step size for the square sampling (in degree) of the grid. One value, data type "numeric" and finite.
#' @param plot Boolean that defines if a map of the sampled area is plotted. The plot will only be shown if the value is \code{TRUE}.
#' @param plotZoom Map zoom, for the "get_map" function of the "ggmap" library. One value, data type "integer", from 3 to 21 and finite.
#' @param treelineSamplingSize A constant number of samples taken from one "treeline piece". One value, data type "integer", not zero and finite.
#' @param plotHist Boolean that defines if a histogram of the sampled elevation is plotted. The plot will only be shown if the value is \code{TRUE}.
#' @param gstMin Growing season temperature threshold for tree growth (in degree Celsius). One value, data type "numeric" and finite.
#' @param gslMin Growing season length threshold for tree growth (days). One value, data type "numeric" and finite.
#' @details This is the main function, which calls the other relevant functions. Specifically, in turn, it calls \code{get_nearest_point} to
#' identify where the nearest local treeline is, \code{generate_grid}, \code{classify_above_treeline}, and \code{sample_treeline} to locally
#' investigate at what elevation the treeline is, and finally \code{calculate_distance} to determine the elevation of the point relative to
#' the local treeline. It is recommended to use this wrapper rather than the individual functions, unless you have a very specific reason not to.
#' The position of a point relative to the treeline depends on a treeline definition.  Here we follow the definition of Paulsen & Körner,
#' Alp. Bot. 124: 1-12 (2014), which is based on specific thresholds of growing season length and growing season temperature (94 days and 9.4°C,
#' respectively). It is possible to adjust these thresholds manually, e.g. to achieve a elevation above or below another climatic line.
#' Note that this requires to first calculate \code{pointDf} for the boundary of interest using the functions \code{generate_point_df}.
#' Because the implemented treeline definition depends not only on temperature but also on growing season length, it can be affected by drought.
#' Therefore, the user must take care in interpreting treeline information in desert mountain systems. Here, we recommend to frequently use
#' the option \code{plot} and \code{plotHist} to gain a thorough understanding of the local situation.
#' @return Returns the distance to the local treeline in meters as one value or as vector.
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
#' gmted2010URL <- paste0("https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/topo/downloads/GMTED/",
#'                       "Global_tiles_GMTED/300darcsec/med/E000/30N000E_20101117_gmted_med300.tif")
#' gmted2010Part <- terra::rast(gmted2010URL, vsi = TRUE)
#'
#' #Check one point
#' distance_to_treeline(lon = 8.65, lat = 46.87, gstRaster = gst, gslRaster = gsl,
#'                      elevationRaster = gmted2010Part, elevation = 504,
#'                      pointDf = pointsAboveTreeline, plot = FALSE,
#'                      plotHist = FALSE, gstMin = 6.4, gslMin = 94)
#' distance_to_treeline(lon = 4.47, lat = 51.92, gstRaster = gst, gslRaster = gsl,
#'                      elevationRaster = gmted2010Part, elevation = 504,
#'                      pointDf = pointsAboveTreeline, plot = FALSE,
#'                      plotHist = FALSE, gstMin = 6.4, gslMin = 94)
#' distance_to_treeline(lon = -156.71, lat = 69.74,gstRaster = gst, gslRaster = gsl,
#'                      elevationRaster = gmted2010Part, elevation = 504,
#'                      pointDf = pointsAboveTreeline, plot = FALSE,
#'                      plotHist = FALSE, gstMin = 6.4, gslMin = 94)
#' }
#' @export

distance_to_treeline <- function(lon, lat, gstRaster, gslRaster, elevationRaster, elevation, pointDf, gridSize = 10, gridStepSize = 0.0025,
                                          plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10, plotHist = FALSE, gstMin = 6.4, gslMin = 94) {
  #Threw a warning if the "plotZoom" variable is changed, but the "plot" variable is still FALSE
  if(plot == FALSE && !is.null(plotZoom)) warning("The changed plotZoom is ignored, because the ploting is dissabled")

  if(is.null(plotZoom)){plotZoom = 12} #Set a default value for "gridSize"

  #Error handling
  if (sum(!is.finite(lon)) != 0)  {stop("lon must be numeric and finite")}
  if (sum(!is.finite(lat)) != 0) {stop("lat must be numeric and finite")}
  if (!is.data.frame(pointDf)) {stop("pointDf must be a data frame")} else if (ncol(pointDf) < 2) {stop("pointDf needs to have at least two columns")} else if (sum(!is.finite(as.matrix(pointDf)))) {stop("pointDf must contain only numeric and finite elements")}
  if (class(gstRaster)[1] != "SpatRaster") {stop("gstRaster must be from class SpatRaster")}
  if (class(gslRaster)[1] != "SpatRaster") {stop("gslRaster must be from class SpatRaster")}
  if (class(elevationRaster)[1] != "SpatRaster") {stop("elevationRaster must be from class SpatRaster")}
  if (sum(!is.finite(elevation)) != 0) {stop("elevation must be numeric and finite")}
  if (length(gridSize) != 1) {stop("gridSize must be of length 1")} else if (!is.finite(gridSize)) {stop("gridSize must be numeric and finite")}
  if (length(gridStepSize) != 1) {stop("gridStepSize must be of length 1")} else if (!is.finite(gridStepSize)) {stop("gridStepSize must be numeric and finite")}
  if (!is.logical(plot)) {stop("plot must be a boolean")}
  if (length(plotZoom) != 1) {stop("plotZoom must be of length 1")} else if (!is.finite(plotZoom) || plotZoom %% 1 != 0) {stop("plotZoom must be a integer and finite")} else if (plotZoom > 21 || plotZoom < 3) {stop("plotZoom must be from 3 to 21")}
  if (length(treelineSamplingSize) != 1) {stop("treelineSamplingSize must be of length 1")} else if (!is.finite(treelineSamplingSize) || treelineSamplingSize %% 1 != 0) {stop("treelineSamplingSize must be a integer and finite")} else if (treelineSamplingSize == 0) {stop("treelineSamplingSize can not be zero")}
  if (!is.logical(plotHist)) {stop("plotHist must be a boolean")}
  if (length(gstMin) != 1) {stop("gstMin must be of length 1")} else if (!is.finite(gstMin)) {stop("gstMin must be numeric and finite")}
  if (length(gslMin) != 1) {stop("gslMin must be of length 1")} else if (!is.finite(gslMin) || gslMin %% 1 != 0) {stop("gslMin must be a integer and finite")}

  #In case vectors are used: generate new vector
  elevDistanceVec <- rep(NA, length(lon))
  startGridSize <- gridSize #Store the initial grid size

  #Create the progress bar
  pb = utils::txtProgressBar(min = 0, max = length(lon), initial = 0, style = 3, width = 60)
  #init <- numeric(length(lon))
  #end <- numeric(length(lon))

  #Loop through the vectors
  for (i in 1:length(lon)){
    #init[i] <- Sys.time() #Get local time

    #Calculate the nearest point above the treeline
    pointAbove <- get_nearest_point(lon[i], lat[i], pointDf)

    treelineDf <- data.frame(NULL) #Create a empty data frame for the treeline polygons
    counter <- 0 #Set up a variable that counts how many times we went through the while loop

    #Create a while loop that increases the "gridsize" if there are not at least 10 lines in "treelineDf"
    while(nrow(treelineDf) < 10) {
      #Generate around the nearest point (above the treeline) a grid
      grid <- generate_grid(pointAbove$lon, pointAbove$lat, gridSize, gridStepSize)

      #Calculate if the grid point is above the treeline or not
      grid$df <- classify_above_treeline(grid$df, gstRaster, gslRaster, gstMin, gslMin)

      #Draw a line between differently classified grid-points
      treelineDf <- sample_treeline(grid$df, grid$lonLength, grid$latLength, gridStepSize)

      counter <- counter + 1 #Increase the counter by one
      gridSize <- gridSize + 5 #Increase the grid size by 5km

      #Safety loop if we increased the step size 5 times
      if(counter > 6) {
        warning(paste("The \"gridSize\" was increased 5 times, to avoid an endlose loop we stop here. Consider to exclude this point it might not be representative."))
        break
      }
    }

    #Reset the grid size
    gridSize <- startGridSize

    #Throw warning if the grid size was increased
    if(counter > 1) warning(paste("The \"gridSize\" parameter was to small. The parameter was automatically increased by", (counter-1)*5, "km."))

    #Plot a map for a visual control (if wished)
    if (plot == TRUE) {plot_distr(pointAbove, grid$df, treelineDf, plotZoom)}


    #Handle the case, when no treeline was detected within the area
    if (nrow(treelineDf) == 0) {
      #In this case the distance to the treeline could not be computed
      elevDistanceVec[i] <- NA
      warning("No treeline was found within the defined gridd. Therfore, the result is \"NA\". Consider retying with a larger gird size.")

    #In case everything went well
    } else {
      #Take discrete sub samples of the line and calculate the median elevation and the distance to the initial elevation
      elevDistance <- calculate_distance(treelineDf, elevationRaster, elevation[i], treelineSamplingSize, plotHist)

      #Add current elevation relative to the treeline to the vector
      elevDistanceVec[i] <- elevDistance
    }

    #Update the progressbar with every iteration
    utils::setTxtProgressBar(pb,i)

    #end[i] <- Sys.time() #Get the end time
    #time <- round(sum(end - init), 0)

    #Estimated remaining time based on the mean time that took to run the previous iterations
    #est <- length(lon) * (mean(end[end != 0] - init[init != 0])) - time
    #remainining <- round(est, 0)

    #cat(paste(" // Execution time (s):", time,
    #          " // Estimated time remaining (s):", remainining), "") #Plot the result
  }
  close(pb) #Close the progress bar

  #Return the distance
  return(elevDistanceVec)
}
