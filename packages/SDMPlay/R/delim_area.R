#' RasterStack preparation for modelling
#'
#'@description Delimit the RasterStack of environmental descriptors at a precise extent (latitude, longitude, maximum depth...) before computing species distribution modelling
#'
#' @usage
#' delim.area(predictors, longmin, longmax, latmin, latmax, interval=NULL,
#'            crslayer = raster::crs(predictors))
#'
#' @param predictors RasterStack object that contains the environmental predictors used for species distribution models
#' @param longmin Expected minimum longitude of the RasterStack
#' @param longmax Expected maximum longitude of the RasterStack
#' @param latmin Expected minimum latitude of the RasterStack
#' @param latmax Expected maximum latitude of the RasterStack
#' @param interval Vector of 2. Minimum and maximum values outside of which the pixel values of the RasterStack first layer will be assigned to NA values. Set as NULL by default (no treatment).
#' @param crslayer CRS object or character string describing a projection and datum. The crs of the original RasterStack is set by default
#'
#'@details
#' \emph{interval} enable the user to delimit the RasterStack according to an interval of values applied on the \strong{first layer} of the RasterStack. It is often applied on depth in SDM studies.
#'
#'Missing values contained in the provided RasterStack must be set up as NA values.
#'
#'@seealso
#'\link[raster]{stack}, \link[raster]{raster}, \link[raster]{origin}, \link[raster]{extent}
#'
#'@return RasterLayer object
#'
#' @examples
#' data('predictors2005_2012')
#' envi <- predictors2005_2012
#'
#' r <- SDMPlay:::delim.area(predictors = envi,
#' longmin = 70,longmax = 75, latmin = -50,latmax = -40,interval = c(0,-1000))
#' r
#'
#' library(grDevices) # plot the result with nice colors
#' palet.col <- colorRampPalette(c('deepskyblue','green','yellow', 'red'))(80)
#' raster::plot(r, col=palet.col)

delim.area <- function(predictors, longmin, longmax, latmin, latmax, interval=NULL, crslayer = raster::crs(predictors)) {

    if (!requireNamespace("raster", quietly = TRUE)) {
        stop("The function requires 'raster' package to work, please install it.", call. = FALSE)
    }

    data <- predictors
    data <- raster::crop(data, raster::extent(longmin, longmax, latmin, latmax))
    raster::crs(data) <- crslayer
    stackpred.final <- data

if(is.null(interval)==FALSE){
    # transform the NA values in -99999 values
    layertochange <- raster::subset(data, subset = 1)
    layertochange <- raster::reclassify(layertochange, cbind(NA, -99999))
    layertochange_v <- raster::rasterToPoints(layertochange)

    nullraster <- layertochange
    raster::values(nullraster) <- 0  # create a null raster to implement rasterize

    if(interval[1]>interval[2]){
      for (i in 1:nrow(layertochange_v)) {
        if (layertochange_v[i, 3] <= interval[1] & layertochange_v[i, 3] >= interval[2]) {
          layertochange_v[i, 3] <- layertochange_v[i, 3]
        } else {

          layertochange_v[i, 3] <- (-99999)
        }
      }
    }

    if(interval[1]<interval[2]){
      for (i in 1:nrow(layertochange_v)) {
        if (layertochange_v[i, 3] >= interval[1] & layertochange_v[i, 3] <= interval[2]) {
          layertochange_v[i, 3] <- layertochange_v[i, 3]
        } else {

          layertochange_v[i, 3] <- (-99999)
        }
      }
    }


    layertochange <- raster::rasterize(as.matrix(layertochange_v[, 1:2]), nullraster, as.vector(layertochange_v[,
        3]))
    layertochange <- raster::reclassify(layertochange, base::cbind(-99999, NA))
    layertochange <- raster::crop(layertochange, raster:: extent(longmin, longmax, latmin, latmax))

    stackpred.final <- raster::stack(layertochange, raster::subset(data, c(2:raster::nlayers(data))))
    names(stackpred.final) <- c(names(predictors))
}

    return(stackpred.final)

}


