#' Compile species distribution dataset for modelling
#'
#'@description Create a dataframe that contains the required information to implement species distribution models
#'
#'@usage
#'SDMtab(xydata, predictors, unique.data = TRUE, same = TRUE,
#'      background.nb=NULL, KDE_layer=NULL)
#'
#'@param xydata Dataframe with longitude (column 1) and latitude (column 2) of the presence-only data. Decimal longitude and latitude are required.
#'@param predictors Rasterstack of environmental descriptors. Used to extract values of the presence location
#'@param unique.data If TRUE (by default), duplicate presence points, that fall in the same grid cell, will be removed
#'@param same If TRUE (by default), the number of background data sampled in the area equals the number of presence data
#'@param background.nb Set as NULL if same= TRUE.
#'@param KDE_layer Rasterlayer that describes the frequency of visits in the area (i.e. the spatial bias that could be present in the occurrence dataset)
#'
#'@details
#'Background data are sampled randomly (without replacement) among the entire area, on pixels that are not assigned NA. It constitutes a summary of environmental descriptors to improve modelling performance. See Barbet Massin et al. (2012) for further information about background selection.
#'
#'@return
#' A dataframe that contains the id (1 for presence, 0 for background data) of data, their longitude, latitude and extracted values of environmental descriptors at the corresponding locations.
#'
#'xydata for which coordinates fall out of the RasterStack extent are removed from the analysis.
#'
#'@references
#'Barbet Massin M, F Jiguet, C Albert & W Thuiller (2012) Selecting pseudo absences for species distribution models: how, where and how many? \emph{Methods in Ecology and Evolution}, 3(2): 327-338.
#'
#'@seealso
#'\link{delim.area} to refine the environmental RasterStack before using this function
#'
#'@examples
#'#Open occurrence data
#'data('ctenocidaris.nutrix')
#'occ <- ctenocidaris.nutrix
#'
#'#Open environmental descriptors RasterStack
#'data(predictors2005_2012)
#'envi <- predictors2005_2012
#'envi
#'
#'#create the dataframe for modelling
#'z <- SDMPlay:::SDMtab(xydata=occ[,c('decimal.Longitude','decimal.Latitude')],predictors=envi)
#'head(z)
#'

SDMtab <- function(xydata, predictors, unique.data = TRUE, same = TRUE, background.nb=NULL, KDE_layer=NULL) {

    # cleaning the xydata file and removing the data that are out of the extent of the RasterStack
    xydata <- base::subset(xydata, xydata[, 1] >= predictors@extent@xmin & xydata[, 1] <= predictors@extent@xmax)
    xydata <- base::subset(xydata, xydata[, 2] >= predictors@extent@ymin & xydata[, 2] <= predictors@extent@ymax)

    # extracting the environmental variables associated with the presence data
    presvals <- raster::extract(predictors, xydata)
    presvals.cellnb <- raster::extract(predictors, xydata, cellnumbers = T)

    # remove the double occurrences
    if (unique.data == TRUE) {
        presvals <- unique(presvals)
        presvals.cellnb <- presvals.cellnb[!base::duplicated(presvals.cellnb[,-1]),]
    }

    # extract background locations (random sampling of latitude and longitudes in the area)
    if(!is.null(background.nb)){
      same <- FALSE
    }

    if (is.null(KDE_layer)){
      if (same == TRUE) {
        background.number <- dismo::randomPoints(predictors, n = nrow(presvals))
      } else {
        background.number <- dismo::randomPoints(predictors, n = background.nb)
      }
      colnames(background.number) <- colnames(xydata)
    } else {
      if (same == TRUE) {
        background.number <- raster::xyFromCell(KDE_layer, sample(which(!is.na(raster::values(KDE_layer))), nrow(presvals), prob=raster::values(KDE_layer)[!is.na(raster::values(KDE_layer))]))
      } else {
        background.number <- raster::xyFromCell(KDE_layer, sample(which(!is.na(raster::values(KDE_layer))), background.nb, prob=raster::values(KDE_layer)[!is.na(raster::values(KDE_layer))]))
              }
      colnames(background.number) <- colnames(xydata)
    }


    # extract environmental values at these locations
    pseudoabs.vals <- raster::extract(predictors, background.number)
    pseudoabs.vals.cellnb <- raster::extract(predictors, background.number, cellnumbers = T)

    # build the id column if
    id <- c(rep(1, nrow(presvals)), rep(0, nrow(pseudoabs.vals)))

    # build the final table latitudes and longitudes of the presence and background data
    xypres <- raster::xyFromCell(raster::subset(predictors, 1), presvals.cellnb[, 1])
    xyback <- raster::xyFromCell(raster::subset(predictors, 1), pseudoabs.vals.cellnb[, 1])
    coord <- base::rbind(xypres, xyback)

    inter <- base::rbind(presvals, pseudoabs.vals)
    SDMtab.object <- base::data.frame(base::cbind(id, coord,inter ),check.rows=T)
    colnames(SDMtab.object) <- c("id", "longitude", "latitude", colnames(presvals))
    return(SDMtab.object)
}
