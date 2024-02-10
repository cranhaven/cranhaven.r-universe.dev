#' Evaluate dataset quality
#'
#'@description
#'Evaluate the percentage of occurrences that fall on pixels assigned by NA values in the environmental RasterStack. It may provide interesting information to interpret model robustness.
#'
#'@usage
#'SDMdata.quality(data)
#'
#'
#'@param data \link{SDMtab} object or dataframe that contains id, longitude, latitude and values of environmental descriptors at corresponding locations
#'
#'@return
#'prop Dataframe that provides the proportion of NA values on which the presence data fall, for each environmental predictor
#'
#'
#'@seealso
#'\link{SDMeval}
#'
#' @examples
#'#Generate a SDMtab
#'data('ctenocidaris.nutrix')
#'occ <- ctenocidaris.nutrix
#'# select longitude and latitude coordinates among all the information
#'occ <- ctenocidaris.nutrix[,c('decimal.Longitude','decimal.Latitude')]
#'
#'library(raster)
#'data("predictors2005_2012")
#'envi <- predictors2005_2012
#'envi
#'
#'#Create the SDMtab matrix
#'SDMtable_ctenocidaris <- SDMPlay:::SDMtab(xydata=occ,
#'                                          predictors=predictors2005_2012,
#'                                          unique.data=FALSE,
#'                                          same=TRUE)
#'
#' # Evaluate the matrix quality
#' SDMPlay:::SDMdata.quality(data=SDMtable_ctenocidaris)


SDMdata.quality <- function(data) {
    # count the number of NA values in each environmental parameter
    table <- base::colSums(is.na(data[,-c(1:3)]))
    table <- base::data.frame(table)
    # calculate the proportion of NA values
    prop <- (table/base::nrow(data)) * 100
    base::colnames(prop) <- "NA.percent (%)"
    return(prop)
}


