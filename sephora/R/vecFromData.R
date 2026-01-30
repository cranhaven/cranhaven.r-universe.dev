#' Get numeric vector from RData file
#' 
#' Extract a numeric vector from an RData file
#' 
#' @param   product character indicating whether \code{data} comes from a \code{MOD13Q1} (default) time series satellite
#'                  imagery or from an \code{independent} product.
#' @param      data a matrix containing measurements of subsets (polygons) of a time series of satellite images. 
#'                  \code{nrow} is equal to the number of pixels in the polygon and \code{ncol} is equal 
#'                  to the number of images in the time series.
#' @param    numRow numeric, number of row to extract from \code{data}.
#' @param lenPeriod numeric, number of observations per period. Default, 23.
#' 
#' 
#' @export
#' 
#' @details Although the first available MOD13Q1 product dates back to 18-02-2000, 
#' when \code{product="MOD13Q1"} this function assumes that \code{data} contains observations from
#' 01-01-2000 and \code{\link[sephora]{fill_initialgap_MOD13Q1}} is used to impute
#' the first three missing values of 2000.
#' 
#' @seealso \code{\link{fill_initialgap_MOD13Q1}}, \code{\link[sephora]{phenopar}}, 
#' \code{\link[geoTS]{raster_intersect_sp}}, \code{\link{vecToMatrix}}.
#' 
#' @return A list with two components:
#' \item{mat}{extracted vector in matricial form}
#' \item{vec}{extracted vector}
#' 
vecFromData <- function(product=c("MOD13Q1", "independent"), data, 
                        numRow, lenPeriod=23){
  data_vec <- as.numeric(data[numRow,])
  data_mat <- vecToMatrix(x=data_vec, lenPeriod=lenPeriod)
  
  product <- match.arg(product)
  
  if( product == "MOD13Q1" ){
    data_mat[1,1:3] <- fill_initialgap_MOD13Q1(m=data_mat)
  }
  
list(mat=data_mat, vec=c(t(data_mat)))
}
