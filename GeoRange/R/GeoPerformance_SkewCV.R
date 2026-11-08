#' Function to calculate the skewness and coefficient of variance for a set of geographic range calculations
#' @param GeoRange - Data matrix containing the geographic range calculations for a set of taxa, as from the GeoRange_MultiTaxa() function
#' @return Returns a list of the skewness and coefficient of variance for each geographic range measure
#' @note The coefficient of variance returned is standard deviation/mean
#' @examples
#' \dontrun{
#' data(BivalvePBDB)
#' BivalveMatrix<-CoordList_PBDB(BivalvePBDB)
#' BivalveGeo<-GeoRange_MultiTaxa(OccMatrix=BivalveMatrix,TaxaStart=3)
#' GeoPerformance_SkewCV(BivalveGeo)
#' }
#' @seealso See the raster and moments packages for more details on the calculation of skewness and coefficient of variance
#' @export
GeoPerformance_SkewCV<-function(GeoRange){
  #library(raster)
  #library(moments)
  CV<-(apply(GeoRange,2,raster::cv))/100
  Skew<-apply(GeoRange,2,moments::skewness)
  res<-list(CoefVar=CV,Skewness=Skew)
  return(res)
}
