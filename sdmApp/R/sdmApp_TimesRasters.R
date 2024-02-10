#' Multiply the probability of occurrence map with the presence/absence map to get a presence map only.
#'
#' @param x Probability of occurrence map, a \code{Raster object}
#' @param y Presence/Absence map, a \code{Raster object}
#'
#' @return Probability of occurrence map with only presence
#' @export
#'
#' @examples
#' r <- raster::raster(system.file("extdata","AETI.tif",package = "sdmApp"))
#' r2 <- r > raster::cellStats(r, stat='mean', na.rm=TRUE)
#' r <- r/raster::maxValue(r)
#' names(r) <- "propability of occurence"
#' z<-sdmApp_TimesRasters(r,r2)
#' sdmApp_RasterPlot(z)
sdmApp_TimesRasters<-function(x,y){
  z<-x * y
  names(z)<-names(x)
  return(z)
}
