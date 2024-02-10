#' Plot a  raster
#'
#' @param x \code{Raster object}
#'
#' @return a \code{ggplot object}
#'
#' @import raster
#'
#' @import ggplot2
#'
#' @import grDevices
#' @import rgdal
#' @export
#'
#' @examples
#' r <- raster::raster(system.file("extdata","AETI.tif",package = "sdmApp"))
#' sdmApp_RasterPlot(r)
sdmApp_RasterPlot<-function(x){
  if(grDevices::is.raster(x)){return(NULL)}
  samp <- raster::sampleRegular(x, 5e+05, asRaster = TRUE)
  map_df <- raster::as.data.frame(samp, xy = TRUE, centroids = TRUE,
                                  na.rm = TRUE)
  colnames(map_df) <- c("Easting", "Northing", "MAP")
  basePlot1 <- ggplot2::ggplot() + ggplot2::geom_raster(data = map_df,
                                                        ggplot2::aes_string(y = "Northing", x = "Easting", fill = "MAP"))
   basePlot1<-basePlot1 + ggplot2::theme_bw() + ggplot2::labs(x = "Longitude", y = "Latitude") +
     ggplot2::ggtitle(label = names(x))   + ggplot2::scale_fill_gradientn(name = " ", colours = rev(grDevices::terrain.colors(10)))
  basePlot1<-basePlot1 + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 10))

  return(basePlot1)

}
