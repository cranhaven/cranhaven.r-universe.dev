#' Plot presence/absence map
#'
#' @param x \code{Raster object}
#'
#' @return a \code{ggplot object}
#'
#' @import raster
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' r <- raster::raster(system.file("extdata","AETI.tif",package = "sdmApp"))
#' r <- r > 4000
#' sdmApp_PA(r)
sdmApp_PA<-function(x){
  samp <- raster::sampleRegular(x, 5e+05, asRaster = TRUE)
  map_df <- raster::as.data.frame(samp, xy = TRUE, centroids = TRUE,
                                  na.rm = TRUE)
  colnames(map_df) <- c("Easting", "Northing", "MAP")
  map_df$MAP<-factor(map_df$MAP)
  basePlot <- ggplot2::ggplot() + ggplot2::geom_raster(data = map_df,
                                                       ggplot2::aes_string(y = "Northing", x = "Easting", fill = "MAP"))
  basePlot<-basePlot + ggplot2::theme_bw() + ggplot2::labs(x = "Longitude", y = "Latitude") + ggplot2::ggtitle(label = names(x)) + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 10))+ggplot2::scale_fill_manual(values=c("red","green"),name="Specie",labels=c("Absence","Presence"))

  return(basePlot)
}
