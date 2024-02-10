#' Explore the generated folds and visualize the placement of folds and distribution of species data over folds.
#'
#' @param blocks A \code{SpatialBlock} object.
#' @param rasterLayer A raster object as background map for visualization.
#' @param speciesData A simple features (sf) or \code{SpatialPoints} object containing species data (response variable).
#' @param num A number of fold to assign as data test set.
#'
#' @return A map showing folds and the species data, that can be used to explore folds.
#' @export
#'
#'
#' @import raster
#'
#' @import ggplot2
#'
#' @import sf
#'
#' @import stats
#'
#' @import blockCV
#'
#' @importFrom graphics plot
#' @examples
#' \dontrun{
#' # load blockCV package data
#' library(blockCV)
#' awt <- raster::brick(system.file("extdata", "awt.grd", package = "blockCV"))
#' #import presence-absence species data
#' PA <- read.csv(system.file("extdata", "PA.csv", package = "blockCV"))
#'#make a sf object from data.frame
#' pa_data <- sf::st_as_sf(PA, coords = c("x", "y"), crs = raster::crs(awt))
#' #spatial blocking by specified range and random assignment
#' sb <- spatialBlock(speciesData = pa_data,species = "Species",
#' rasterLayer = awt,theRange = 70000,k = 5,
#' selection = "random",iteration = 100)
#' sdmApp_fold_Explorer(sb,awt,pa_data,1)
#'}
sdmApp_fold_Explorer<-function (blocks, rasterLayer, speciesData, num) {
  if (is.null(rasterLayer)) {
    stop("A raster layer should be provided")
  }
  else if (is.null(speciesData)) {
    stop("Species data should be provided")
  }
  else if (is.null(blocks)) {
    stop("An object of SpatialBlock is needed")
  }
  if (class(blocks) == "SpatialBlock") {
    polyObj <- blocks$blocks
  }
  else {
    polyObj <- NULL
  }

  if (num<0 || num>length(blocks$folds)){
    stop("The num parameter is incorrect!")
  }
  folds <- blocks$folds
  kmax <- length(folds)
  species <- blocks$species
  speciesData <- sf::st_as_sf(speciesData)
  speciesData[[1]] <- as.factor(speciesData[[1]])
  samp <- raster::sampleRegular(rasterLayer[[1]], 5e+05, asRaster = TRUE)
  map_df <- raster::as.data.frame(samp, xy = TRUE, centroids = TRUE,
                                  na.rm = TRUE)
  colnames(map_df) <- c("Easting", "Northing", "MAP")
  mid <- stats::median(map_df$MAP)
  basePlot <- ggplot2::ggplot() + ggplot2::geom_raster(data = map_df,
                                                       ggplot2::aes_string(y = "Northing", x = "Easting", fill = "MAP")) +
    ggplot2::scale_fill_gradient2(low = "darkred", mid = "yellow",
                                  high = "darkgreen", midpoint = mid) + ggplot2::guides(fill = FALSE) +
    ggplot2::theme_bw() + ggplot2::labs(x = "", y = "")
  trainSet <- unlist(folds[[num]][1])
  testSet <- unlist(folds[[num]][2])
  training <- speciesData[trainSet, ]
  testing <- speciesData[testSet, ]
  plotPoly <- polyObj[polyObj$folds ==num,]
  plotPoly <- sf::st_as_sf(plotPoly)
  if (is.null(species)) {
    if (class(blocks) == "SpatialBlock") {
      ptr <- basePlot + ggplot2::geom_sf(data = plotPoly,
                                         color = "red", fill = "orangered4", alpha = 0.04,
                                         size = 0.2) + ggplot2::geom_sf(data = training,
                                                                        alpha = 0.7, color = "blue", size = 2) +
        ggplot2::ggtitle("Training set") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 10))
      pts <- basePlot + ggplot2::geom_sf(data = plotPoly,
                                         color = "red", fill = "orangered4", alpha = 0.04,
                                         size = 0.2) + ggplot2::geom_sf(data = testing,
                                                                        alpha = 0.7, color = "blue", size = 2) +
        ggplot2::ggtitle("Testing set") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 10))
    }
    else {
      ptr <- basePlot + ggplot2::geom_sf(data = training,
                                         alpha = 0.7, color = "blue", size = 2) +
        ggplot2::ggtitle("Training set") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 10))
      pts <- basePlot + ggplot2::geom_sf(data = testing,
                                         alpha = 0.7, color = "blue", size = 2) +
        ggplot2::ggtitle("Testing set") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 10))
    }
  }
  else {
    if (class(blocks) == "SpatialBlock") {
      ptr <- basePlot + ggplot2::geom_sf(data = plotPoly,
                                         color = "red", fill = "orangered4", alpha = 0.04,
                                         size = 0.2) + ggplot2::geom_sf(data = training,
                                                                        ggplot2::aes(color = get(species)), show.legend = "point",
                                                                        alpha = 0.7, size = 2) + ggplot2::labs(color = species) +
        ggplot2::ggtitle("Training set") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 10))
      pts <- basePlot + ggplot2::geom_sf(data = plotPoly,
                                         color = "red", fill = "orangered4", alpha = 0.04,
                                         size = 0.2) + ggplot2::geom_sf(data = testing,
                                                                        ggplot2::aes(color = get(species)), show.legend = "point",
                                                                        alpha = 0.7, size = 2) + ggplot2::labs(color = species) +
        ggplot2::ggtitle("Testing set") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 10))
    }
    else {
      ptr <- basePlot + ggplot2::geom_sf(data = training,
                                         ggplot2::aes(color = get(species)), show.legend = "point",
                                         alpha = 0.7, size = 2) + ggplot2::labs(color = species) +
        ggplot2::ggtitle("Training set") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 10))
      pts <- basePlot + ggplot2::geom_sf(data = testing,
                                         ggplot2::aes(color = get(species)), show.legend = "point",
                                         alpha = 0.7, size = 2) + ggplot2::labs(color = species) +
        ggplot2::ggtitle("Testing set") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 10))
    }
  }
  cowplot::plot_grid(ptr, pts)
}
