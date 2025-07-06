  #' Plot Bathymetry Map
  #'
  #' Generate a bathymetry map from a provided DEM raster with optional contours and depth labels.
  #'
  #' @param DEM SpatRaster object of a given waterbody, rasters can be transformed to SpatRaster via the rast() function in 'terra'
  #' @param contours logical indicating whether contours should included (TRUE) or not (FALSE), default = TRUE
  #' @param start numeric value describing what value contours should start at, default = 0
  #' @param end numeric value describing what value contours should end at, default = max depth
  #' @param by numeric value describing contour intervals, default = 5
  #' @param breaks optional numeric vector describing specific contours to include if contours = T, default = NULL
  #' @param units character describing units of depth measurement, default = "ft"
  #' @param labels logical indicating whether labels should be included (TRUE) or not (FALSE), default = TRUE
  #' @param textSize number describing text size of contour labels if included, default = 1.5
  #' @param plotTitle optional character string adding title to output plot
  #' @return ggplot object
  #' @author Tristan Blechinger, Department of Zoology & Physiology, University of Wyoming
  #' @export
  #' @rawNamespace import(terra, except = c(union,intersect, animate))
  #' @import ggplot2
  #' @import tidyterra
  #' @import isoband
  #' @examples
    #' #load raster
    #' DEM <- terra::rast(system.file("extdata", "example_raster.tif", package = 'rLakeHabitat'))
    #' #run function
    #' bathyMap(DEM, contours = TRUE, units = 'm', labels = TRUE)

bathyMap <- function(DEM, contours = TRUE, start = NULL, end = NULL, by = 5, breaks = NULL, units = "ft", labels = TRUE, textSize = 1.5, plotTitle = NULL){

  if(!inherits(DEM, "SpatRaster")){
  DEM <- terra::rast(DEM)
  }

  #checks
  if(!inherits(DEM, "SpatRaster"))
    stop("DEM must be a SpatRaster object or be able to be converted using the 'rast' function in package 'terra'.")

  #identify maximum depth, isolate first DEM layer
  DEM <- DEM[[1]]
  max_value <- as.numeric(max(values(DEM, na.rm = T)))

  if(!inherits(units, "character"))
    stop("units must be a character")
  if(!inherits(textSize, "numeric"))
    stop("textSize must be numeric")
  if(!is.logical(contours))
    stop("contours must be either 'T', 'F', TRUE, or FALSE")
  if(contours == T || contours == TRUE){
    if(is.null(start)){
      start <- 0
    }
    if(is.null(end)){
      end <- max_value
    }
    if(is.na(by) || is.null(by) || !is.numeric(by)){
      stop("by must be specified and numeric when including contours.")
    }
    depths <- base::seq(start, end, by)
  }
  if(contours == F || contours == FALSE){
    start <- NULL
    end <- NULL
    by = NULL
    labels == F
  }
  if(!is.null(breaks)){
    if(!is.vector(breaks))
      stop("breaks must be in vector format: c(1,2,3,...)")
    if (all(base::sapply(breaks, function(x) !is.numeric(x))))
      stop("not all elements of breaks are numeric")
    if(contours == F)
      stop("contours must be T when including breaks")
    depths <- breaks
    start <- NULL
    end <- NULL
    by <- NULL
  }
  if(!is.null(plotTitle)){
    if(!is.character(plotTitle))
      stop("plotTitle must be a character string.")
  }

  df <- as.data.frame(DEM, xy = TRUE, cells = TRUE)

  #rename column 4 to depth
  colnames(df)[4] = "lyr1"

  DEMout <- DEM
  DEMout[DEMout >= 0] <- 1
  outline <- terra::as.polygons(DEMout)

  if(contours == F){
    bathymap <- ggplot() +
      geom_raster(data = df, aes(x = df$x, y = df$y, fill = df$lyr1)) +
      scale_fill_continuous(low = "blue", high = "lightblue", na.value="transparent", trans = "reverse") +
      geom_sf(data = outline, fill = NA) +
      theme(panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent')) +
      labs(fill = paste("Depth (", units, ")", sep = ""), x = "", y = "", title = plotTitle)
  }

  if(contours == T & labels == T){
    bathymap <- ggplot() +
    geom_raster(data = df, aes(x = df$x, y = df$y, fill = df$lyr1)) +
    scale_fill_continuous(low = "blue", high = "lightblue", na.value="transparent", trans = "reverse") +
    tidyterra::geom_spatraster_contour_text(data = DEM, breaks = depths,
      linewidth = .1, color = "grey10", family = "serif", size = textSize, label_placer = isoband::label_placer_minmax(n = 2)) +
    geom_sf(data = outline, fill = NA) +
    theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent')) +
    labs(fill = paste("Depth (", units, ")", sep = ""), x = "", y = "", title = plotTitle)
  }

  if(contours == T & labels == F){
    bathymap <- ggplot() +
      geom_raster(data = df, aes(x = df$x, y = df$y, fill = df$lyr1)) +
      scale_fill_continuous(low = "blue", high = "lightblue", na.value="transparent", trans = "reverse") +
      tidyterra::geom_spatraster_contour(data = DEM, breaks = depths,
                                   linewidth = .1, color = "grey10") +
      geom_sf(data = outline, fill = NA) +
      theme(panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent')) +
      labs(fill = paste("Depth (", units, ")", sep = ""), x = "", y = "", title = plotTitle)
  }
return(bathymap)
}
