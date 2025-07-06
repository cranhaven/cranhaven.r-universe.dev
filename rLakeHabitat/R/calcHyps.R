  #' Calculate Hypsography
  #'
  #' Calculates area at each depth for a given waterbody.
  #'
  #' @param DEM SpatRaster object of a given waterbody, rasters can be transformed to SpatRaster via the rast() function in 'terra'
  #' @param DEMunits character describing units of raster coordinate system. Can be meters, kilometers, or hectares ("m", "km", "ha"), default = "m"
  #' @param depthUnits character describing units of depth measurement. Can be either feet or meters ("ft", "m"), default = "ft"
  #' @param by numeric increment per unit by which volumes are calculated. Higher values will result in lower resolution. Default = 1
  #' @param output character describing desired output, can either be a data frame of values ("values") or a hypsography plot ("plot"). Default = "values"
  #' @return data frame of areas at each depth unit ("values") or a hypsography plot ("plot")
  #' @author Tristan Blechinger, Department of Zoology & Physiology, University of Wyoming
  #' @export
  #' @import ggplot2
  #' @rawNamespace import(terra, except = c(union,intersect, animate))
  #' @examples
    #' #load raster
    #' DEM <- terra::rast(system.file("extdata", "example_raster.tif", package = 'rLakeHabitat'))
    #' #run function
    #' calcHyps(DEM, DEMunits = 'm', depthUnits = 'm', by = 1, output = 'values')

calcHyps <- function(DEM, DEMunits = "m", depthUnits = "ft", by = 1, output = "values"){

  if(!inherits(DEM, "SpatRaster")){
    DEM <- terra::rast(DEM)
  }

  #class testing
  if(!inherits(DEM, "SpatRaster"))
    stop("DEM must be a SpatRaster object or be able to be converted using the 'rast' function in package 'terra'.")
  if(!DEMunits %in% c("m", "km", "ha"))
    stop("DEM units misspecified. Please choose either 'm', 'km', or 'ha'")
  if(by == 0)
    stop("by can't be zero")
  if (!is.numeric(by))
    stop("by value must be numeric.")
  if(!depthUnits %in% c("ft", "m"))
    stop("depthUnits not specified, must be 'ft' or 'm'")
  if(!output %in% c("values", "plot"))
    stop("output not specified, must be 'values' or 'plot'")

  #use only first layer, either stack or single raster
  DEM <- DEM[[1]]

  #get max depth
  max_value <- as.numeric(max(values(DEM, na.rm = T)))

  #create sequence of depths by feet
  sequence <- base::seq(0, max_value, by=by)

  #create empty data frame for data
  values <- as.data.frame(matrix(nrow = length(sequence), ncol = 2))
  colnames(values) <- c("depth", "area")

  #get area at each value in sequence
  for(i in 1:length(sequence)){

    #filter depths by feet
    DEM[DEM < sequence[i]] <- NA

    #calculate and store areas of each layer
    area <- as.data.frame(terra::expanse(DEM, unit = DEMunits, byValue = F))
    values[i,"area"] <- area[1,2]
    values[i,"depth"] <- sequence[i]
  }


  if(output == "values"){
    return(values)
  }
  if(output == "plot"){

    plot <- ggplot() +
      geom_line(data = values, aes(x = area, y = depth), color = 'blue', lwd = 1.5) +
      scale_y_reverse() +
      theme_bw() +
      theme(axis.title=element_text(size=18),
            title = element_text(size = 18),
            axis.text = element_text(size = 10),
            strip.text = element_text(size = 18)) +
      labs(title = "Hypsography Plot", x = paste("Area (", DEMunits, "\u00b2", ")", sep=""), y = paste("Depth (", depthUnits, ")", sep=""))

    return(plot)
  }
}
