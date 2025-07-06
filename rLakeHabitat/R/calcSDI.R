  #' Calculate Shoreline Development Index
  #'
  #' Calculates Shoreline Development Index value across water levels for a given waterbody.
  #'
  #' @param DEM SpatRaster object of a given waterbody, rasters can be transformed to SpatRaster via the rast() function in 'terra'
  #' @param units character describing units of raster coordinate system. Can be meters, kilometers, or hectares ("m", "km", "ha"), default = "m"
  #' @param by numeric increment per unit depth by which areas are calculated. Higher values will result in lower resolution. Default = 1
  #' @param stop optional numeric value specifying depth at which to stop calculations, default = NULL
  #' @return data frame of perimeter lengths and SDI values for given depths
  #' @author Tristan Blechinger, Department of Zoology & Physiology, University of Wyoming
  #' @export
  #' @rawNamespace import(terra, except = c(union,intersect, animate))
  #' @examples
    #' #load raster
    #' DEM <- terra::rast(system.file("extdata", "example_raster.tif", package = 'rLakeHabitat'))
    #' #run function
    #' calcSDI(DEM, units = 'm')

calcSDI <- function(DEM, units = "m", by = 1, stop = NULL){

  if(!inherits(DEM, "SpatRaster")){
    DEM <- terra::rast(DEM)
  }

  #class testing
  if(!inherits(DEM, "SpatRaster"))
    stop("DEM must be a SpatRaster object or be able to be converted using the 'rast' function in package 'terra'.")
  if(!units %in% c("m", "km", "ha"))
    stop("units misspecified. Please choose 'm', 'km', or 'ha'")
  if(by == 0)
    stop("by can't be zero")
  if (!is.numeric(by))
    stop("by value must be numeric")
  if(!is.null(stop)){
    if(!is.numeric(stop))
      stop("stop must be numeric")
    if(stop < by)
      stop("stop cannot be less than 'by'")
  }

  #ensure SpatRaster format -- throw error if not?
  #use only first layer, either stack or single raster
  DEM <- DEM[[1]]

  #get max depth
  max_value <- as.numeric(max(values(DEM, na.rm = T)))

  if(is.null(stop)){
    stop <- max_value
  }

  #create sequence of depths by feet
  sequence <- base::seq(0, stop, by=by)

  #create empty data frame for data
  habitat <- as.data.frame(matrix(nrow = length(sequence), ncol = 4))
  colnames(habitat) <- c("depth", "tot_area", "perimeter", "SDI")

  #calculate SDI
  for (i in 1:length(sequence)) {

    #subset DEM to depth, get area
    temp <- DEM
    temp[temp <= sequence[i]] <- NA
    tot.area <- as.data.frame(terra::expanse(temp, unit = units, byValue = F))

    #reclassify to binary for one contour
    r_binary <- terra::classify(DEM, c(sequence[i], max_value, 0))

    #remove values outside water level
    r_binary[r_binary == 0] <- NA

    #Convert the binary raster to polygons
    polygons <- terra::as.polygons(r_binary)

    #Calculate the perimeter of the polygons
    perimeter <- terra::perim(polygons)

    habitat[i,1] <- sequence[i]
    habitat[i,2] <- base::round(tot.area[1,2], digits = 2)
    habitat[i,3] <- perimeter #save total length (m)
    habitat[i,4] <- perimeter/(2 * sqrt(3.14159 * habitat[i,2])) #save SDI
  }
  return(habitat)
}
