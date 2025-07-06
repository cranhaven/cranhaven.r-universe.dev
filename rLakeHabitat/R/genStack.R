  #' Create Raster Stack
  #'
  #' Create a raster stack from a single raster, option to save as file.
  #'
  #' @param DEM raster object
  #' @param by numeric increment per unit depth by which layers are split. Default = 1
  #' @param stop optional numeric value specifying depth at which to stop stacking rasters, default = NULL
  #' @param save logical, save raster stack (TRUE) or not (FALSE), default = TRUE
  #' @param file_name character string used to name saved raster stack
  #' @param file_type character string defining file type to save, default = "COG"
  #' @return a raster stack of specified depth increments for a given waterbody. Raster stack is either stored as an object (save = FALSE) or written to a file in the directory (save = TRUE).
  #' @author Tristan Blechinger, Department of Zoology & Physiology, University of Wyoming
  #' @export
  #' @rawNamespace import(terra, except = c(union,intersect, animate))
  #' @examples
    #' #load raster
    #' DEM <- terra::rast(system.file("extdata", "example_raster.tif", package = 'rLakeHabitat'))
    #' #run function
    #' genStack(DEM, by = 1, save = FALSE)

genStack <- function(DEM, by = 1, stop = NULL, save = TRUE, file_name = NULL, file_type = "COG"){

  names <- as.data.frame(gdal(drivers = T))
  names <- as.list(names[,1])

  if(!inherits(DEM, "SpatRaster")){
    DEM <- terra::rast(DEM)
  }

  #checks
  #class testing
  if(!inherits(DEM, "SpatRaster"))
    stop("DEM must be a SpatRaster object or be able to be converted using the 'rast' function in package 'terra'.")
  if(by == 0)
    stop("by can't be zero")
  if (!is.numeric(by))
    stop("by value must be numeric.")
  if (!is.logical(save))
    stop("save must be either 'T', 'F', TRUE, or FALSE")
  if(save == T){
    if(is.null(file_name))
      stop("file_name cannot be NULL when save = T")
    if(is.null(file_type))
      stop("file_type cannot be NULL when save = T")
  }
  if(!is.null(file_name)){
    if(!is.character(file_name))
      stop("file_name must be a character")
  }
  if(!is.null(file_type)){
    if(!is.character(file_type))
      stop("file_type must be a character")
    if(!file_type %in% names)
      stop("file_type not accepted, use 'gdal(drivers = T)' to see accepted file types")
  }
  if(!is.null(stop)){
    if(!is.numeric(stop))
      stop("stop must be numeric")
    if(stop < by)
      stop("stop cannot be less than 'by'")
  }

  #use only first layer, either stack or single raster
  DEM <- DEM[[1]]

  #get max depth
  max_value <- as.numeric(max(values(DEM, na.rm = T)))

  #create empty list for output
  rastOut <- list()
  rastOut[[1]] <- DEM #save original to 1st spot (0ft)

  if(!is.null(stop)){
    max_value <- stop
  }

  depths <- base::seq(0, max_value, by = by) #create sequence of depths to max by 1

  for (i in 1:length(depths)) {

    DEM[DEM < depths[i]] <- NA #create raster for depth

    rastOut[[i+1]] <- DEM #save to list

  }
  rastStack <- terra::rast(rastOut) #turn list into stack

  if(save == T){
    #save output as COG
    terra::writeRaster(
    rastStack,
    paste(file_name, "rastStack", sep="_"),
    gdal = c("COMPRESS = LZW", "BLOCKSIZE = 128", "OVERVIEW = TRUE", "BIGTIFF = TRUE"),
    overwrite = TRUE,
    filetype = file_type)
  }
  if(save == F){
    return(rastStack)
  }
}
