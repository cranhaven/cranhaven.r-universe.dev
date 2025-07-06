  #' Calculate Littoral Area
  #'
  #' Calculates littoral surface area (2D) of a given waterbody across water levels based on an average photic depth value.
  #'
  #' @param DEM SpatRaster object of a given waterbody, rasters can be transformed to SpatRaster via the rast() function in 'terra'
  #' @param photic number giving the average photic depth, overwrites Secchi depth
  #' @param secchi number giving the average secchi depth, photic zone estimated as 2.6m * secchi.
  #' @param DEMunits character describing units of raster coordinate system. Can be meters, kilometers, or hectares ("m", "km", "ha"), default = "m"
  #' @param depthUnits character describing units of depth measurement (secchi and DEM). Can be either feet or meters ("ft", "m"), default = "ft"
  #' @param by numeric increment per unit depth by which areas are calculated. Higher values will result in lower resolution. Default = 1
  #' @param stop optional numeric value specifying depth at which to stop calculations, default = NULL
  #' @return data frame of areas in specified units for each depth, as well as the littoral percentage of total surface area
  #' @author Tristan Blechinger, Department of Zoology & Physiology, University of Wyoming
  #' @export
  #' @import dplyr
  #' @rawNamespace import(terra, except = c(union,intersect, animate))
  #' @examples
    #' #load raster
    #' DEM <- terra::rast(system.file("extdata", "example_raster.tif", package = 'rLakeHabitat'))
    #' #run function
    #' calcLittoral(DEM, secchi = 1, depthUnits = "m", DEMunits = "m")

calcLittoral <- function(DEM, photic = NULL, secchi = NULL, DEMunits = "m", depthUnits = "ft", by = 1, stop = NULL){

  if(!inherits(DEM, "SpatRaster")){
    DEM <- terra::rast(DEM)
  }

  #class testing
  if(!inherits(DEM, "SpatRaster"))
    stop("DEM must be a SpatRaster object or be able to be converted using the 'rast' function in package 'terra'.")
  if(!DEMunits %in% c("m", "km", "ha"))
    stop("DEMunits misspecified. Please choose 'm', 'km', or 'ha'")
  if(!depthUnits %in% c("m", "ft"))
    stop("depthUnits misspecified. Please choose 'm' or 'ft'")
  if(by == 0)
    stop("by can't be zero")
  if (!is.numeric(by))
    stop("by value must be numeric.")
  if(!is.null(stop)){
    if(!is.numeric(stop))
      stop("stop must be numeric")
    if(stop < by)
      stop("stop cannot be less than by")
  }

  #use only first layer of either stack or single raster
  DEM <- DEM[[1]]

  #get max depth
  max_value <- as.numeric(max(values(DEM, na.rm = T)))

  if(is.null(secchi) & is.null(photic))
    stop("Either secchi or photic must be defined when including littoral area.")
  if(!is.null(photic)){
    if(!is.numeric(photic) || is.na(photic))
      stop("photic must be numeric")
    if(photic == 0)
      stop("photic cannot be zero")
    secchi <- NULL
  }
  if(!is.null(secchi)){
    if(!is.numeric(secchi) || is.na(secchi))
      stop("secchi must be numeric")
    if(secchi == 0)
      stop("secchi cannot be zero")
    if(depthUnits == "m"){
      photic <- secchi * 2.6 #estimate photic zone in m
    }
    if(depthUnits == "ft"){
      photic <- ((secchi/3.2808399) * 2.6) * 3.2808399 #estimate photic zone in ft
    }
  }

  if(!is.null(stop)){
    max_value <- stop
  }

  #create sequence of depths by feet
  sequence <- base::seq(0, max_value, by=by)

  #create empty data frame for data
  habitat <- as.data.frame(matrix(nrow = length(sequence), ncol = 4))
  colnames(habitat) <- c("depth", "tot_area", "lit_area", "perc_lit_tot")

  #calculate total area
  for (i in 1:length(sequence)) {

    #temp copy of DEM
    totDEM <- DEM
    totDEM[totDEM < sequence[i]] <- NA

    #temp copy of DEM
    litDEM <- DEM
    litDEM[litDEM < sequence[i]] <- NA
    litDEM[litDEM > (sequence[i]+photic)] <- NA

    #calculate and store areas
    tot.area <- as.data.frame(expanse(totDEM, unit = DEMunits, byValue = F))
    lit.area <- as.data.frame(expanse(litDEM, unit = DEMunits, byValue = F))

    habitat[i,1] <- sequence[i]
    habitat[i,2] <- base::round(tot.area[1,2], digits = 2)
    habitat[i,3] <- base::round(lit.area[1,2], digits = 2)
  }

  habitat <- habitat %>%
    dplyr::mutate(perc_lit_tot = habitat$lit_area/habitat$tot_area)

  return(habitat)
}
