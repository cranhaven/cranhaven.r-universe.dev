  #' True Surface Area
  #'
  #' Calculate the true angular surface area of a DEM with the option to specify littoral area.
  #'
  #' @param DEM SpatRaster object of a given waterbody, rasters can be transformed to SpatRaster via the rast() function in 'terra'
  #' @param DEMunits character describing unit of measure for input raster grid
  #' @param CRSunits character describing CRS units of input raster, either "radians" or "degrees", default = "radians"
  #' @param neighbors numeric value describing number of neighbors with which to calculate cell slopes, must be 4 or 8, default = 4
  #' @param littoral logical describing if the surface area of the littoral zone across water levels is desired (TRUE) or a single surface area value (FALSE). Default = TRUE
  #' @param photic number giving the average photic depth, overwrites Secchi depth
  #' @param secchi number giving the average secchi depth, photic zone estimated as 2.6m * secchi
  #' @param depthUnits character describing units of depth measurement. Can be either feet or meters ("ft", "m"), default = "ft"
  #' @param by numeric increment per unit by which volumes are calculated. Higher values will result in lower resolution. Default = 1
  #' @param stop optional numeric value specifying depth at which to stop habitat volume calculations, default = NULL
  #' @return numeric surface area value (littoral = FALSE) or dataframe of surface area values (littoral = TRUE)
  #' @author Tristan Blechinger, Department of Zoology & Physiology, University of Wyoming
  #'
  #' @rawNamespace import(terra, except = c(union,intersect, animate))
  #'
  #' @examples
    #' #load raster
    #' DEM <- terra::rast(system.file("extdata", "example_raster.tif", package = 'rLakeHabitat'))
    #' #run function
    #' trueSA(DEM, DEMunits = "m", CRSunits = "radians", neighbors = 4, littoral = TRUE, photic = 2, depthUnits = "m", by = 1)
  #' @noRd

trueSA <- function(DEM, DEMunits = "m", CRSunits = "radians", neighbors = 4, littoral = TRUE, photic = NULL, secchi = NULL, depthUnits = "ft", by = 1, stop = NULL){

  if(!inherits(DEM, "SpatRaster")){
    DEM <- terra::rast(DEM)
  }

  #checks
  #class testing
  if(!inherits(DEM, "SpatRaster"))
    stop("DEM must be a SpatRaster object or be able to be converted using the 'rast' function in package 'terra'.")
  if(!DEMunits %in% c("m", "km", "ha"))
    stop("DEMunits misspecified. Please choose 'm', 'km', or 'ha'")
  if(!CRSunits %in% c("radians", "degrees"))
    stop("CRSunits misspecified. Please choose 'radians' or 'degrees'")
  if(!is.numeric(neighbors))
    stop("neighbors must be either 4 or 8")
  if(!neighbors %in% c(4,8))
    stop("neighbors must be either 4 or 8")
  if(!is.logical(littoral))
    stop("littoral must be either 'T' 'F', 'TRUE', or 'FALSE'")

  if(littoral == T || littoral == TRUE){

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

  #get cell resolution xy, convert to area units
  if(DEMunits == "m"){
    if(CRSunits == "radians"){
      center_lat <- as.numeric((terra::ext(DEM)[3] + terra::ext(DEM)[4])/2)
      resX <- terra::res(DEM)[1] * 40008000 / 360
      resY <- terra::res(DEM)[2] * 40008000 / 360 * base::cos(center_lat * pi / 180)
    }
    else{
      center_lat <- as.numeric((terra::ext(DEM)[3] + terra::ext(DEM)[4])/2)
      resX <- terra::res(DEM)[1] * 111319.49
      resY <- terra::res(DEM)[2] * 111319.49 * base::cos(center_lat * pi / 180)
    }
  }
  if(DEMunits == "km" || DEMunits == "ha"){
    if(CRSunits == "radians"){
      center_lat <- as.numeric((terra::ext(DEM)[3] + terra::ext(DEM)[4])/2)
      resX <- terra::res(DEM)[1] * 6371 * (pi / 180)
      resY <- terra::res(DEM)[2] * 6371 * (pi / 180) * base::cos(center_lat * pi / 180)
    }
    else{
      center_lat <- as.numeric((terra::ext(DEM)[3] + terra::ext(DEM)[4])/2)
      resX <- terra::res(DEM)[1] * 111.319
      resY <- terra::res(DEM)[2] * 111.319 * base::cos(center_lat * pi / 180)
    }
  }

  area <- resX * resY

  if(DEMunits == "ha"){
    area <- area * 100
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
    tot.slope <- terra::terrain(totDEM, "slope", unit = CRSunits, neighbors = neighbors)
    lit.slope <- terra::terrain(litDEM, "slope", unit = CRSunits, neighbors = neighbors)

    totsurface_area <- area/(base::cos(tot.slope))
    litsurface_area <- area/(base::cos(lit.slope))

    habitat[i,1] <- sequence[i]
    habitat[i,2] <- base::round(base::sum(terra::values(totsurface_area, na.rm = T)), digits = 2)
    habitat[i,3] <- base::round(base::sum(terra::values(litsurface_area, na.rm = T)), digits = 2)
  }

  habitat <- habitat %>%
    dplyr::mutate(perc_lit_tot = habitat$lit_area/habitat$tot_area)

  return(habitat)
  }

  if(littoral == F || littoral == FALSE){



    #get cell resolution xy, convert to area units
    if(DEMunits == "m"){
      if(CRSunits == "radians"){
        center_lat <- as.numeric((terra::ext(DEM)[3] + terra::ext(DEM)[4])/2)
        resX <- terra::res(DEM)[1] * 40008000 / 360
        resY <- terra::res(DEM)[2] * 40008000 / 360 * base::cos(center_lat * pi / 180)
      }
      else{
        center_lat <- as.numeric((terra::ext(DEM)[3] + terra::ext(DEM)[4])/2)
        resX <- terra::res(DEM)[1] * 111319.49
        resY <- terra::res(DEM)[2] * 111319.49 * base::cos(center_lat * pi / 180)
      }
    }
    if(DEMunits == "km" || DEMunits == "ha"){
      if(CRSunits == "radians"){
        center_lat <- as.numeric((terra::ext(DEM)[3] + terra::ext(DEM)[4])/2)
        resX <- terra::res(DEM)[1] * 6371 * (pi / 180)
        resY <- terra::res(DEM)[2] * 6371 * (pi / 180) * base::cos(center_lat * pi / 180)
      }
      else{
        center_lat <- as.numeric((terra::ext(DEM)[3] + terra::ext(DEM)[4])/2)
        resX <- terra::res(DEM)[1] * 111.319
        resY <- terra::res(DEM)[2] * 111.319 * base::cos(center_lat * pi / 180)
      }
    }

    area <- resX * resY

    if(DEMunits == "ha"){
      area <- area * 100
    }

    slope <- terra::terrain(DEM, "slope", unit = CRSunits, neighbors = neighbors)

    surface_area <- area/(base::cos(slope))


    return(base::round(base::sum(terra::values(surface_area, na.rm = T)), digits = 2))
  }
}
