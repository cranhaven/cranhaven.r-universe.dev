  #' Calculate Pelagic Habitat Volumes
  #'
  #' Calculates epilimnion, metalimnion, and hypolimnion volumes based on defined thermocline depths across water levels.
  #'
  #' @param DEM SpatRaster object of a given waterbody, rasters can be transformed to SpatRaster via the rast() function in 'terra'
  #' @param thermo_depth number giving the estimated middle of thermocline, results in calculation of only epilimnion and hypolimnion volumes. Default = NULL, cannot use in conjunction with thermo_low and thermo_high
  #' @param thermo_high number giving the upper bound of thermocline depth, results in calculation of epilimnion, metalimnion, and hypolimnion values
  #' @param thermo_low number giving the lower bound of thermocline depth, results in calculation of epilimnion, metalimnion, and hypolimnion values
  #' @param DEMunits character describing units of raster coordinate system. Can be meters, kilometers, or hectares ("m", "km", "ha"), default = "m"
  #' @param depthUnits character describing units of depth measurement. Can be either feet or meters ("ft", "m"), default = "ft"
  #' @param by numeric increment per unit by which volumes are calculated. Higher values will result in lower resolution. Default = 1
  #' @param stop optional numeric value specifying depth at which to stop habitat volume calculations, default = NULL
  #' @return a data frame of volumes in cubic meters calculated for each habitat (epilimnion, metalimnion, hypolimnion)
  #' @author Tristan Blechinger, Department of Zoology & Physiology, University of Wyoming
  #' @export
  #' @import dplyr
  #' @rawNamespace import(terra, except = c(union,intersect, animate))
  #' @examples
    #' #load raster
    #' DEM <- terra::rast(system.file("extdata", "example_raster.tif", package = 'rLakeHabitat'))
    #' #run function
    #' calcVolume(DEM, thermo_depth = 3, DEMunits = 'm', depthUnits = 'm')

calcVolume <- function(DEM, thermo_depth = NULL, thermo_high, thermo_low, DEMunits = "m", depthUnits = "ft", by = 1, stop = NULL){

  if(!inherits(DEM, "SpatRaster")){
    DEM <- terra::rast(DEM)
  }

  #class testing
  if(!inherits(DEM, "SpatRaster"))
    stop("DEM must be a SpatRaster object or be able to be converted using the 'rast' function in package 'terra'.")
  if(!DEMunits %in% c("m", "km", "ha"))
    stop("DEMunits misspecified. Please choose either 'm', 'km', or 'ha'")
  if(!depthUnits %in% c("m", "ft"))
    stop("depthUnits misspecified. Please choose either 'm' or 'ft'")
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

  #use only first layer, either stack or single raster
  DEM <- DEM[[1]]

  #get max depth
  max_value <- as.numeric(max(values(DEM, na.rm = T)))

  if(!is.null(thermo_depth)) {
    if(!is.numeric(thermo_depth) || is.na(thermo_depth)) {
      stop("thermo_depth must be numeric")
    }
    if(thermo_depth == 0) {
      stop("thermo_depth can't be zero")
    }
    if(thermo_depth > max_value){
      stop("thermo_depth cannot exceed maximum waterbody depth")
    }
    thermo_high <- NULL
    thermo_low <- NULL
  }
   else {
     if(is.null(thermo_high) || is.null(thermo_low)){
       stop("Both thermo_high and thermo_low must be provided when thermo_depth is NULL")
     }
     if (!is.numeric(thermo_low) || !is.numeric(thermo_high)) {
       stop("thermo_low and thermo_high values must be numeric.")
     }
     if (thermo_low == 0 || thermo_high == 0) {
       stop("thermo_low and thermo_high can't be zero.")
     }
     if (thermo_low < thermo_high) {
       stop("thermo_low must be greater than thermo_high.")
     }
     if(thermo_low > max_value || thermo_high > max_value){
       stop("thermo_low and thermo_high cannot exceed maximum waterbody depth")
     }
   }


  #thermo_depth
  if(!is.null(thermo_depth)){

    #modify thermo_depth to reflect increment value
    thermo_depth <- base::round(thermo_depth/by, digits = 0)

    #create sequence of depths by feet
    sequence <- base::seq(0, max_value, by=by)

    if(!is.null(stop)){
      stopsequence <- base::seq(0, stop, by = by)
    }
    else{
      stopsequence <- sequence
    }

    #create empty data frame for data
    habitat <- as.data.frame(matrix(nrow = length(sequence), ncol = 6))
    colnames(habitat) <- c("depth", "area", "cubic_m", "tot_vol_m3", "epi_vol_m3", "hyp_vol_m3")

    #get area at each value in sequence
    for(i in 1:length(sequence)){

      #filter depths by feet
      DEM[DEM < sequence[i]] <- NA

      #calculate and store areas of each layer
      area <- as.data.frame(terra::expanse(DEM, unit = DEMunits, byValue = F))
      habitat[i,2] <- area[1,2]
      habitat[i,1] <- sequence[i]
    }

    #convert area to volume for single layer
    #areas multiplied by 'by' for volume
    if(depthUnits == "ft" & DEMunits == "m"){
      habitat <- habitat %>%
        dplyr::mutate(cubic_m = area * 0.3048 * by)
    }
    if(depthUnits == "ft" & DEMunits == "km"){
      habitat <- habitat %>%
        dplyr::mutate(cubic_m = area * 1000000 * 0.3048 * by)
    }
    if(depthUnits == "ft" & DEMunits == "ha"){
      habitat <- habitat %>%
        dplyr::mutate(cubic_m = area * 10000 * 0.3048 * by)
    }
    if(depthUnits == "m" & DEMunits == "m"){
      habitat <- habitat %>%
        dplyr::mutate(cubic_m = area * by)
    }
    if(depthUnits == "m" & DEMunits == "km"){
      habitat <- habitat %>%
        dplyr::mutate(cubic_m = area * 1000000 * by)
    }
    if(depthUnits == "m" & DEMunits == "ha"){
      habitat <- habitat %>%
        dplyr::mutate(cubic_m = area * 10000 * 0.3048 * by)
    }

    #total volume
    for (i in 1:length(sequence)) {
      habitat[i,4] <- base::sum(habitat[i:length(sequence), 3])
    }

    #sum volumes for each wlf increment
    for (j in 1:length(stopsequence)) {
      #epilimnion = volume above thermocline
      habitat[j,5] <- base::round(base::sum(habitat[j:(j+thermo_depth),3]), digits = 2)
      #hypolimnion = total volume - volume below thermocline
      habitat[j,6] <- base::round(habitat[j,4] - habitat[j,5], digits = 2)
    }

    #add in acre ft, percentages of total, organize
    habitat <- habitat[c("depth", "tot_vol_m3", "epi_vol_m3", "hyp_vol_m3")]

    habitat <- habitat %>%
      dplyr::mutate(perc_epi_tot = base::round(habitat$epi_vol_m3/habitat$tot_vol_m3 * 100, digits = 2),
             perc_hyp_tot = base::round(habitat$hyp_vol_m3/habitat$tot_vol_m3 * 100, digits = 2),
             tot_vol_acft = habitat$tot_vol_m3 * 0.0008107132,
             epi_vol_acft = habitat$epi_vol_m3 * 0.0008107132,
             hyp_vol_acft = habitat$hyp_vol_m3 * 0.0008107132) %>%
      dplyr::relocate(c(habitat$tot_vol_acft, habitat$epi_vol_acft, habitat$hyp_vol_acft),
                      .before = habitat$perc_epi_tot)
  }

  #thermo_lowhigh
  if(is.null(thermo_depth)){

    #modify thermo_high & thermo_low to reflect increment value
    thermo_high <- base::round(thermo_high/by, digits = 0)
    thermo_low <- base::round(thermo_low/by, digits = 0)

      #create sequence of depths by feet
      sequence <- base::seq(0, max_value, by=by)

      if(!is.null(stop)){
        stopsequence <- base::seq(0, stop, by = by)
      }
      else{
        stopsequence <- sequence
      }

    #create empty data frame for data
      habitat <- as.data.frame(matrix(nrow = length(sequence), ncol = 7))
      colnames(habitat) <- c("depth", "area", "cubic_m", "tot_vol_m3", "epi_vol_m3", "met_vol_m3", "hyp_vol_m3")

    #get area at each value in sequence
    for(i in 1:length(sequence)){

      #filter depths by feet
      DEM[DEM < sequence[i]] <- NA

      #calculate and store areas of each layer
      area <- as.data.frame(terra::expanse(DEM, unit = DEMunits, byValue = F))
      habitat[i,2] <- area[1,2]
      habitat[i,1] <- sequence[i]
    }

      #convert area to volume for single layer
      #areas multiplied by 'by' for volume
      if(depthUnits == "ft" & DEMunits == "m"){
        habitat <- habitat %>%
          dplyr::mutate(cubic_m = area * 0.3048 * by)
      }
      if(depthUnits == "ft" & DEMunits == "km"){
        habitat <- habitat %>%
          dplyr::mutate(cubic_m = area * 1000000 * 0.3048 * by)
      }
      if(depthUnits == "ft" & DEMunits == "ha"){
        habitat <- habitat %>%
          dplyr::mutate(cubic_m = area * 10000 * 0.3048 * by)
      }
      if(depthUnits == "m" & DEMunits == "m"){
        habitat <- habitat %>%
          dplyr::mutate(cubic_m = area * by)
      }
      if(depthUnits == "m" & DEMunits == "km"){
        habitat <- habitat %>%
          dplyr::mutate(cubic_m = area * 1000000 * by)
      }
      if(depthUnits == "m" & DEMunits == "ha"){
        habitat <- habitat %>%
          dplyr::mutate(cubic_m = area * 10000 * 0.3048 * by)
      }

      #total volume
      for (i in 1:length(sequence)) {
        habitat[i,4] <- base::sum(habitat[i:length(sequence), 3])
      }

      #sum volumes for each wlf increment
      for (j in 1:length(stopsequence)) {
        #epilimnion = volume above thermocline
        habitat[j,5] <- base::round(base::sum(habitat[j:(j+thermo_high),3]), digits = 2)
        #hypolimnion = total volume - volume below thermocline
        habitat[j,7] <- base::round(habitat[j,4] - base::sum(habitat[j:(j+thermo_low), 3]), digits = 2)
        #metalimnion = total - epi - hypo
        habitat[j,6] <- base::round(habitat[j,4] - habitat[j,5] - habitat[j,7])
      }

      #add in acre ft, percentages of total, organize
      habitat <- habitat[c("depth", "tot_vol_m3", "epi_vol_m3", "met_vol_m3", "hyp_vol_m3")]

      habitat <- habitat %>%
        dplyr::mutate(perc_epi_tot = base::round(habitat$epi_vol_m3/habitat$tot_vol_m3 * 100, digits = 2),
               perc_met_tot = base::round(habitat$met_vol_m3/habitat$tot_vol_m3 * 100, digits = 2),
               perc_hyp_tot = base::round(habitat$hyp_vol_m3/habitat$tot_vol_m3 * 100, digits = 2),
               tot_vol_acft = habitat$tot_vol_m3 * 0.0008107132,
               epi_vol_acft = habitat$epi_vol_m3 * 0.0008107132,
               met_vol_acft = habitat$met_vol_m3 * 0.0008107132,
               hyp_vol_acft = habitat$hyp_vol_m3 * 0.0008107132) %>%
        dplyr::relocate(c(habitat$tot_vol_acft, habitat$epi_vol_acft, habitat$met_vol_acft, habitat$hyp_vol_acft),
                        .before = habitat$perc_epi_tot)

  }
  return(habitat)
 }
