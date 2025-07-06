  #' Calculate Littoral Volume
  #'
  #' Calculate littoral and pelagic volume across water levels from a DEM based on estimated photic depth.
  #'
  #' @param DEM SpatRaster object of a given waterbody, rasters can be transformed to SpatRaster via the rast() function in 'terra'
  #' @param DEMunits character describing units of raster coordinate system. Can be meters, kilometers, or hectares ("m", "km", "ha"), default = "m"
  #' @param secchi number giving the average secchi depth, photic zone estimated as 2.6m * secchi
  #' @param photic number giving the average photic depth, overwrites Secchi depth
  #' @param depthUnits character describing units of depth measurement. Can be either feet or meters ("ft", "m"), default = "ft"
  #' @param by numeric increment per unit by which volumes are calculated. Higher values will result in lower resolution. Default = 1
  #' @return data frame of littoral and pelagic volume estimates
  #' @author Tristan Blechinger, Department of Zoology & Physiology, University of Wyoming
  #' @export
  #' @rawNamespace import(terra, except = c(union,intersect, animate))
  #' @examples
    #' #load raster
    #' DEM <- terra::rast(system.file("extdata", "example_raster.tif", package = 'rLakeHabitat'))
    #' #run function
    #' littoralVol(DEM, photic = 2, DEMunits = "m", depthUnits = "m", by = 1)

littoralVol <- function(DEM, photic, secchi = NULL, DEMunits = "m", depthUnits = "ft", by = 1){

  if(!inherits(DEM, "SpatRaster")){
    DEM <- terra::rast(DEM)
  }

  #checks
  if(!inherits(DEM, "SpatRaster"))
    stop("DEM must be a SpatRaster object or be able to be converted using the 'rast' function in package 'terra'.")
  if(!DEMunits %in% c("m", "km", "ha"))
    stop("DEM units misspecified. Please choose 'm', 'km', or 'ha'")
  if(!depthUnits %in% c("m", "ft"))
    stop("Depth units misspecified. Please choose either 'm' or 'ft'")
  if(by == 0)
    stop("by can't be zero")
  if (!is.numeric(by))
    stop("by value must be numeric.")

  #use only first layer of stack or single raster
  DEM <- DEM[[1]]

  #get max depth
  max_value <- as.numeric(max(values(DEM, na.rm = T)))

  if(!is.null(secchi)){
    if(!is.numeric(secchi) || is.na(secchi))
      stop("secchi must be numeric")
    if(secchi == 0)
      stop("secchi cannot be zero")
    if(secchi > max_value)
      stop("secchi cannot exceed maximum waterbody depth")
    if(depthUnits == "m"){
      photic <- secchi * 2.6 #estimate photic zone in m
    }
    else{
      photic <- ((secchi/3.2808399) * 2.6) * 3.2808399 #estimate photic zone in ft
    }
  }
  else{
    if(!is.numeric(photic) || is.na(photic))
      stop("photic must be numeric")
    if(photic == 0)
      stop("photic cannot be zero")
    if(photic > max_value)
      stop("photic cannot exceed maximum waterbody depth")
    secchi <- NULL
  }

  #create sequence of depths by feet
  sequence <- base::seq(0, max_value, by=by)

  #create empty data frame for data
  df <- as.data.frame(matrix(nrow = length(sequence), ncol = 12))
  colnames(df) <- c("depth", "tot_area", "lit_area", "pel_area", "tot_cubic_m", "lit_cubic_m", "pel_cubic_m", "tot_vol_m3", "lit_vol_m3", "pel_vol_m3", "perc_lit_tot", "perc_pel_tot")

  #get area at each value in sequence
  for(i in 1:length(sequence)){

    #subset DEM
    DEM[DEM < sequence[i]] <- NA

    #copy of DEM
    litDEM <- DEM
    litDEM[litDEM < sequence[i]] <- NA
    litDEM[litDEM > (sequence[i]+photic)] <- NA

    #calculate and store areas
    tot.area <- as.data.frame(terra::expanse(DEM, unit = DEMunits, byValue = F))
    lit.area <- as.data.frame(terra::expanse(litDEM, unit = DEMunits, byValue = F))

    df[i,1] <- sequence[i]
    df[i,2] <- base::round(tot.area[1,2], digits = 2)
    df[i,3] <- base::round(lit.area[1,2], digits = 2)
    df[i,4] <- df[i,2]-df[i,3]
  }

  #convert area to volume for single layer
  #areas multiplied by 'by' for volume
  if(depthUnits == "ft" & DEMunits == "m"){
      df <- df %>%
        dplyr::mutate(tot_cubic_m = df$tot_area * 0.3048 * by,
               lit_cubic_m = df$lit_area * 0.3048 * by,
               pel_cubic_m = df$pel_area * 0.3048 * by)
  }
  if(depthUnits == "ft" & DEMunits == "km"){
    df <- df %>%
      dplyr::mutate(tot_cubic_m = df$tot_area * 1000000 * 0.3048 * by,
             lit_cubic_m = df$lit_area * 1000000 * 0.3048 * by,
             pel_cubic_m = df$pel_area * 1000000 * 0.3048 * by)
  }
  if(depthUnits == "ft" & DEMunits == "ha"){
    df <- df %>%
      dplyr::mutate(tot_cubic_m = df$tot_area * 10000 * 0.3048 * by,
             lit_cubic_m = df$lit_area * 10000 * 0.3048 * by,
             pel_cubic_m = df$pel_area * 10000 * 0.3048 * by)
  }
  if(depthUnits == "m" & DEMunits == "m"){
    df <- df %>%
      dplyr::mutate(tot_cubic_m = df$tot_area * by,
             lit_cubic_m = df$lit_area * by,
             pel_cubic_m = df$pel_area * by)
  }
  if(depthUnits == "m" & DEMunits == "km"){
    df <- df %>%
      dplyr::mutate(tot_cubic_m = df$tot_area * 1000000 * by,
             lit_cubic_m = df$lit_area * 1000000 * by,
             pel_cubic_m = df$pel_area * 1000000 * by)
  }
  if(depthUnits == "m" & DEMunits == "ha"){
    df <- df %>%
      dplyr::mutate(tot_cubic_m = df$tot_area * 10000 * 0.3048 * by,
             lit_cubic_m = df$lit_area * 10000 * 0.3048 * by,
             pel_cubic_m = df$pel_area * 10000 * 0.3048 * by)
  }

  #calculate total volumes
  for (i in 1:length(sequence)) {
    df[i,8] <- base::sum(df[i:length(sequence), 5])
    df[i,9] <- base::sum(df[i:length(sequence), 6])
    df[i,10] <- base::sum(df[i:length(sequence), 7])
  }

  #add in acre ft, percentages of total, organize
  df <- df[c("depth", "tot_vol_m3", "lit_vol_m3", "pel_vol_m3")]

  df <- df %>%
    dplyr::mutate(perc_lit_tot = base::round(df$lit_vol_m3/df$tot_vol_m3 * 100, digits = 2),
           perc_pel_tot = base::round(df$pel_vol_m3/df$tot_vol_m3 * 100, digits = 2),
           tot_vol_acft = df$tot_vol_m3 * 0.0008107132,
           lit_vol_acft = df$lit_vol_m3 * 0.0008107132,
           pel_vol_acft = df$pel_vol_m3 * 0.0008107132) %>%
    dplyr::relocate(c(df$tot_vol_acft, df$lit_vol_acft, df$pel_vol_acft), .before = df$perc_lit_tot)

  return(df)
}
