  #' Generate Animated Plot
  #'
  #' Generate an animated plot of littoral area at different water level increments from a raster digital elevation model (DEM).
  #'
  #' @param DEM SpatRaster object of a given waterbody, rasters can be transformed to SpatRaster via the rast() function in 'terra'
  #' @param units character describing depth units of DEM. Can be meters ("m") or feet ("ft"). Default = "ft"
  #' @param littoral logical indicating if littoral zone should be plotted (T) or entire waterbody (F), default = TRUE
  #' @param secchi number giving the average Secchi depth of the waterbody, photic zone estimated as 2.6m * secchi
  #' @param photic number giving the average photic depth of the waterbody, overwrites Secchi
  #' @param stop optional numeric value specifying depth at which to stop animation, default = NULL (all depths)
  #' @param by numeric value specifying depth increments between plots. Higher values will result in lower resolution. Default = 1
  #' @return an animated ggplot object
  #' @author Tristan Blechinger, Department of Zoology & Physiology, University of Wyoming
  #' @export
  #' @import ggplot2
  #' @import dplyr
  #' @rawNamespace import(terra, except = c(union,intersect, animate))
  #' @import sf
  #' @import gganimate
  #' @examples
  #' \donttest{
    #' #load raster
    #' DEM <- terra::rast(system.file("extdata", "example_raster.tif", package = 'rLakeHabitat'))
    #' \dontshow{.old_wd <- setwd(tempdir())}
    #' #run function
    #' animBathy(DEM, units = 'm', littoral = TRUE, secchi = 1, by = 5)
    #' \dontshow{setwd(.old_wd)}}

animBathy <- function(DEM, units = "ft", littoral = TRUE, secchi = NULL, photic = NULL, stop = NULL, by = 1){

  if(!inherits(DEM, "SpatRaster")){
    DEM <- terra::rast(DEM)
  }

  #select first DEM layer
  DEM <- DEM[[1]]

  #class testing
  if(!inherits(DEM, "SpatRaster"))
    stop("DEM must be a SpatRaster object or be able to be converted using the 'rast' function in package 'terra'.")
  if(!units %in% c("m", "ft"))
    stop("units misspecified. Please choose 'm' or 'ft'")
  if(by == 0 || is.null(by) || is.na(by))
    stop("by must be specified")
  if (!is.numeric(by))
    stop("by value must be numeric.")
  if(!is.null(stop)){
    if(!is.numeric(stop))
      stop("stop must be numeric")
    if(stop < by)
      stop("stop cannot be less than by")
  }
  if(is.null(stop)){
    stop <- as.numeric(max(values(DEM, na.rm = T)))
  }
  if(!is.logical(littoral))
    stop("littoral must be logical: 'T', 'TRUE', 'F', or 'FALSE'")
  if(littoral == T || littoral == TRUE){
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
    if(units == "m"){
      photic <- secchi * 2.6 #estimate photic zone in m
    }
    if(units == "ft"){
      photic <- ((secchi/3.2808399) * 2.6) * 3.2808399 #estimate photic zone in ft
    }
    }
  }

  #create sequence of possible depths with WLF
  sequence <- base::seq(0, stop, by = by)

  #calculate photic depth from secchi depth
  if(units == "m"){
    photic <- secchi * 2.6 #estimate photic zone in m
  }
  else{
    photic <- ((secchi/3.2808399) * 2.6) * 3.2808399 #estimate photic zone in ft
  }

  #df list to store dataframes
  df_list <- list()

  #for loop to calculate habitat metrics
  if(littoral == T || littoral == TRUE){
    for (i in 1:length(sequence)) {

    #temp copy of DEM
    litDEM <- DEM
    litDEM[litDEM < sequence[i]] <- NA
    litDEM[litDEM > (sequence[i]+photic)] <- NA

    r_df <- as.data.frame(litDEM, xy = TRUE, cells = TRUE)

    r_df$wlf <- sequence[i]

    df_list[[i]] <- r_df
    }
  }
  if(littoral == F || littoral == FALSE){
    for (i in 1:length(sequence)) {

      #temp copy of DEM
      litDEM <- DEM
      litDEM[litDEM < sequence[i]] <- NA

      r_df <- as.data.frame(litDEM, xy = TRUE, cells = TRUE)

      r_df$wlf <- sequence[i]

      df_list[[i]] <- r_df
    }
  }

  #combine dataframes
  all_contours <- base::do.call("rbind", df_list)

  #rename column 4 to depth
  colnames(all_contours)[4] = "lyr1"

  #adjust depth
  all_contours <- all_contours %>%
    dplyr::mutate(depth = all_contours$lyr1 - all_contours$wlf) ###
  all_contours <- as.data.frame(all_contours)

  DEMout <- DEM
  DEMout[DEMout >= 0] <- 1
  outline <- terra::as.polygons(DEMout)
  outline <- sf::st_as_sf(outline)

  # generate animated plot
  frame_time <- seq(0, stop, by = by) * -1

  p <- ggplot() +
  geom_raster(data = all_contours, aes(x = all_contours$x, y = all_contours$y, fill = depth, group = all_contours$wlf)) + ##
  scale_fill_continuous(low = "blue", high = "lightblue", na.value="transparent", trans = "reverse") +
  geom_sf(data = outline, fill = NA) +
    theme(panel.background = element_rect(fill='transparent'),
          plot.background = element_rect(fill='transparent', color=NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent'))

  animated_plot <- p +
    gganimate::transition_time(all_contours$wlf) +
    labs(title = 'Water Level Drop: {round(frame_time, 0)}', x = '', y = '', fill = paste("Depth (", units, ")", sep = ""))

  return(animated_plot)
}
