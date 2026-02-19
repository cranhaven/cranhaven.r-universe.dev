#' @importFrom terra resample rast
#' 
#' 
#' @title Get and prepare correction
#' @author Johannes De Groeve
#' @description Get and prepare a correction dataset
#'
#' @param correction SpatRaster. Correction value, vector, grid, or list of grids to account for spatial-(non-)explicit and temporal (non-)linear changes in the topography (e.g., uplift and subsidence rates, sedimentation and erosion ticknesses)
#' @param topo SpatRaster. Topographic/Bathymetric model as SpatRaster or path to dataset. The topo projection is the reference for further outputs. 
#' @param curve SpatRaster. Curve value, vector, grid or list of grids indicating the relative altitude of a biogeographic system per time period compared to the present. A typical example is a sea level curve indicating the relative sea level position above or below sea level compared to the present.
#' @param units numeric. Units of topo, curve and correction provided as a list (default: units=list(topo='m', curve=c(names='yr', value='m'), correction='mm/yr'))
#' @param verbose boolean. FALSE: No messages are printed. TRUE: Standard verbose mode 2: Very verbose mode, displaying detailed information. 
#' 
#' @return A SpatRaster or vector with corrrection values in a suitable format for the reconstruct function, including a value for each time step, defined by the curve.
#'
#' @seealso \href{https://uva_ibed_piac.gitlab.io/tabs/articles/Bb-tabs-correction.html}{correction}
#' 
#' 
#' 
#' 
#' @export
#'
#' @examples
#' 
#' sporades <- sporades()
#' topo <- sporades$topo
#' correction <- sporades$correction
#' curve <- sporades$curve
#' 
#' cor <- get_correction(correction=correction,
#'                       topo=topo,
#'                       curve=curve)
#' 
get_correction <- function(correction=NULL,
                           topo=NULL, 
                           curve=NULL, 
                           units=list(topo='m',curve=c(names='yr',value='m'),correction='mm/yr'),
                           verbose=FALSE){
  
  # make sure the format of the curve is correct
  curve <- get_curve(curve)
  
  if(is.null(correction)){
    #tectonic_uplift_m <- list(0)
    tectonic_uplift_m <- 0
    names(tectonic_uplift_m) <- 0
    attr(tectonic_uplift_m,'source') <- 'no correction'
    if(as.numeric(verbose) > 1){message('no correction grid specified')}
    } else {
    if( length(names(curve)) != length(names(correction)) ){ # only run if correction is not yet formatted to curve 
      if(class(correction)[1] %in% c('SpatRaster','numeric')){
        uplift <- correction
        #attr(uplift)
      } else {
        uplift <- terra::rast(correction)
        }
      if(as.numeric(verbose) > 1){message('import correction grid')}
    
    # # if topo is null 
    # if(is.null(topo)){ # open - check if raster is loaded locally
    #   #download(path=path,name='topo',autoupdate=autoupdate, verbose=verbose)
    #   topopath <- list.files(pattern=options()$tabs.datasetDatatype$topo,paste0(options()$tabs.datasetPath,'/topo'),full.names =TRUE)
    #   topo <- terra::rast(topopath) # THIS NEEDS TO BE CORRECTED - ONLY VALID FOR GEBCO! NOT FOR OTHER NC FILES
    #   if(as.numeric(verbose) > 1){message(paste0('Raster ', basename(topopath),' imported from filepath.'))}    
    # } else {
    #   if(class(topo)[1] == 'SpatRaster'){
    #     topo <- topo #terra::rast(topo) # THIS NEEDS TO BE CORRECTED - ONLY VALID FOR GEBCO! NOT FOR OTHER NC FILES
    #   }
    #   if(class(topo)[1] %in% c('sf','character')){
    #     topo <- terra::rast(topo)
    #   }
    #   if(as.numeric(verbose) > 1){message(paste0('Raster already loaded.'))}
    # }
    if(is.null(topo) & inherits(uplift,'SpatRaster')){
      stop("topo model is not defined, define topo model at the extent by running 
           data <- get_data(topo='<topo model>')
           topo <- data$topo
           ")
    }
    
    # convert
    if(inherits(uplift,'SpatRaster')){
    if(crs(topo) != crs(uplift)) uplift <- project(x=uplift, y=topo); 
    if(as.numeric(verbose) > 1){message('reproject correction grid to projection system of topo (default: WGS84)')}
    
    uplift <- crop(terra::resample(uplift,topo,method='bilinear'),topo)
    if(as.numeric(verbose) > 1){message('resample uplift grid to resolution of topo')}
    }
    # conversion table for units 
    mt_unit <- expand.grid(metric_unit=c('mm','cm','dm','m','dam','hm','km'), 
                           time_unit=c('yr','Kyr','Myr')
    )
    mt_unit$metric_time_unit <- paste(mt_unit$metric_unit,mt_unit$time_unit, sep='/')
    mt_conv <- expand.grid(metric_conv=c(1,10,100,1000,10000,100000,1000000), 
                           time_conv=c(1,1000,1000000))
    mt <- cbind(mt_unit,mt_conv)
    if(as.numeric(verbose) > 1){message('implemented units: ',paste(mt$metric_time_unit, collapse=' '))}
    
    from_unit <- mt[which(mt$metric_time_unit==units$correction),]
    curve_unit <- paste0(units$curve[2],'/',units$curve[1])
    to_unit <- mt[which(mt$metric_time_unit==curve_unit),]
    
    # conversion factor for correct linear interpolation to m per yr
    fac <- (to_unit$metric_conv / from_unit$metric_conv) / (to_unit$time_conv / from_unit$time_conv)
    if(as.numeric(verbose) > 1){message('factor to convert from ', units$correction,' (correction-grid-unit) to ', curve_unit,' (curve-units) = ',fac)}
    tectonic_uplift_m <- lapply(abs(as.numeric(names(curve))),function(x) { x * uplift / fac}) # TODO double check whether the conversion is correct
    if(class(tectonic_uplift_m[[1]])[1]=='SpatRaster'){
      tectonic_uplift_m <- rast(tectonic_uplift_m)
      
      # add source 
      sample <- sporades()$correction
      if(all.equal(correction,sample)[1] == TRUE){
        attr(tectonic_uplift_m,'source') <- 'correction sporades (sample)'
        }
      
      if(is.null(attr(correction,'source'))){
      attr(tectonic_uplift_m,'source') <- 'custom correction raster'
      } 
        
    } else {
      tectonic_uplift_m <- unlist(tectonic_uplift_m)
      if(is.null(attr(correction,'source'))){
      attr(tectonic_uplift_m,'source') <- 'custom correction vector'
      }
    }
    
    names(tectonic_uplift_m) <- names(curve)
    if(as.numeric(verbose) > 1){message('calculate uplift or subsidence per time period')}
    } else {
      
      if(length(curve) == 1 & as.numeric(names(curve)[1]) == 0){
        #tectonic_uplift_m <- list(0)
        tectonic_uplift_m <- 0
        names(tectonic_uplift_m) <- 0
        attr(tectonic_uplift_m,'source') <- 'no correction'
        if(as.numeric(verbose) > 1){message('no corrections performed because the curve only includes the present')}
      }
      
      if(as.numeric(verbose) > 1){message('correction already calculated')}
      tectonic_uplift_m <- correction
      
      if(class(tectonic_uplift_m)[1] %in% c('SpatRaster')){
        if(is.null(attr(tectonic_uplift_m,'source'))){
          attr(tectonic_uplift_m,'source') <- 'custom correction raster'
        } 
      } else {
        if(is.null(attr(tectonic_uplift_m,'source'))){
          attr(tectonic_uplift_m,'source') <- 'custom correction vector'
        }
      }
      #terra::ext(tectonic_uplift_m) <- terra::ext(topo)
    }
      
    }
  
  # ensure that the labels of the correction are the same as the curve 
  # names(tectonic_uplift_m) <- ifelse(grepl('-',names(tectonic_uplift_m)),
  #                        paste0('BP',sprintf("%07d", abs(as.numeric(names(tectonic_uplift_m))))),
  #                        paste0('AP',sprintf("%07d", abs(as.numeric(names(tectonic_uplift_m))))))
  
  # if(is.null(curve_data$curve_name)){
  #   if(grepl('.csv',curve[1])){
  #     attr(sea_level_m,'source') <- gsub('.csv','',basename(curve))
  #   } else {
  #     warning("no column named curve_name, we cannot derive the source name of the curve, please add 'curve_name' column, or the default 'custom' is used")
  #     attr(sea_level_m,'source') <- 'custom'
  #   }
  # } else {
  #   attr(sea_level_m,'source') <- curve_data$curve_name[1]
  # }
  
  # just in case if still NULL
  if(class(tectonic_uplift_m)[1] %in% c('SpatRaster')){
    if(is.null(attr(tectonic_uplift_m,'source'))){
      attr(tectonic_uplift_m,'source') <- 'custom correction raster'
    } 
  } else {
    if(is.null(attr(tectonic_uplift_m,'source'))){
    attr(tectonic_uplift_m,'source') <- 'custom correction vector'
    }
  }
  
  return(tectonic_uplift_m)
}





