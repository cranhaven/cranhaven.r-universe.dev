#' @importFrom sf st_as_text st_as_sf st_geometry 
#' @importFrom terra vect rast project aggregate crop vrt
#' 
NULL 
#' 
#' 
#'
#' @title Get and prepare input datasets (topo, labs, curve, correction)
#' 
#' @author Johannes De Groeve
#' @description load and prepare input datasets topo, labs, curve and correction 
#'
#' @param region SpatVector. Region selection object defined by extent coordinates, a polygon object or path to dataset, an island, archipelago, country, mountain or plate name from the regions-list. If region is not defined a selection window will pop-up to define the area of interest.
#' @param topo SpatRaster. Topographic/Bathymetric model as SpatRaster or path to dataset. The topo projection is the reference for further outputs. 
#' @param curve SpatRaster. Curve value, vector, grid or list of grids indicating the relative altitude of a biogeographic system per time period compared to the present. A typical example is a sea level curve indicating the relative sea level position above or below sea level compared to the present. 
#' @param correction SpatRaster. Correction value, vector, grid, or list of grids to account for spatial-(non-)explicit and temporal (non-)linear changes in the topography (e.g., uplift and subsidence rates, sedimentation and erosion ticknesses)
#' @param reclabs character. Dataset or column used for labeling biogeographic shapes. By default the island labeling dataset is used, while if reclabs is set to ‘mnts’ the mountain labeling is used. Otherwise another column from the region object could be used, or a feature from the geonames feature list (e.g., ‘peaks’, ‘peak’) could be specified. Note that any overlapping name from the list geonames features cannot be used. If so, it is recommended to rename your labeling column. Note that in case of a user-defined reclabs column, the concerned column will be replicated in the labs-object under the column name ‘name’.
#' @param buffer numeric. Draws a buffer around the selected region. For extent, the buffer is 0, otherwise 10000 m.
#' @param aggregate boolean. Whether to aggregate biogeographic shapes.
#' @param units numeric. Units of topo, curve and correction provided as a list (default: units=list(topo='m', curve=c(names='yr', value='m'), correction='mm/yr'))
#' @param fact numeric. Spatial resolution factor at which the bathymetric model will be resampled 
#' @param verbose boolean. FALSE: No messages are printed. TRUE: Standard verbose mode, providing progress bar. 2: Very verbose mode, displaying detailed information. 
#' 
#' @return a list including topo, labs, curve and correction input datasets 
#'
#' @export
#'
#' @examples
#' 
#' # load data samples
#' sporades <- sporades()
#' topo <- sporades$topo
#' labs <- sporades$labs
#' correction <- sporades$correction
#' curve <- sporades$curve
#' 
#' # subset first and last period 
#' curve <- curve[[c(1,dim(curve)[3])]]
#' 
#' 
#' data <- get_data(topo=topo, 
#'                  region=labs, 
#'                  curve=curve)
#' data <- get_data(topo=topo, 
#'                  region=labs, 
#'                  curve=curve, 
#'                  correction=correction)
#' 
#' # run reconstruct using prepared input datasets                  
#' rec <- reconstruct(data)
#' 
#' 
get_data <- function(region=NULL,
                     topo=NULL,
                     aggregate=FALSE,
                     curve=NULL,
                     correction=NULL,
                     reclabs=NULL,
                     units=list(topo='m',curve=c(names='yr',value='m'),correction='mm/yr'),
                     buffer=NULL,
                     fact=0,
                     verbose=FALSE
                     ){ #y=NULL,xmin=NULL,xmax=NULL,ymin=NULL,ymax=NULL,island=NULL,plate=NULL
  
  if(as.numeric(verbose) == 1) message('1. prepare input topo labs curve correction')
  
  if(units$topo != as.vector(units$curve[2])){
    stop('units of topo and curve are not the same, both should be expressed in the same unit, e.g. meter')
    }
  
  # Capture unevaluated expression
  region_expr <- substitute(region)
  region_str <- deparse(region_expr)
  region_str <- gsub("overwrite = T)", "overwrite = TRUE)", region_str)

  # Evaluate if region is still an expression (i.e., if it's get_region())
  # Capture unevaluated function calls if provided directly in get_data()
  if (!is.null(region) && is.call(region)) {
    region <- substitute(region)
    #print(region)
  }
  
  # If `region` is still an expression (i.e., a function call), evaluate it now
  if (is.language(region_expr) && identical(region_str, "get_region(overwrite = TRUE)")) {
    overwrite <- TRUE #print('true')
  } else {
    overwrite <- FALSE #print('false')
  }
  
  islandsv <- get_region(region=region, buffer=buffer, reclabs=reclabs, aggregate=aggregate, overwrite=overwrite) # features=features
  labs_attr <- copy_attributes(islandsv)
  
  #islands <- get_islands(y=y,xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,island=island,plate=plate) #,autoupdate=autoupdate,path=path
  #islandsv <- terra::vect(islands)
  #islandsv <- terra::project(islandsv,"GEOGCRS[\"unknown\",\n    DATUM[\"unnamed\",\n        ELLIPSOID[\"Spheroid\",6378137,298.257223563,\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]]],\n    PRIMEM[\"Greenwich\",0,\n        ANGLEUNIT[\"degree\",0.0174532925199433,\n            ID[\"EPSG\",9122]]],\n    CS[ellipsoidal,2],\n        AXIS[\"latitude\",north,\n            ORDER[1],\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]],\n        AXIS[\"longitude\",east,\n            ORDER[2],\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]]]")
  if(as.numeric(verbose) > 1){message('retrieve labs from labeling dataset')}
  
  #projwgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    
  e <- terra::as.polygons(options()$tabs.current_extent,crs=crs(islandsv))
  
  #projarea <- sf::st_crs('+proj=moll +lon_0=0')
  #projdist <- sf::st_crs('+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
  
  if(is.null(topo)){ # open - check if raster is loaded locally
    #download(path=path,name='topo',autoupdate=autoupdate, verbose=verbose)
    topo <- list.files(paste0(options()$tabs.datasetPath,'/topo'),full.names =TRUE,recursive =TRUE) 
    bat <- suppressWarnings({terra::vrt(topo)}) # this is for the default
    if(as.numeric(verbose) > 1){message(paste0('Raster ', basename(topo),' imported from filepath.'))}    
  } else {
    if(class(topo)[1] == 'SpatRaster'){
      bat <- topo #terra::rast(topo) # THIS NEEDS TO BE CORRECTED - ONLY VALID FOR GEBCO! NOT FOR OTHER NC FILES
    }
    if(class(topo)[1] %in% c('sf','character')){ 
      if(!grepl('\\.[a-z].*',topo)){ 
        bat <-  suppressWarnings({terra::vrt(list.files(topo,recursive =TRUE,full.names =TRUE))})
        } else {
        bat <- terra::rast(topo)
        }
    }
    if(as.numeric(verbose) > 1){message(paste0('Raster already loaded.'))}
    
  } # close - check if raster is loaded locally
  
  # the projection is EPSG 4326
  if(terra::crs(bat,proj=TRUE) == '+proj=longlat +ellps=WGS84 +no_defs')
  {
    terra::crs(bat) <- 'EPSG:4326'
  }
  # crop bathymetry to extent and make sure that the labeling dataset and bathymetry have the same crs 
  bat <- terra::crop(bat, terra::project(e,bat))
  islandsv <- terra::project(islandsv,bat)
  
  #names(bat) <- 'elevation'
  if(as.numeric(verbose) > 1){message('raster cropped')}
  if(fact > 0){ # for all other factors load raster.gebco_2019
    bat <- terra::aggregate(bat, fact=fact)
  } 
  if(as.numeric(verbose) > 1){message('resolution set using a factor of ', fact)}
  
  names(bat) <- 'topo'
  
  #### LABS ####
  labs <- add_peaks(bat,vec=islandsv,reference=TRUE)
  labs <- paste_attributes(labs,labs_attr)
  # append additional columns to labs object
  default <- grepl('unique_id|name|uniquename|refx|refy|refz|refn',names(labs))
  labs <- labs[,c('unique_id','name','uniquename','refx','refy','refz','refn',names(labs)[!default])]
  
  #### CURVE ####
  if(is.null(curve)){ # present
    sea_level_m <- get_curve() # get curve imports the present condition 0 BP / 0 mBSL 
    if(as.numeric(verbose) > 1){message('get curve: present day sea level')}
  } else {
    if(inherits(curve,'character')){ # curve stored in database specified as character
      sea_level_m <- get_curve(curve)
      if(as.numeric(verbose) > 1){message(paste0('get curve from database or local: ',curve))}
    } else { # curve stored in package
      if(inherits(curve,'numeric')){
      sea_level_m <- get_curve(curve)
      } else {
        #if(inherits(curve,'SpatRaster')){
          if(all.equal(sporades()$curve,curve)[1] == TRUE){ # if sample is used
            sea_level_m <- get_curve(curve)
          } else {
          sea_level_m <- curve
        }
      }
      if(as.numeric(verbose) > 1){message(paste0('get curve'))}
    }
  }
  

  
  # resample the sea level curve to match resolution of bathymetry
  if( class(sea_level_m)[1] == "SpatRaster"){
    sea_level_m <- project(sea_level_m,bat)
    sea_level_m <- terra::resample(sea_level_m,bat,method='bilinear')
  }
  
  #### CORRECTION GRID UNIT ####
  tectonic_uplift_m <- get_correction(topo=bat, 
                                      curve=sea_level_m, 
                                      correction=correction,
                                      units = units,
                                      verbose = verbose
                                      )
  
  # resample the correction grid to match the resolution of the bathymetry 
  if(!is.null(nrow(tectonic_uplift_m))){
    if(nrow(tectonic_uplift_m) != nrow(bat) & length(tectonic_uplift_m[1]) > 1){
    tectonic_uplift_m <- terra::resample(tectonic_uplift_m,bat,method='bilinear')
    }
  }
  
  
  #### LIST DATASETS #### 
  dat <- list(bat, labs, sea_level_m, tectonic_uplift_m)
  names(dat) <- c('topo','labs','curve','correction')
  if(as.numeric(verbose) > 1) message('READY: ', paste0(names(dat),collapse=' '))
  
  return(dat)
}


