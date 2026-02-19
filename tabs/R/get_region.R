#' @importFrom terra ext vect merge aggregate 
#' @importFrom sf st_read st_as_text st_geometry st_as_sf read_sf st_crop st_bbox st_write st_cast 
#' @importFrom dplyr summarize 
#' @importFrom leaflet leafletOutput renderLeaflet addTiles setView layersControlOptions addLayersControl
#' @importFrom utils data globalVariables
#' @importFrom stringi stri_trans_general
#' 
#'
NULL


#' @title check_type_region 
#' 
#' @author Johannes De Groeve
#' 
#' @description check the type and default dataset of region (islands, mountains) 
#'
#' @param region labeling dataset
#' 
#' @return a list including used region type and dataset
#' 
#' @noRd
#' @keywords internal
#' 
#'
check_type_region <- function(region){
  # load regions
  data("regions", package = "tabs", envir = environment())
  if_character <- grepl('character',class(region)[1])
  if(if_character) if_character_is_file <- file.exists(region[1]) else if_character_is_file <- FALSE
  if(if_character)
  {# priority order of region types for overlapping region names (archipelago, country, island, mountain, plates)
    if(!if_character_is_file[1]){
      which_type <- do.call(rbind.data.frame,lapply(region,function(x) # USE ASCII SO ALWAYS CONVERT TO ASCII FIRST 
        regions[grepl(paste0('^',
                             gsub("([][{}()*+?.\\^$|])", 
                                  "\\\\\\1", 
                                  stringi::stri_trans_general(str = x, id = "Latin-ASCII")),
                             '$'), 
                      regions$name_ascii,ignore.case =TRUE),] %>% 
          arrange(dataset,region) %>% 
          dplyr::slice(1))) %>% 
        dplyr::group_by(dataset,region) %>% 
        dplyr::mutate(n=n()) %>% 
        dplyr::arrange(dplyr::desc(n))
      type <- which_type$region
      dataset <- which_type$dataset
    } else {
      type <- 'Custom'
      dataset <- 'Custom'
    }
  } else {
    type <- 'Custom'
    dataset <- 'Custom'
  }
  return(list(type=type,dataset=dataset))
}

#' @title get_ext_island 
#' 
#' @author Johannes De Groeve
#' 
#' @description extent of island 
#'
#' @param region island name 
#' @param buffer buffer distance 
#' 
#' @return extent object
#' 
#' @noRd
#' @keywords internal
#' 
#'
get_ext_island <- function(region, buffer=NULL){ # search in island dataset
  y <- list.files(pattern=options()$tabs.datasetDatatype$labs,paste0(options()$tabs.datasetPath,'/labs'),full.names =TRUE) #[1]
  data("regions", package = "tabs", envir = environment())
  
  region_l<- lapply(region, function(x) 
    regions[grepl(paste0('^',stringi::stri_trans_general(str = x, id = "Latin-ASCII"),'$'),regions$name_ascii,ignore.case =TRUE),'name']
  )
  region <- unlist(region_l)
  s<- terra::vect(y[grepl('reference',y)],query=paste0("SELECT unique_id,name,area_km2,coastline_km, plate_id,land_type_id, geom FROM islands 
                                       WHERE name is not NULL AND lower(name) in ('",paste0(c(tolower(region)), collapse="','"),"') ORDER BY unique_id"))
  if(is.null(buffer)) buffer <- 10000 # default buffer 
  e <- extent_outputs(p=terra::buffer(terra::as.polygons(terra::ext(s),crs=terra::crs(s)),
                                      width=buffer))
  return(e)
} # search in island dataset


#' @title get_ext_plate
#' 
#' @author Johannes De Groeve
#' 
#' @description extent of plate 
#'
#' @param region plate name 
#' @param buffer buffer distance 
#' 
#' @return extent object
#' 
#' @noRd 
#' @keywords internal
#' 
#'
get_ext_plate <- function(region,buffer=NULL){ # search in plate dataset
  y <- list.files(pattern=options()$tabs.datasetDatatype$labs,paste0(options()$tabs.datasetPath,'/labs'),full.names =TRUE) #[1]
  
  s <- terra::vect(y[grepl('reference',y)],query=paste0("SELECT plate_name plate, plate_id, geom FROM plates WHERE lower(plate_name) in ('",gsub(' ','',gsub('plate','',paste0(c(tolower(region)), collapse="','"))),"')"))
  if(is.null(buffer)) buffer <- 10000 # default buffer 
  e <- extent_outputs(p=terra::buffer(terra::as.polygons(terra::ext(s),crs=terra::crs(s)),
                                      width=buffer))
  return(e)
} # search in plate dataset


#' @title get_ext_country
#' 
#' @author Johannes De Groeve
#' 
#' @description extent of country 
#'
#' @param region country name 
#' @param buffer buffer distance 
#' 
#' 
#' @return extent object
#' 
#' @noRd 
#' @keywords internal
#' 
#' 
get_ext_country <- function(region,buffer=NULL){ # search in country dataset
  countries <- terra::unwrap(qs2::qs_read(system.file("extdata", "countries.qs2", package = "tabs")))
  s <- countries[grepl(region,countries$geounit,ignore.case =TRUE)]
  #s <- terra::vect(rnaturalearth::ne_countries(country = region, scale = "large"))
  
  if(is.null(buffer)) buffer <- 10000
  e <- extent_outputs(p=terra::buffer(terra::as.polygons(terra::ext(s),crs=terra::crs(s)),
                                      width=buffer))
  
  return(e)
} # search in country dataset


#' @title get_ext_archipelago
#' 
#' @author Johannes De Groeve
#' 
#' @description extent of archipelago
#'
#' @param region archipelago name 
#' @param buffer buffer distance 
#' 
#' @return extent object
#' 
#' @noRd 
#' @keywords internal
#' 
#'
get_ext_archipelago <- function(region,buffer=NULL){ # search in archipelago dataset
  y <- list.files(pattern=options()$tabs.datasetDatatype$labs,paste0(options()$tabs.datasetPath,'/labs'),full.names =TRUE) #[1]
  
  s <- terra::vect(y[grepl('reference',y)],query=paste0("SELECT archip, geom FROM archipelago WHERE lower(archip) in ('",paste0(c(tolower(region)), collapse="','"),"')"))
  
  if(is.null(buffer)) buffer <- 10000
  e <- extent_outputs(p=terra::buffer(terra::as.polygons(terra::ext(s),crs=terra::crs(s)),
                                      width=buffer))
  return(e)
} # search in archipelago dataset


#' @title get_ext_mountains
#' 
#' @author Johannes De Groeve
#' 
#' @description extent of mountains 
#'
#' @param region mountain name 
#' @param buffer buffer distance 
#' 
#' 
#' @return extent object
#' 
#' @noRd 
#' @keywords internal
#' 
#'
get_ext_mountains <- function(region,buffer=NULL){ # search in mountain dataset
  y <- list.files(pattern=options()$tabs.datasetDatatype$labs,paste0(options()$tabs.datasetPath,'/labs'),full.names =TRUE) #[1]
  data("regions", package = "tabs", envir = environment())
  
  region_l <- lapply(region, function(x) # SLOW
    regions[grepl(paste0('^', gsub("([][{}()*+?.\\^$|])", 
                                   "\\\\\\1", 
                                   stringi::stri_trans_general(str = x, id = "Latin-ASCII")
                                   ),
                         '$'),regions$name_ascii,ignore.case =TRUE),'name'][1]
  )
  region <- unlist(region_l)
  
  s <- terra::vect(y[grepl('mountains',y)],query=paste0("SELECT hier_lvl, feature, dbasename, mapunit, geom 
                                       FROM mountains_standard 
                                       WHERE lower(dbasename) in ('",paste0(c(tolower(region)), collapse="','"),"')")) # mapunit='Aggregated' AND
  
  if(is.null(buffer)) buffer <- 10000
  e <- extent_outputs(p=terra::buffer(terra::as.polygons(terra::ext(s),crs=terra::crs(s)),
                                      width=buffer))
  
  return(e)
} # search in mountain dataset


#' @title get_ext_region
#' 
#' @author Johannes De Groeve
#' 
#' @description extent of region defined by archipelago, country, island, plate, mountain 
#'
#' @param region region name defined by archipelago, country, island, plate, mountain 
#' @param buffer buffer distance 
#' 
#' 
#' @return extent object
#' 
#' @noRd 
#' @keywords internal
#' 
#'
get_ext_region <- function(region, buffer=NULL, verbose=FALSE){
  if_character <- grepl('character',class(region)[1])
  if(as.numeric(verbose) > 1){message('region is character ',if_character)}
  if(if_character) if_character_is_file <- file.exists(region) else if_character_is_file <- FALSE
  if(if_character){
    if(!if_character_is_file[1]){
      r <-check_type_region(region)
      type <- r$type
      if(grepl('Island',type,ignore.case=TRUE)[1]){
        # island 
        e <- get_ext_island(region=region,buffer=buffer)
      } else {
        if(grepl('Plate',type,ignore.case=TRUE)[1]){
          # plate
          e <- get_ext_plate(region=region,buffer=buffer)
        } else {
          if(grepl('Country',type,ignore.case=TRUE)[1]){
            # country
            e <- get_ext_country(region=region,buffer=buffer)
          } else {
            # archipelago
            if(grepl('Archipelago',type,ignore.case=TRUE)[1] & !grepl('iceland',region[1],ignore.case=TRUE) ){
              e <- get_ext_archipelago(region=region,buffer=buffer)
            } else {
              # mountain
              e <- get_ext_mountains(region=region,buffer=buffer)
            }
          }
        }
      }
    }
  }
  return(e)
}

#' @title get_ext
#' 
#' @author Johannes De Groeve
#' 
#' @description extent defined by extent coordinates 
#'
#' @param region region defined by extent coordinates
#' @param buffer buffer distance 
#' 
#' 
#' @return extent object
#' 
#' @noRd 
#' @keywords internal
#' 
#'
get_ext <- function(region=NULL,buffer=buffer,verbose=FALSE){ # from extent coordinates and if no coordinates are given open a pop-up
  is_terra_spatextent <- grepl('SpatExtent',class(region)[1])
  if(as.numeric(verbose) > 1){message('input terra extent ',is_terra_spatextent)}
  is_extent_numeric <- all(class(region) == 'numeric' & length(region) == 4)
  if(as.numeric(verbose) > 1){message('input numeric extent ',is_extent_numeric)}
  
  if(any(is_terra_spatextent | is_extent_numeric)){ # OPEN - EXTENT IS SPECIFIED BY COORDINATES
    s <- terra::as.polygons(terra::ext(region),crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") #e.g. 142,142.4,26.5,26.8]
    if(is.null(buffer)) buffer <- 0 # default buffer 
    e <- extent_outputs(p=terra::buffer(terra::as.polygons(terra::ext(s),crs=terra::crs(s)),
                                        width=buffer))    # e <- as(raster::extent(options()$tabs.current_extent_vect),'SpatialPolygons')
    # e.g. 128.718567,131.322327,29.118574,30.930501
  } # CLOSE - EXTENT IS SPECIFIED BY COORDINATES
  
  if(is.null(region)){ # & default # OPEN - EXTENT IS SPECIFIED BY GET_EXTENT
    # if xmin is NULL, check if y is the default
    if(isFALSE(options()$tabs.current_extent) | is.null(options()$tabs.current_extent)){
      get_extent(overwrite=TRUE)
      v <- options()$tabs.current_extent_vect
      p <- options()$tabs.current_extent_poly
      l <- options()$tabs.current_extent_leaf
      ex <- options()$tabs.current_extent
      attr(v,'pol') <- p
      attr(v,'ext') <- ex
      attr(v,'leaf') <- l
      e <- v
      } else {
      v <- options()$tabs.current_extent_vect
      p <- options()$tabs.current_extent_poly
      l <- options()$tabs.current_extent_leaf
      ex <- options()$tabs.current_extent
      attr(v,'pol') <- p
      attr(v,'ext') <- ex
      attr(v,'leaf') <- l
      e <- v
    } # !exists('e')
  }
  
  return(e)
} # from extent coordinates and if no coordinates are given open a pop-up

#' @title get_ext_custom
#' 
#' @author Johannes De Groeve
#' 
#' @description extent defined by custom polygon or file path
#'
#' @param region region defined by custom polygon or file path
#' @param buffer buffer distance 
#' 
#' @noRd 
#' @keywords internal
#' 
#'
get_ext_custom <- function(region,buffer=NULL,verbose=FALSE){ # use the extent of the custom provided polygon
  if_character <- grepl('character',class(region)[1])
  if(as.numeric(verbose) > 1){message('region is character ',if_character)}
  if(if_character) if_character_is_file <- file.exists(region) else if_character_is_file <- FALSE
  if_terra_object <- grepl('SpatVector',class(region)[1])
  if(as.numeric(verbose) > 1){message('region is terra object ',if_terra_object)}
  if_sf_object <- grepl('sf',class(region)[1])
  if(as.numeric(verbose) > 1){message('region is sf object ',if_sf_object)}
  
  # If it concerns an already imported sf/vect object or a file name you can simply run 
  if(any(if_terra_object | if_sf_object | if_character_is_file[1])){
    if(if_terra_object){region <- sf::st_as_sf(region)}
    s <- terra::vect(region)
    if(is.null(buffer)) buffer <- 10000 # default buffer 
    e <- extent_outputs(p=terra::buffer(terra::as.polygons(terra::ext(s),crs=terra::crs(s)),
                                        width=buffer))
  }
  return(e)
} # use the extent of the custom provided polygon


#' @title get_e
#' 
#' @author Johannes De Groeve
#' 
#' @description general function to select the extent of a region 
#'
#' @param region region definition 
#' @param buffer buffer distance 
#' 
#' 
#' @return extent object
#' 
#' @noRd 
#' @keywords internal
#' 
#'
get_e<-function(region=NULL,buffer=NULL,verbose=FALSE){
  # check if the region is numeric 
  is_terra_spatextent <- grepl('SpatExtent',class(region)[1])
  if(as.numeric(verbose) > 1){message('input terra extent ',is_terra_spatextent)}
  is_extent_numeric <- all(class(region) == 'numeric' & length(region) == 4)
  if(as.numeric(verbose) > 1){message('input numeric extent ',is_extent_numeric)}
  is_null <- is.null(region[1])
  
  if(any(is_null | is_extent_numeric | is_terra_spatextent)){
    e <- get_ext(region=region,buffer=buffer)
  }
  
  # check if the region is custom (does it have a file extension, is it sf or terra object)
  if_character <- grepl('character',class(region)[1])
  if(as.numeric(verbose) > 1){message('region is character ',if_character)}
  if(if_character) if_character_is_file <- file.exists(region) else if_character_is_file <- FALSE
  if_terra_object <- grepl('SpatVector',class(region)[1])
  if(as.numeric(verbose) > 1){message('region is terra object ',if_terra_object)}
  if_sf_object <- grepl('sf',class(region)[1])
  if(as.numeric(verbose) > 1){message('region is sf object ',if_sf_object)}
  
  # If it concerns an already imported sf/vect object or a file name you can simply run 
  if(any(if_terra_object | if_sf_object | if_character_is_file[1])){
    e <- get_ext_custom(region=region,buffer=buffer)
  }
  # check if the region character without file extension. If yes, check what kind of region definition it is
  # check the list of regions and what the name is (island, plate, country, archipelago, mountain)
  
  if(if_character[1]){
    if(!if_character_is_file[1]){
    e <- get_ext_region(region=region,buffer=buffer) 
    }
  }
  # store whether the extent is defined by mountain or by island 
  return(e)
}

#' @title get_layer_isls
#' 
#' @author Johannes De Groeve
#' 
#' @description obtain islands layer 
#'
#' @param region region definition 
#' @param buffer buffer distance 
#' 
#' 
#' @return SpatVector
#' 
#' 
#' @noRd 
#' @keywords internal
#' 
#'
get_layer_isls <- function(region,buffer=NULL){
  y <- list.files(pattern=options()$tabs.datasetDatatype$labs,paste0(options()$tabs.datasetPath,'/labs'),full.names =TRUE) #[1]
  
  E <- get_e(region=region,buffer=buffer)
  QUERY <- paste0("SELECT unique_id,name,area_km2,coastline_km, plate_id,land_type_id, geom FROM islands")
  s <- terra::vect(y[grepl('reference',y)],extent=terra::ext(E), query=QUERY)
  if(dim(s)[1] == 0) { "No islands found for region"} else{
    # add the plates information 
    plates <- terra::vect(y[grepl('reference',y)],extent=terra::ext(E), query=paste0('SELECT plate_name plate, plate_id, geom FROM plates'))
    # load continent 
    s <- terra::merge(s,plates,by='plate_id',all.x=TRUE)[,c('unique_id','name','area_km2','coastline_km','land_type_id','plate')]
  }
  
  continents_metadata_id <- terra::vect(y[grepl('reference',y)],extent=terra::ext(E), query=paste0('SELECT continents_metadata_id unique_id, geom FROM continents'))
  if(dim(continents_metadata_id)[1] != 0){
    continents_metadata_id <- terra::aggregate(continents_metadata_id,'unique_id')
    continents_metadata <- terra::vect(y[grepl('reference',y)],query=paste0('SELECT continents_metadata_id unique_id,name,area_km2,coastline_km, land_type_id, NULL plate FROM continents_metadata'),what='attributes')
    continents <- terra::merge(continents_metadata_id,continents_metadata,'unique_id')
    continents$agg_n <- NULL
    s <- rbind(s,continents)
    
  }
  
  s <- terra::crop(s,E)
  
  return(s)
}


#' @title get_layer_mnts
#' 
#' @author Johannes De Groeve
#' 
#' @description obtain mountain layer 
#'
#' @param region region definition 
#' @param buffer buffer distance 
#' 
#' 
#' @return SpatVector
#' 
#' 
#' @noRd 
#' @keywords internal
#' 
#'
get_layer_mnts <- function(region,buffer=NULL){
  y <- list.files(pattern=options()$tabs.datasetDatatype$labs,paste0(options()$tabs.datasetPath,'/labs'),full.names =TRUE) #[1]
  
  QUERY <- paste0("SELECT hier_lvl level,feature regiontype, gid, 
                           dbasename name, feature, area, perimeter, elev_low, elev_high, geom 
                        FROM mountains_standard 
                         WHERE mapunit='Basic'")
  
  E <- get_e(region=region,buffer=buffer)
  s<- terra::vect(y[grepl('mountains',y)],extent=attr(E,'ext'), query=QUERY)
  
  s <- terra::crop(s,E)
  
  return(s)
}

#' @title get_layer_feat
#' 
#' @author Johannes De Groeve
#' 
#' @description obtain feature layer 
#'
#' @param region region definition 
#' @param buffer buffer distance 
#' 
#' 
#' @return SpatVector
#' 
#' 
#' 
#' @noRd 
#' @keywords internal
#' 
#'
get_layer_feat <- function(region,buffer=NULL,reclabs){
  #utils::globalVariables(c("features"))
  data("features", package = "tabs", envir = environment())
  feature_list <- features$feature
  
  y <- list.files(pattern=options()$tabs.datasetDatatype$labs,paste0(options()$tabs.datasetPath,'/labs'),full.names =TRUE) #[1]
  feature_codes <- suppressWarnings(terra::vect(y[grepl('reference',y)], query='SELECT * FROM lu_feature_code;', what='attributes'))
  feature_code_description <- match.arg(reclabs, choices=feature_codes$feature_code_description_short,several.ok=TRUE)
  feature_code_id <- feature_codes[which(feature_codes$feature_code_description_short %in% feature_code_description),]$feature_code_id
  
  QUERY <- paste0("SELECT unique_id,geonameid,name,plate_id,geom FROM reference_points WHERE feature_code_id in (",paste0(feature_code_id,collapse=","),")")
  
  E <- get_e(region=region,buffer=buffer)
  s<- terra::vect(y[grepl('reference',y)],extent=attr(E,'ext'), query=QUERY)
  
  s <- terra::crop(s,E)
  
  return(s)
}

#' @title get_layer_custom
#' 
#' @author Johannes De Groeve
#' 
#' @description obtain custom layer 
#'
#' @param region region definition 
#' @param buffer buffer distance 
#' 
#' @return SpatVector
#' 
#' 
#' 
#' @noRd 
#' @keywords internal
#' 
#'
get_layer_custom <- function(region,buffer=NULL){
  E <- get_e(region=region,buffer=buffer)
  if(grepl('SpatVector',class(region)[1])){
    s <- terra::crop(region,attr(E,'ext'))
    #terra::ext(s) <- E
  } else {
    s<- terra::vect(region,extent=attr(E,'ext'))
  }

  s <- terra::crop(s,E)
  
  return(s)
}

#' @title get_layer_false
#' 
#' @author Johannes De Groeve
#' 
#' @description obtain template layer (when no labeling is performed)
#'
#' @param region region definition 
#' @param buffer buffer distance 
#' 
#' 
#' @return SpatVector
#' 
#' 
#' @noRd 
#' @keywords internal
#' 
#'
get_layer_false <- function(region,buffer=NULL, aggregate=FALSE){
  E <- get_e(region=region,buffer=buffer)
  #if(aggregate){
  #  s <- aggregate(terra::as.points(terra::ext(E)))
    #s <- terra::as.polygons(terra::ext(E))
  #} else {
  s <- terra::as.points(terra::ext(E))
  #}
  if(nchar(terra::crs(s))==0){
  terra::crs(s) <- terra::crs(terra::vect(attr(E,'pol')))
  }
  s$unique_id <- 1:length(s)
  s$name <- 'S'
  
  s <- terra::crop(s,E)
  
  return(s)
}


#' @title add_attributes
#' 
#' @author Johannes De Groeve
#' 
#' @description add attributes 
#'
#' @param s object to add attributes to  
#' @param layer layer
#' @param reclabs reclabs argument 
#' @param aggregate boolean 
#' 
#' 
#' @return SpatVector with attributes
#' 
#' 
#' @noRd 
#' @keywords internal
#' 
#'
add_attributes <- function(s, layer, reclabs, aggregate){
  attr(s,'reclabs') <- reclabs
  attr(s,'aggregate') <- aggregate
  attr(s,'layer') <- layer
  return(s)
}

#' @title copy_attributes
#' 
#' @author Johannes De Groeve
#' 
#' @description copy attributes from object
#'
#' @param s object with attributes
#' 
#' 
#' @return list of attributes
#' 
#' 
#' @noRd 
#' @keywords internal
#' 
#'
copy_attributes <- function(s){
  copy <- list(reclabs=attr(s,'reclabs'),
               aggregate=attr(s,'aggregate'),
               layer=attr(s,'layer')) 
  return(copy)
}

#' @title paste_attributes
#' 
#' @author Johannes De Groeve
#' 
#' @description paste attributes
#'
#' @param s object with missing attributes
#' @param copy attributes returned by copy_attributes
#' 
#' 
#' @return SpatVector
#'
#' @noRd 
#' @keywords internal
#' 
#'
paste_attributes <- function(s,copy){
  attribute_list <- names(copy)
  for(x in attribute_list){
    attr(s, x) <- as.vector(unlist(copy[x]))
  }
  return(s)
}

#' # if you want all labels to be automatically generated use reclabs=FALSE


#' @title get_layer
#' 
#' @author Johannes De Groeve
#' 
#' @description obtain layer 
#'
#' @param region extent object (raster, terra, vector); polygon object (sf, terra); region names (islands, countries, plates, archipelagoes, mountains), if NULL a leaflet map opens to draw an extent.
#' @param buffer a buffer to apply defining the region. For islands, countries, plates and polygons a default buffer of 10000 m is used, which can be modified. To remove the default buffer, set buffer to 0. For extent definitions no buffer is defined
#' @param reclabs a column name that is used to name biogeographical shapes. Is NULL if the default dataset is used. 
#' @param aggregate boolean, aggregate the input labeling dataset, default is FALSE
#' @param verbose boolean, print messages if true
#' 
#' 
#' @return SpatVector
#' 
#' @noRd 
#' @keywords internal
#' 
#'
get_layer <- function(region=NULL,reclabs=NULL,buffer=NULL,verbose=FALSE,aggregate=FALSE){
  
  # NULL = automated label generation 
  if(isFALSE(reclabs)){
    s <- get_layer_false(region=region,buffer=buffer,aggregate=aggregate)
    s <- add_attributes(s,layer=FALSE, reclabs=reclabs, aggregate=aggregate)
    
  } else {
    #if reclabs is NULL it would still be good to automatically assign island names to islands, archipelago etc, and if mountains as mountains
    data("features", package = "tabs", envir = environment())
    feature_list <- features$feature
    
    
    is_terra_spatextent <- grepl('SpatExtent',class(region)[1])
    if(as.numeric(verbose) > 1){message('input terra extent ',is_terra_spatextent)}
    is_extent_numeric <- all(class(region) == 'numeric' & length(region) == 4)
    if(as.numeric(verbose) > 1){message('input numeric extent ',is_extent_numeric)}
    is_null <- is.null(region[1])
    
    
    if(is.null(region[1]) && is.null(reclabs[1])){
      s<- get_layer_false(region=region,buffer=buffer,aggregate=aggregate)
      s <- add_attributes(s, layer=FALSE,reclabs=reclabs, aggregate=aggregate)
      
    } else {
      
      if(any(is_null | is_extent_numeric | is_terra_spatextent)  && is.null(reclabs[1])){
        s<- get_layer_false(region=region,buffer=buffer,aggregate=aggregate)
        s <- add_attributes(s, layer=FALSE,reclabs=reclabs, aggregate=aggregate)
        
      } else{
        
        # default use isls when the labels from island dataset is used 
        BOOL_ISLS <- !is.null(region[1]) && check_type_region(region)$dataset[1] == 'Islands' && is.null(reclabs[1]) || (!is.null(reclabs[1]) && reclabs[1] == 'isls') 
        # default use mnts when the labels from mountain dataset is used 
        BOOL_MNTS <- !is.null(region[1]) && check_type_region(region)$dataset[1] == 'Mountains' && is.null(reclabs[1]) || (!is.null(reclabs[1]) && reclabs[1] == 'mnts')
        # default use of custom if custom polygon is used
        # BOOL_CUSTOM <- !is.null(region[1]) && check_type_region(region)$dataset[1] == 'Custom' && is.null(reclabs[1]) || (!is.null(reclabs[1]) && reclabs[1] == 'mnts')
        if (BOOL_ISLS) { 
          # get the island data
          s <- get_layer_isls(region=region,buffer=buffer)
          s <- add_attributes(s, layer='isls',reclabs='isls', aggregate=aggregate)
          
        } else if (BOOL_MNTS) {
          # get mountain data
          s <- get_layer_mnts(region=region,buffer=buffer)
          s <- add_attributes(s, layer='mnts',reclabs='mnts', aggregate=aggregate)
          
        } else if (if(!is.null(reclabs)){reclabs[1] %in% feature_list} else {FALSE}) { #!is.null(region[1]) && 
          # get features from feature dataset
          s <- get_layer_feat(region=region,buffer=buffer,reclabs=reclabs)
          s <- add_attributes(s, layer='geonames',reclabs=reclabs, aggregate=aggregate)
          
        } else {
          # use the spatial objects that was provided 
          s <- get_layer_custom(region=region,buffer=buffer) # column name 
          s <- add_attributes(s, layer='custom',reclabs=reclabs, aggregate=aggregate)
          if(class(s)[1] == 'SpatVector' & nrow(s)==1 & ncol(s)==0){
            # add a unique_id and name if the custom layer is just a polygon without information
            s$unique_id <- 1
            s$name <- 'S'
          } 
        }
      }
    }
  }
  
  
  #s <- terra::crop(s,e)
  
  if(aggregate){ # if FALSE, aggregate polygons 
    #if(attr(s, 'layer') != 'custom' & attr(s, 'aggregate')==TRUE){
    if(as.numeric(verbose) == 1) message('aggregate shapes, avoid labeling')
    s <- aggregate(s)
    s$unique_id <- 1
    s$name <- 'S'
    #}
  }
  
  return(s)
}




#' @title Get and prepare region 
#'
#' @description Retrieve region of interest
#'
#' @param region SpatVector. Region selection object defined by extent coordinates, a polygon object or path to dataset, an island, archipelago, country, mountain or plate name from the regions-list. If region is not defined a selection window will pop-up to define the area of interest.
#' @param buffer numeric. Draws a buffer around the selected region. For extent, the buffer is 0, otherwise 10000 m.
#' @param reclabs character. Dataset or column used for labeling biogeographic shapes. By default the island labeling dataset is used, while if reclabs is set to ‘mnts’ the mountain labeling is used. Otherwise another column from the region object could be used, or a feature from the geonames feature list (e.g., ‘peaks’, ‘peak’) could be specified. Note that any overlapping name from the list geonames features cannot be used. If so, it is recommended to rename your labeling column. Note that in case of a user-defined reclabs column, the concerned column will be replicated in the labs-object under the column name ‘name’.
#' @param aggregate boolean. Whether to aggregate biogeographic shapes.
#' @param verbose boolean. FALSE: No messages are printed. TRUE: Standard verbose mode. 2: Very verbose mode, displaying detailed information. 
#' @param overwrite boolean. TRUE: overwrite region and select a new extent via selection window will pop-up.
#'
#' @return A SpatVector object with the labeling polygons for the specified extent coming from spatial object, extent or region name
#' 
#' @seealso \href{https://uva_ibed_piac.gitlab.io/tabs/articles/Ab-tabs-region.html}{region}
#' 
#' 
#' 
#' @export
#'
#' @examples
#'
#' if(interactive()){
#' # interactive selection
#' r <- get_region()
#' # overwrite interactive selection
#' r <- get_region(overwrite=TRUE)
#' }
#' 
#' # sample dataset
#' labs <- sporades()$labs
#' # labels based on "name" column
#' r <- get_region(region=labs)
#' # labels based on specific column
#' r <- get_region(region=labs[,'plate'],reclabs='plate')
#' # automatic labeling 
#' r <- get_region(region=labs, reclabs = FALSE)
#' # aggregate shapes
#' r <- get_region(region=labs, aggregate=TRUE)
#' # define by extent 
#' library(terra)
#' e <- terra::ext(labs)
#' r <- get_region(region=e)
#' e <- as.vector(e)
#' r <- get_region(region=e)
#' # add buffer
#' r <- get_region(region=e,buffer=10000)
#' r <- get_region(region=e,buffer=100000)
#' 
#' 
#'
#'
get_region <- function(region=NULL, buffer=NULL, reclabs=NULL, aggregate=FALSE, verbose=FALSE,overwrite=FALSE){
  # get the labels 
  if(overwrite){
    s <- get_layer(region=get_extent(overwrite=TRUE),buffer=buffer,reclabs=reclabs,verbose=verbose, aggregate=aggregate)
  } else {
    s <- get_layer(region=region,buffer=buffer,reclabs=reclabs,verbose=verbose, aggregate=aggregate)
  }
  # s <- get_layer(region=region,buffer=buffer,reclabs=reclabs,verbose=verbose, aggregate=aggregate)
  s_attr <- copy_attributes(s)

  
  data("features", package = "tabs", envir = environment())
  feature_list <- features$feature
  
  # reclabs column
  if(is.null(reclabs[1])){
      #if(!is.null(attr(s,'reclabs'))){ 
        # if default datasets (isls, mnts, geonames) are used the labeling column is 'name'
        reclabs <- 'name'
      #} else { 
        # if a custom dataset is used, and reclabs is NULL, throw an error 
      #  stop('no column with a unique name, specify labeling column')
      # }

    } else {
    if(reclabs[1]=='mnts'|reclabs[1]=='isls'| reclabs[1] %in% feature_list){ # if null, default naming column is used
      reclabs <- 'name'
    } else { 
      # if(aggregate){ # if FALSE, aggregate polygons 
      #   message('aggregate polygons, avoid labeling')
      #   s <- aggregate(s)
      #   s$unique_id <- 1
      #   s$name <- 'extent'
      # }
    }
    }
  # check if the reclabs column does exist, otherwise throw error and stop 
  if(!any(grepl(paste0('^',reclabs[1],'$'),names(s))) & attr(s,'layer') == 'custom'){
    stop("reclabs column '", reclabs[1], "' does not exist. Specify existing column as reclabs argument.")
    }
  
  # check which columns have unique values and are integer, use those to generate a unique_id column 
  is_unique <- sapply(1:ncol(s),function(x) {nrow(unique(as.data.frame(s[,x]))) == nrow(as.data.frame(s[,x]))})
  is_character <- sapply(1:ncol(s),function(x) is.character(as.vector(unlist(as.data.frame(s[,x])))))
  is_integer <- sapply(1:ncol(s),function(x) is.integer(as.vector(unlist(as.data.frame(s[,x])))))
  
  if(any(is_unique & is_integer)){ # if integer and unique we assume it is good to use as a unique identifier
    s$unique_id <- as.vector(unlist(as.data.frame(s[,which(is_unique & is_integer)[1]])))
    if(as.numeric(verbose) > 1) message('existing column (',colnames(as.data.frame(s[,which(is_unique & is_integer)[1]])), ') found that is integer and unique across polygons:\nused to generate unique_id column')
  } else { # otherwise we create a unique_id column 
    s$unique_id <- 1:nrow(s) 
    if(as.numeric(verbose) > 1) message('no column found that is integer and unique across polygons:\nnew unique_id column created')
  }
  s <- s[order(s$unique_id),] # order by unique_id
  s <- sf::st_as_sf(s) # create sf object
  if(as.numeric(verbose) > 1) message('reference dataset imported')
  
  if(nrow(s) != 0){
    # # specify the reclabs column 
    # if(default){reclabs<-'name'}
    # # check for names column existence - only in case the polygon names of default are used
    # if(any(grepl('^name$',colnames(s))) & is.null(reclabs) & !default) { reclabs <- 'name'} 
    
    if(nrow(s[is.na(s[,reclabs]),]) > 0){s[is.na(s[,reclabs]),reclabs] <- 'UNKNOWN'}
    if(!isFALSE(reclabs[1])) { 
      s$name <- as.data.frame(s)[,reclabs] 
    } # nrow(s)
    # This probably also works by setting reclabs as 'name' instead of if statement but to be sure I keep it for now as reclabs
    if(isFALSE(reclabs[1])){ # if no labeling is performed
      s$uniquename <- paste0(as.data.frame(s)[,'name'],sprintf("%06d", s$unique_id)) # nrow(s)
    } else {
      s$uniquename <- paste0(as.data.frame(s)[,reclabs],sprintf("%06d", s$unique_id)) # nrow(s)
    }
    #if(names)
    
  } else {
    s <- NA
    warning('no shapes found')
  }
  s <- terra::vect(s)
  
  s <- paste_attributes(s,s_attr)

  if(terra::crs(s,proj=TRUE) == '+proj=longlat +datum=WGS84 +no_defs')
    {
    terra::crs(s) <- 'EPSG:4326'
    }
  
  return(s)
}

# 
# reset <- function(){
#   get_extent(overwrite=T)
# }


# get_region <- function(region=NULL, buffer=NULL, reclabs=NULL, aggregate=FALSE, verbose=FALSE,overwrite=FALSE){
#   if(overwrite){
#     get_region(region=get_extent(overwrite=overwrite), buffer=buffer, reclabs=reclabs, aggregate=aggregate, verbose=verbose) 
#   }
# }
# 





















