#' @importFrom terra xmin xmax ymin ymax sources
#' @importFrom jsonlite toJSON fromJSON
#' 
#' 
NULL 
#' 
#' @title metadata_file
#' 
#' @author Johannes De Groeve
#' @description create a json metadata file for the output of the function reconstruction
#'
#' @param topo bathymetric model object or path to dataset
#' @param labs reference dataset
#' @param curve sea curve object 
#' @param correction correction object or path to dataset
#' @param recvect reconstruction dataset
#'
#' @return a json file with metadata for output of function reconstruction 
#'
#' @noRd
#' @keywords internal
#'
#' 
#' 
metadata_file <- function(topo=NULL,curve=NULL,correction=NULL, recvect = NULL, labs = NULL, names=NULL){

  if(is.null(topo)){ # SOURCE OF BATHYMETRIC MODEL
    topo_source <- 'https://www.gebco.net/data_and_products/gridded_bathymetry_data/'
  } else {
    if(class(topo)[1] == 'SpatRaster'){topo_source <- sources(topo)}
    if(class(topo)[1] == 'character'){topo_source <- topo}
    topo_source <- tolower(basename(topo_source))
    
    if(grepl('gebco',tolower(topo_source))){
      if(grepl('2019',topo_source)){topo_source <- 'gebco 2019: https://www.gebco.net/data_and_products/gridded_bathymetry_data/gebco_2019/gebco_2019_info.html'}
      if(grepl('2020',topo_source)){topo_source <- 'gebco 2020: https://www.gebco.net/data_and_products/gridded_bathymetry_data/gebco_2020/'}
      if(grepl('2021',topo_source)){topo_source <- 'gebco 2021: https://www.gebco.net/data_and_products/gridded_bathymetry_data/gebco_2021/'}
      if(grepl('2022',topo_source)){topo_source <- 'gebco 2022: https://www.gebco.net/data_and_products/gridded_bathymetry_data/'}
    }
  } # SOURCE OF BATHYMETRIC MODEL
  
  
  # SOURCE OF THE LABS DATASET 
  if(attr(labs,'layer') == 'isls'){
    labs_source <- 'global shoreline vector (GSV): https://doi.org/10.1080/1755876X.2018.1529714'
  }
  if(attr(labs,'layer') == 'mnts'){
    labs_source <- 'global mountain biodiversity assessment (GMBA v2): https://doi.org/10.1038/s41597-022-01256-y'
  }
  if(attr(labs,'layer') == 'geonames'){
    labs_source <- 'geoNames: www.geonames.org'
  }
  if(attr(labs,'layer') == 'custom'){
    if(class(labs)[1] == 'SpatVector'){labs_source <- sources(labs)}
    if(class(labs)[1] == 'character'){labs_source <- labs}
  }
  if(attr(labs,'layer') == FALSE){
    labs_source <- 'no labels'
  }
  
  # if(is.null(region) | is.null(names)){ # SOURCE OF LABS DATASET 
  #   region_source <- 'global coastline vector: https://doi.org/10.1080/1755876X.2018.1529714'
  # } else {
  #   if(class(region)[1] == 'SpatVector'){region_source <- sources(region)}
  #   if(class(region)[1] == 'character'){region_source <- region}
  #   region_source <- tolower(basename(region_source))
  # } # SOURCE OF LABS DATASET 
  
  
  
  if(is.null(curve)){ # SOURCE OF CURVE 
    curve_source <- 'no curve'
      } else {
        if(class(curve)[1] == 'SpatRaster'){
        #  if(terra::sources(curve)[1]==""){
        #    curve_source <- attr(curve,'source')
        #  } else {
            curve_source <- sources(curve)[1]
        #  }
          }
        if(class(curve)[1] %in% c('integer','numeric')){
          curve_source <- attr(curve,'source') # get curve name from attribute
          if(is.null(curve_source)){ # if attribute is missing, check if there is a match with the existing curves in values and names 
            curve_len <- length(curve)
            curves_available <- list(tabs::lambeck,tabs::cutler,tabs::bintanja)
            names(curves_available) <- unlist(lapply(curves_available, function(x) attr(x,'source')))
            curves_available_len <- lapply(curves_available,function(x) length(which(x %in% curve & names(x) %in% names(curve))))
            curve_source <- paste(names(curves_available_len[curves_available_len == curve_len]),collapse='-')
            if(is.null(curve_source)){
              curve_source <- 'custom-unknown'
            }
          }
        }
        if(class(curve)[1] == 'character'){curve_source <- curve}
        curve_source <- basename(curve_source)
        
        # if the curve that is used is stored in the dataset path it means that Koene is used 
        curve_path <- options()$tabs.datasetPath
        if(grepl(paste0(curve_path,'/curve/RSL'),curve_source,ignore.case =TRUE) | curve_source == 'st_curve'){
          curve_retained <- options()$tabs.datasetCurves['st_curve']
          curve_source <- paste0(names(curve_retained),': ',curve_retained) 
          } else {
            curve_sources <- options()$tabs.datasetCurves 
            if(length(curve_sources[names(curve_sources) == curve_source]) > 0){ # only run this if one of the default curves is used
            curve_retained <- curve_sources[names(curve_sources) == curve_source]
            curve_source <- paste(paste0(names(curve_retained),': ',curve_retained),collapse=' - ')
            }
        }
    
  } # SOURCE OF CURVE 
  
  if(is.null(correction)){
    correction_source <- 'no correction'
  } else {
    if(class(correction)[1] == 'SpatRaster'){correction_source <- sources(correction)}
    if(class(correction)[1] == 'character'){correction_source <- correction}
    if(class(correction)[1] == 'numeric'){correction_source <- 'based on single value'}
    correction_source <- tolower(basename(correction_source))
  }
  
# RECONSTRUCTION METADATA 
meta_ext <- list(xmin=xmin(recvect[[1]]), 
                 xmax=xmax(recvect[[1]]), 
                 ymin=ymin(recvect[[1]]), 
                 ymax=ymax(recvect[[1]]))
meta_sources <- list(topo= topo_source, 
                     labs= labs_source,
                     curve= curve_source,
                     correction= correction_source)
meta_descriptions <- list(topo='raster of the topography and/or bathymetry usually expressed in meter below/above sea level', 
                          labs='vector layer used for labeling reconstructed biogeographical shapes',
                          curve='vector or (list of) raster(s) expressing the relative shift in altitude over time for the biogeographical system of interest (e.g. relative sea level, upper forest limit)',
                          correction='vector or (list of) raster(s) expressing vertical motion due to geotectonic and geophysical activities (e.g. uplift/subsidence, delta aggregation, volcanism)')

meta_names <- list(
  period=
    list(type=class(recvect[[1]]$period),
         description='character; lower bound of a time period expressed in years before/after present for a reconstruction at a specific curve value (e.g. sea level position)',
         unit='Years before/after present',
         source='tabs'),
  curve=
    list(class=class(recvect[[1]]$curve),
         description='numeric; curve value (e.g. sea level position) for that period; In case of a raster (e.g., st_curve; spatial-explicit curve) the average curve value is calculated within the region',
         unit='meter',
         source='tabs'),
  correction=
    list(class=class(recvect[[1]]$correction),
         description='numeric; correction value (e.g. uplift/subsidence) for that period; In case of a raster the average correction value is calculated within the region',
         unit='meter',
         source='tabs'),
  iso=
    list(type=class(recvect[[1]]$iso),
         description='numeric; meter above or below the curve value (e.g. sea level position) defining the lower bound of the biogeographic region',
         unit='number',
         source='tabs'),  
  unique_id=
    list(type=class(recvect[[1]]$unique_id),
         description='integer; unique identifier of a biogeographic unit or region for a time period',
         unit='number',
         source='tabs'),
  area=
    list(type=class(recvect[[1]]$area),
         description='numeric; size of a biogeographic unit/region in square meter',
         unit='square meter',
         source='tabs'),
  n=
    list(type=class(recvect[[1]]$n),
         description='integer; number of cells at the resolution of the topo within a biogeographic unit/region; Will change if the res_level parameter is modified',
         unit='number',
         source='tabs'),
  x=
    list(type=class(recvect[[1]]$x),
         description='numeric; x-coordinate in degrees (SRID=4326) of the highest point within a biogeographic unit/region extracted through intersection with topo; in case the highest point could not be extracted the centroid of a biogeographic unit/region is used',
         unit='degrees',
         source='tabs'),
  y=
    list(type=class(recvect[[1]]$y),
         description='numeric; y-coordinate in degrees (SRID=4326) of the highest point within a biogeographic unit/region extracted through intersection with topo; in case the highest point could not be extracted the centroid of a biogeographic unit/region is used',
         unit='degrees',
         source='tabs'),
  z=
    list(type=class(recvect[[1]]$z),
         description='numeric; meter above/below present sea level of the highest point within a biogeographic unit/region extracted through intersection with topo; in case the highest point could not be extracted the centroid of a biogeographic unit/region is used',
         unit='meter above sea level',
         source='tabs'),
  recid= 
    list(type=class(recvect[[1]]$recname),
         description='character; reconstructed id, when biogeographic units merge over time it is named after the biogeographic region with the highest point. The id is included cause it is unique across islands with identical names',
         unit='character',
         source='tabs'),    
  recname=
    list(type=class(recvect[[1]]$recname),
         description='character; reconstructed name, when biogeographic units merge over time it is named after the biogeographic region with the highest point',
          definitions=list('I-<PERIOD>-<ID>'='I=identified in topo but not in labeling dataset; or paleo island that remains disconnected from a present day existing island 
                            PERIOD=the most recent period the island emerged; 
                            ID=identifier',
                          UNKNOWN='the name of the island is unknown',
                          UNNAMED='unnamed islands by labeling dataset'),
         unit='character',
         source='tabs'),
  recnames=
    list(type=class(recvect[[1]]$recnames),
         description='json object including the name and id of intersecting reconstructed polygons (from t0 until ti, where t=time period) within a reconstructed polygon of ti',
         unit='character',
         source='tabs'),
  refnames=
    list(type=class(recvect[[1]]$refnames),
         description='json object including the name and id of intersecting labeling points/polygons (from t0 until ti, where t=time period) within a reconstructed polygon of ti',
         unit='character',
         source='tabs')
  )

# LABS METADATA 
meta_ext_ref <- list(xmin=xmin(labs), 
                     xmax=xmax(labs), 
                     ymin=ymin(labs), 
                     ymax=ymax(labs))
# if(all(grepl('unique_id&name&area_km2&coastline_km&plate&land_type_id&uniquename&refx&refy&refz&refn',names(labs)))){
#   meta_names_ref <- list(
#     unique_id=
#       list(type=class(labs$unique_id),
#            description='integer; unique identifier of a biogeographic unit in labeling dataset',
#            unit='number',
#            source='tabs'),
#     name=
#       list(type=if('name' %in% colnames(labs)){class(labs$name)}else{'not exported'},
#            description='character; name of the island as identified by the original global shoreline vector, i.e. labeling dataset (Sayre et al. 2018). Based on the highest point within the labeling polygon',
#            unit='character',
#            source='tabs'),
#     uniquename=
#       list(type=class(labs$uniquename),
#            description='character; name and unique identifier',
#            unit='character',
#            source='tabs'),
#     refx=
#       list(type=class(labs$refx),
#            description='numeric; x-coordinate (SRID=4326) of the highest point of a biogeographic unit in the labeling dataset; in case the labeling dataset are points the x-coordinate of the point is given',
#            unit='degrees',
#            source='tabs'),   
#     refy=
#       list(type=class(labs$refy),
#            description='numeric; y-coordinate (SRID=4326) of the highest point of a biogeographic unit in the labeling dataset; in case the labeling dataset are points the y-coordinate of the point is given',
#            unit='degrees',
#            source='tabs'),   
#     refz=
#       list(type=class(labs$refz),
#            description='integer; elevation of the highest point of a biogeographic unit in the labeling dataset; in case the labeling dataset are points the z-coordinate of the point is given',
#            unit='number',
#            source='tabs'),
#     refn=
#       list(type=class(labs$refn),
#            description='integer; number of cells at the resolution of the topo within a biogeographic unit in the labeling dataset; in case the labeling dataset are points the number of cells will equal to 1',
#            unit='number',
#            source='tabs'),
#     area_km2=
#       list(type=if('area_km2' %in% colnames(labs)){class(labs$area_km2)}else{'not exported'},
#            description='numeric; area expressed in square km as calculated by the original global shoreline vector',
#            unit='squared kilometer',
#            source='gsv'),
#     coastline_km=
#       list(type=if('coastline_km' %in% colnames(labs)){class(labs$area_km2)}else{'not exported'},
#            description='numeric; coastline expressed in km as calculated by the original global shoreline vector',
#            unit='kilometer',
#            source='gsv'),     
#     plate=
#       list(type=if('plate' %in% colnames(labs)){class(labs$plate)}else{'not exported'},
#            description='character; tectonic plate id derived through intersection with the plate layer',
#            unit='character',
#            source='plates'),
#     land_type_id=
#       list(type=if('land_type_id' %in% colnames(labs)){class(labs$land_type_id)}else{'not exported'},
#            description='integer; whether it concerns mainland, a big island or a small island, respectively labeled 1,2,3; source data All_BI_Named_2 includes 1 and 2; global_islands_outputs.gdb includes 3',
#            unit='number',
#            source='gsv')
#   )
# } else {
  meta_names_ref <- list(
    unique_id=
      list(type=class(labs$unique_id),
           description='integer; unique identifier of a shape in the labeling dataset',
           unit='number',
           source='tabs'),
    uniquename=
      list(type=class(labs$uniquename),
           description='character; name and unique identifier',
           unit='character',
           source='tabs'),
    name=
      list(type=class(labs$name),
           description='character; name of a shape in the labeling dataset',
           unit='character',
           source='tabs'),     
    refx=
      list(type=class(labs$refx),
           description='numeric; x-coordinate (SRID=4326) of the highest point of a biogeographic unit in the labeling dataset; in case the labeling dataset are points the x-coordinate of the point is given',
           unit='degrees',
           source='tabs'),   
    refy=
      list(type=class(labs$refy),
           description='numeric; y-coordinate (SRID=4326) of the highest point of a biogeographic unit in the labeling dataset; in case the labeling dataset are points the y-coordinate of the point is given',
           unit='degrees',
           source='tabs'),   
    refz=
      list(type=class(labs$refz),
           description='integer; elevation of the highest point of a biogeographic unit in the labeling dataset; in case the labeling dataset are points the z-coordinate of the point is given',
           unit='number',
           source='tabs'),
    refn=
      list(type=class(labs$refn),
           description='integer; number of cells at the resolution of the topo within a biogeographic unit in the labeling dataset; in case the labeling dataset are points the number of cells will equal to 1',
           unit='number',
           source='tabs')
  )
#}
meta <- list(
  recvect=list( # OPEN - RECONSTRUCTION
               extent=meta_ext,
               'coord. ref.'=crs(recvect[[1]]),
               period=list(names(recvect)),
               sources=meta_sources,
               description=meta_descriptions,
               names=meta_names), # CLOSED - RECONSTRUCTION
  labs=list( # OPEN - LABS
    extent=meta_ext_ref,
    'coord. ref.'=crs(labs),
    period=0,
    names=meta_names_ref
    ) # CLOSED - labs
  )
  
metadata_json <- jsonlite::toJSON(list('metadata'=meta, 'data'= c('labs', 'recvect')), pretty =FALSE, auto_unbox =TRUE)
return(metadata_json)
}


#' @title metadata_table
#' 
#' @author Johannes De Groeve
#' @description convert metadata file created with metadata_file to metadata table 
#'
#' @param meta json object 
#'
#' @return a list of dataframes with metadata derived from the metadata json 
#' 
#' @noRd
#' @keywords internal
#' 
metadata_table <- function(meta){
  # examples
  # data <- reconstruct()
  # meta <- metadata_file(recvect=data$output$recvect,labs=data$input$labs)
  # meta_table <- metadata_table(meta)
  js <- jsonlite::fromJSON(meta)
  
  recvect_metadata <- data.frame(
    dataset_name=rep(names(js$metadata)[1],length(js$metadata$recvect$names)),
    column_name=names(js$metadata$recvect$names),
    description_type='column description',
    source=sapply(1:length(js$metadata$recvect$names), function(v) js$metadata$recvect$names[[v]]$source),
    description=paste0(sapply(1:length(js$metadata$recvect$names), function(v) js$metadata$recvect$names[[v]]$unit),'; ',
                       sapply(1:length(js$metadata$recvect$names), function(v) js$metadata$recvect$names[[v]]$description)))
  labs_metadata <- data.frame(
    dataset_name=rep(names(js$metadata)[2],length(js$metadata$labs$names)),
    column_name=names(js$metadata$labs$names),
    description_type='column description',
    source=sapply(1:length(js$metadata$labs$names), function(v) js$metadata$labs$names[[v]]$source),
    description=paste0(sapply(1:length(js$metadata$labs$names), function(v) js$metadata$labs$names[[v]]$unit),'; ',
                       sapply(1:length(js$metadata$labs$names), function(v) js$metadata$labs$names[[v]]$description)))
  
  dataset_metadata <- data.frame(
    dataset_name=names(js$metadata$recvect$sources),
    column_name=NA,
    description_type='dataset description',
    source=as.vector(unlist(js$metadata$recvect$sources)),
    description=paste0(as.vector(unlist(js$metadata$recvect$description))))
  
  return(list(recvect_metadata,labs_metadata,dataset_metadata))
}


#' @title Metadata of object of class tabs (reconstruction)
#' 
#' @author Johannes De Groeve
#' @description retrieve dataset and column descriptions as well as the sources from reconstruction object
#'
#' @param x object of class tabs 
#'
#' @return a list of data frames with a description of columns of vector datasets and the sources of the input datasets
#' 
#' @export
#'
#' @inherit reconstruct examples
#' 
metadata <- function(x){
  M <- metadata_table(x$metadata)
  names(M) <- c('recvect','labs','sources')
  return(M)
}



