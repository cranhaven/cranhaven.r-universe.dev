#' @importFrom terra wrap cats
#' @importFrom gpkg gpkg_create_spatial_view
#' @importFrom sf st_layers st_read
#' @importFrom qs2 qs_save
#' @importFrom RSQLite dbExecute
#' 
#' @title Export a reconstruction of class tabs in various formats
#' 
#' @author Johannes De Groeve
#' @description export data 
#'
#' @param x tabs. Object of class tabs, after running the reconstruct-function.
#' @param filename character. Path where files will be exported. Default as directory tree. Use .qs2, .rds, .zip to save as qs2, rds or zipped directory tree.
#' @param overwrite boolean. Whether to overwrite the output when filename is specified.
#'
#' @return No return value, called for side effects
#'
#' @export
#'
#' @inherit reconstruct examples
#' 
export <- function(x,filename, overwrite=FALSE){
  oldwd <- getwd()
  on.exit(setwd(oldwd))
  
  # overwrite error message 
  overwrite_message <- "Error: file exists. You can use 'overwrite=TRUE' to overwrite it"

  if(grepl('.rds$|qs2$',filename)){ # open - save as rds
    x <- list(recvect=lapply(x$recvect, function(x) terra::wrap(x)),
              recrast= terra::wrap(x$recrast),
              recarea=x$recarea,
              labs=terra::wrap(x$labs),
              topo=terra::wrap(x$topo),
              curve=if(inherits(x$curve,'SpatRaster')){terra::wrap(x$curve)}else{x$input$curve},
              correction=if(inherits(x$correction,'SpatRaster')){terra::wrap(x$correction)}else{x$correction},
              metadata=x$metadata
              )
    
   if(!file.exists(filename)){ # check if file exists 
    if(grepl('rds$',filename)) {saveRDS(x,filename)}
    if(grepl('qs2$',filename)) {qs2::qs_save(x,filename)}
     
       } else {
         # if overwrite is true, remove the old version 
         if(overwrite){
           unlink(filename)
           if(grepl('rds',filename)) {saveRDS(x,filename)}
           if(grepl('qs2',filename)) {qs2::qs_save(x,filename)}
         } else {
           message(overwrite_message)
        }
       }
    
    
  } else {
  # datasets 
  
  # input 
  topo <- x$topo
  region <- x$labs
  curve <- x$curve
  correction <- x$correction
  
  # output
  paleov <- x$recvect
  paleor <- x$recrast
  area <- x$recarea

  # metadata 
  META <- x$metadata
  
  # set temp directory
  if(!grepl('.rds$|.gpkg$',filename)){
    #tdir <-tempfile(pattern=paste0("RECON"))
    #dir.create(tdir)
    if(grepl('.zip',filename)){ file <- '_zip' } else { file <- ""}
    
    if(!dir.exists(gsub('.zip',file,filename))) dir.create(gsub('.zip',file,filename),recursive=TRUE) 
    tdir <- gsub('.zip',file,filename)

  } else {
    tdir <- filename
  }

  tdir <- path.expand(tdir)
  
  if(grepl('.gpkg',filename)){ # open - save as rds
    vect_name <- ''
  } else {
    vect_name <- '/vect.gpkg'
  }
  #### WRITE INPUT DATA ####
  # if overwrite is true, remove the old version 
  if(grepl('.zip',filename) | grepl('.gpkg',filename)){
    if(grepl('.gpkg',filename)){
      if(file.exists(tdir)){
        if(overwrite){
          unlink(tdir)
        }
      }
    } 
    if(grepl('.zip',filename)){
      if(file.exists(paste0(tdir,'.zip'))){
        if(overwrite){
          unlink(paste0(tdir,'.zip'))
        }
      }
    }
    
  } else {
    if(dir.exists(tdir)){
        if(overwrite){
          unlink(tdir,recursive=TRUE)
          if(!dir.exists(filename)) dir.create(filename,recursive=TRUE)
      }
    }
  }
  
  # LABS
  terra::writeVector(region,
                     filename=paste0(tdir,vect_name), #tdir_input,/input
                     layer='labs',
                     overwrite=TRUE)

  # TOPO
  if(grepl('.gpkg',filename)){ # open - save as rds
  terra::writeRaster(topo,
                     paste0(tdir,vect_name), #tdir_input,/input
                     gdal = c("APPEND_SUBDATASET=YES", "RASTER_TABLE=topo"))
  } else {
  terra::writeRaster(topo,
                     paste0(tdir,'/topo.tif')
                    )
  }
    
  # CORRECTION 
  if(!is.list(correction)){ # check if correction is a list, if list no correction grid is used
    
    # correction is numeric
    if(inherits(correction,'integer') | inherits(correction,'numeric')){ # open - check if the correction grid is numeric 
      correction_df <- data.frame(year_before_after_present = names(correction), value=as.vector(correction), name=attr(correction,'source'))
      rownames(correction_df) <- NULL
      if(grepl('.gpkg',filename)){ # open - save as rds
        # export dataframe to geopackage 
        sf::st_write(correction_df,  
                     paste0(tdir,vect_name),
                     'correction',
                     quiet=TRUE
                    )
      } else {
        # If correction is data.frame store both in geopackage and outside geopackage 
        # export dataframe to geopackage 
        sf::st_write(correction_df,  
                     paste0(tdir,vect_name),
                     'correction',
                     quiet=TRUE
        )
        # export as table 
        sf::st_write(correction_df,  
                     paste0(tdir,'/correction.csv'),
                     'correction',
                     quiet=TRUE
        )
      }
      } # close - check if the correction grid is numeric 
    
    # correction is raster
    if(inherits(correction,'SpatRaster')){ # check if the correction grid is spatraster
    for(i in 1:dim(correction)[3]){
      rasn <- names(paleor)[i]
      if(grepl('.gpkg',filename)){ # open - save as rds
        # export rasters to geopackage 
        terra::writeRaster(correction[[i]],
                           paste0(tdir,vect_name),#tdir_input,/input
                           NAflag = NaN,
                           gdal = c("APPEND_SUBDATASET=YES", paste0("RASTER_TABLE=correction",rasn)),
                           filetype='GPKG')
      }else{
        # export rasters as tif 
        tdir_correction <- paste0(tdir,'/correction/')
        if(!dir.exists(tdir_correction)){dir.create(tdir_correction)}
        terra::writeRaster(correction[[i]],
                           paste0(tdir_correction,'correction',rasn,'.tif'),#tdir_input,/input
                           NAflag = NaN)
      }
    }
    }

  } # check if correction is a list, if list no correction grid is used

  # CURVE
  if(!is.null(unlist(curve))){
    
    # TEMPORAL CURVE (EUSTATIC CURVE)
    if(inherits(curve,'integer') | inherits(curve,'numeric')){ # open - check if the curve is numeric 
      curve_df <- data.frame(year_before_after_present = names(curve), value=curve, name=attr(curve,'source'))
      rownames(curve_df) <- NULL
      if(grepl('.gpkg',filename)){ # open - save as rds
        # export dataframe to geopackage 
        sf::st_write(curve_df,  
                     paste0(tdir,vect_name),
                     'curve',
                     quiet=TRUE
        )
      }else{ 
        # If curve is data.frame store both in geopackage and outside geopackage 
        # export dataframe to geopackage 
        sf::st_write(curve_df,  
                     paste0(tdir,vect_name),
                     'curve',
                     quiet=TRUE
        )
        # export as table 
        sf::st_write(curve_df,  
                     paste0(tdir,'/curve.csv'),
                     'curve',
                     quiet=TRUE
        )
      }
      
    } # close - check if the curve is numeric 
    
    # SPATIO-TEMPORAL CURVE (SPATIAL-EXPLICIT CURVE)
    if(inherits(curve,'SpatRaster')){ # open - check if the curve is spatraster
      for(i in 1:dim(curve)[3]){ # open - export spatraster
        rasn <- names(paleor)[i]
        if(grepl('.gpkg',filename)){ # open - save as rds
          # export rasters as gpkg 
          terra::writeRaster(curve[[i]],
                             paste0(tdir,vect_name),#tdir_input,/input
                             gdal = c("APPEND_SUBDATASET=YES", paste0("RASTER_TABLE=curve",rasn)),
                             NAflag = NaN,
                             filetype='GPKG')
        } else {
          # export rasters as tif 
          tdir_curve <- paste0(tdir,'/curve/')
          if(!dir.exists(tdir_curve)){dir.create(tdir_curve)}
          terra::writeRaster(curve[[i]],
                             paste0(tdir_curve,'curve',rasn,'.tif'),#tdir_input,/input
                             NAflag = NaN)
    }
      
    }
    } # close - export spatraster
    
  } # close - check if the curve is spatraster
  
  #### WRITE OUTPUT DATA ####
  
  #### GPKG (Vector) ####
  for(i in 1:length(paleov)){
    vecn <- names(paleov)[i]
    terra::writeVector(paleov[[i]],
                       filename=paste0(tdir,vect_name), #tdir_output
                       insert = TRUE,
                       layer = paste0('recvect',vecn),
                       overwrite=TRUE)
  }
  
  #### CREATE A VIEW ####
  gpkg::gpkg_create_spatial_view(
    paste0(tdir,vect_name), 
    'recvect', 
    paste0('SELECT * FROM recvect',names(paleov),collapse=' UNION ')
    )
  
  #### RASTER ####
  for(i in 1:dim(paleor)[3]){
    rasn <- names(paleor)[i]
    if(grepl('.gpkg',filename)){ # open - save as rds
      terra::writeRaster(if(nrow(terra::cats(paleor[[i]])[[1]]) == 1) {if(terra::cats(paleor[[i]])[[1]]$iso == 0){
                                 terra::as.bool(paleor[[i]]) }} else {paleor[[i]]}, # if only iso is at 0 meter, turn into a boolean raster
                         paste0(tdir,vect_name), #tdir_output
                         NAflag = NaN,
                         gdal = c("APPEND_SUBDATASET=YES", paste0("RASTER_TABLE=recrast",rasn)),
                         filetype='GPKG')
    }else{
      # export rasters as tif 
      tdir_recrast <- paste0(tdir,'/recrast/')
      if(!dir.exists(tdir_recrast)){dir.create(tdir_recrast)}
      terra::writeRaster(paleor[[i]],
                         paste0(tdir_recrast,'recrast',rasn,'.tif'),#tdir_input,/input
                         NAflag = NaN)
    }

  }
  
  #### LEVELS ####
  lev <- as.data.frame(terra::cats(x$recrast)[[1]])
  sf::st_write(lev,
               paste0(tdir,vect_name),
               layer='iso',
               quiet=TRUE
               )
  
  #### AREA #### 
  if(!is.null(area)){
    if(!grepl('.gpkg',filename)){ # open - save as rds
    #area_df <- cbind(data.frame(name=rownames(area)),as.data.frame(area))
    write.table(x=area, #area_df
                file=paste0(tdir,'/recarea.csv'), #tdir_output
                sep=';',
                col.names=NA
    )
    } 
    sf::st_write(area, #area_df
                 paste0(tdir,vect_name),
                 layer='recarea',
                 quiet=TRUE
                 )
  }

  #### META ####
    # if(is.null(html)){} else {
    #   htmlwidgets::saveWidget(widget=timelaps(paleov),paste0(tdir,'/reconstruction.html'))
    #   }
  if(!grepl('.gpkg',filename)){ # open - save as rds
   write(META, paste0(tdir, '/metadata.json'))
  }
  #write.csv(metadata_reconstruction,paste0(tdir_output,'/reconstruction_metadata.csv'))
  #write.csv(metadata_labs,paste0(tdir_input,'/labs_metadata.csv'))
  META_TABLE <- unique(metadata_table(meta=META))
  metadata_reconstruction <- META_TABLE[[1]]
  metadata_labs <- META_TABLE[[2]]
  metadata_dataset <- META_TABLE[[3]]
  sf::st_write(data.frame(metadata=as.character(META)),
               paste0(tdir,vect_name),#tdir_input,'/input.gpkg'
               layer = "metadata",
               quiet=TRUE)
  sf::st_write(metadata_labs,
               paste0(tdir,vect_name),#tdir_input,'/input.gpkg'
               layer = "labs_metadata",
               quiet=TRUE)
  sf::st_write(metadata_reconstruction,
               paste0(tdir,vect_name),#tdir_output
               layer = "recvect_metadata",
               quiet=TRUE)
  sf::st_write(metadata_dataset,
               paste0(tdir,vect_name),#tdir_input,'/input.gpkg'
               layer = "sources",
               quiet=TRUE)

  #terra::writeRaster(bat,paste0(tdir,'/topo.gpkg'),overwrite=TRUE)

  if(grepl('.zip$',filename)){
    setwd(tdir) 
    # zip(zipfile=paste0(filename),
    #     files=list.files(tdir,full.names = TRUE),
    #     flags = '-r') # '-r9Xj'
    #sink(tempfile()) 
    zip(zipfile=paste0(filename),
        files=list.files(),
        flags = '-r') # '-r9Xj'
    unlink(gsub('.zip','_zip',paste0(tdir)),recursive=TRUE)
    #sink()
    setwd(oldwd)
  }
    
  } # close - save as rds
    
}



# 
# metadata <- terra::vect('/Users/jedgroev/Downloads/empty.gpkg',query='SELECT * FROM gpkg_metadata_labs;',what='attributes')
# 
# res11 <- reconstruct(region=region,
#                      curve=curve,
#                      correction=correction,
#                      reclabs='name',
#                      filename='~/tabs_temp_trial')
# p <- '/Users/jedgroev/tabs_temp_trial/output/reconstruction.gpkg'
# 
# # Create a table
# gpkg_metadata <- data.frame(
#   id = integer(),
#   md_scope = character(),
#   md_standard_uri = character(),
#   mime_type = character(),
#   metadata = character(),
#   stringsAsFactors = FALSE
# )
# write.table(gpkg_metadata,
#             filename=p,
#             layer='gpkg_metadata')
# 
# 
# # Write to a GeoPackage
# write.table(gpkg_metadata,'/Users/jedgroev/tabs_temp_trial/output/reconstruction.gpkg', row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)
# 
# sf::st_layers('/Users/jedgroev/tabs_temp_trial/output/reconstruction.gpkg')
# 
# # Create a table
# gpkg_metadata <- data.frame(
#   id = integer(),
#   md_scope = character(),
#   md_standard_uri = character(),
#   mime_type = character(),
#   metadata = character(),
#   stringsAsFactors = FALSE
# )
# 
# # Add constraints and defaults
# attributes(gpkg_metadata)$constraints <- list(
#   id = constraint(primary = TRUE, notnull = TRUE),
#   md_scope = constraint(notnull = TRUE),
#   md_standard_uri = constraint(notnull = TRUE),
#   mime_type = constraint(notnull = TRUE),
#   metadata = constraint(notnull = TRUE)
# )
# attributes(gpkg_metadata)$defaults <- list(
#   md_scope = "dataset",
#   mime_type = "text/xml",
#   metadata = ""
# )
# 
# # Write to a GeoPackage
# write.table(gpkg_metadata, "gpkg_metadata", row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)
# 
