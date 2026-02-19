#' #' @importFrom gdalUtils mosaic_rasters gdalinfo
#' #' @importFrom raster projection extent raster
#' #' @importFrom terra crs
#' #'
#' #' @title mosaic
#' #'
#' #' @author Johannes De Groeve
#' #' @description zip the generated iso data
#' #'
#' #' @param r bathymetric model
#' #' @param data path to the ISO dataset
#' #' @param e extent
#' #'
#' #' @export
#' #' @keywords internal
#' #'
#' #' @return exports a global mosaic for every time period
#' #'
#' #'
#' mosaic <- function(r, data=dataset_root,e=NULL)
#' {
#'   # function
#'   substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
#' 
#'   # import gebco raster if not yet imported
#'   if(!class(r)[1] == 'SpatRaster'){
#'     r <- terra::rast(r)
#'   }
#' 
#'   # remove trailing slash from dataset path
#'   data <- gsub('/$','',data)
#'   name <- gsub('_','',gsub('[0-9]','',basename(data)))
#'   tilenames <- list.files(data,pattern='.ASC$', recursive = TRUE,full.names = T)
#'   tilenames_per_period <- split(tilenames, f=substrRight(tilenames,7))
#' 
#'   for(i in 1:length(tilenames_per_period)){
#' 
#'     PERIOD <- tilenames_per_period[[i]]
#'     filename <- paste0(name,gsub('.ASC','',names(tilenames_per_period))[i],".tif")
#'     filedir <- paste0(data,'/',filename)
#' 
#'     DIR_G <- paste0(data,'/',name,'_mosaic/')
#'     if(!file.exists(paste0(DIR_G,filename))){
#' 
#'     # make an empty raster with the wished extent
#'     e_gebco <- r %>% raster::raster() %>% raster::extent()
#'     template <- raster::raster(e_gebco)
#' 
#'     raster::projection(template) <- terra::crs(r)
#'     if(!is.null(e)){
#'       raster::extent(template) <- e
#'     # raster::extent(template) <- c(floor((e[1])/10 ) * 10,
#'     #                               ceiling((e[2])/10 ) * 10,
#'     #                               floor((e[3])/10) * 10,
#'     #                               ceiling((e[4])/10) *10)
#'     }
#' 
#'     terra::writeRaster(template, file=filedir, format="GTiff",overwrite=TRUE)
#'     # mosaic the raster
#'     if(!is.null(e)){
#'     PERIOD <- PERIOD[!grepl('nan',as.vector(unlist(lapply(PERIOD,function(x) gdalinfo(x)[32]))))] # EXCLUDE TILES WITH ONLY NULL VALUES
#'     }
#'     gdalUtils::mosaic_rasters(gdalfile=PERIOD,dst_dataset=filedir,of="GTiff")
#'     gdalUtils::gdalinfo(filedir)
#' 
#'     if(!dir.exists(DIR_G)){
#'       dir.create(DIR_G)
#'     }
#'     rr <- rast(filedir)
#'     if(!is.null(e)){rr <- terra::crop(rr,ext(e))}
#'     terra::writeRaster(rr, filename=paste0(DIR_G,filename),overwrite=T)
#'     message(paste0('write mosaic ', i, ' to ',DIR_G))
#'     unlink(filedir)
#'     unlink(paste0(filedir,'.aux.xml'))
#'     message(paste0('remove mosaic ', i))
#'     }
#'     message(paste0(DIR_G,filename,' exists!'))
#'   }
#' 
#'   res <- rast(list.files(DIR_G,full.names = T))
#'   return(res)
#' }
