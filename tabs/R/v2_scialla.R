#' #' @importFrom readr parse_number
#' #' @importFrom terra resample terraOptions crs
#' #' @importFrom parallel mclapply detectCores
#' #'
#' #'
#' #' @title scialla_tile_stack
#' #'
#' #' @author Johannes De Groeve
#' #' @description generates sea shelf extent map for every sea level stand using a spatiotemporal sea level curve. The function works on a tile stack with the same extent
#' #'
#' #' @param r bathymetric model (tile/extent)
#' #' @param seacurve paths of the sea level curve dataset
#' #' @param res_fact resolution factor compared to the original resolution of the bathymetry
#' #' @param isohypse sea level stand below sea level for which to calculate the sea shelf extent (e.g. -40 to 0; isohypse=40)
#' #' @param to_path output path where the resulting dataset is stored, a directory will be created named after the sealevel
#' #'
#' #' @keywords internal
#' #' @return exports a map for every time period for which the sea level stand is known
#' #'
#' #'
#' scialla_tile_stack <- function(r,seacurve, res_fact = 8, isohypse=40, to_path,name='ISO'){
#' 
#'   # SELECT THE SEA CURVE OF INTEREST
#'   if(is.null(seacurve)){ # present
#'     seacurvestack <- get_curve()
#'   } else {
#'     if(is.character(seacurve)){ # curve stored in database specified as character
#'       if(length(seacurve) > 1){ # unique(grepl('.ASC$',seacurve))
#'       seacurve_names <- seacurve
#'       seacurve <- unique(dirname(seacurve))
#'       }
#'       seacurvestack <- get_curve(seacurve)
#'       terra::crs(seacurvestack) <- terra::crs(r)
#'     } else { # curve stored in package
#'       seacurvestack <- seacurve
#'     }
#'   }
#' 
#'    #seacurvestack <- terra::rast(seacurve)
#'    #terra::crs(seacurvestack) <- terra::crs(r)
#' 
#'   if(class(seacurvestack)[1] == 'SpatRaster'){
#'     e <- terra::ext(seacurvestack)
#'     elev <-terra::crop(r, e)
#'   } else {
#'     elev <- r
#'   }
#' 
#'   # depending from the wished resolution we will need to aggregate
#'   if(is.null(res_fact) | res_fact == 0){
#'     res_fact <- 0
#'   } else {
#'     elev <- terra::aggregate(elev, fact=res_fact, threads=TRUE)
#'   }
#' 
#'   # RESAMPLE CURVE
#'   if(class(seacurvestack)[1] == 'SpatRaster'){
#'   seacurvestack <- terra::resample(seacurvestack, elev, method="bilinear")
#'   print('seacurve stack resampled')
#'   period <- data.frame(index=1:dim(seacurvestack)[3],period=readr::parse_number(names(seacurvestack)))
#'   } else { # if csv
#'   period <- data.frame(index=1:length(seacurvestack),period=readr::parse_number(names(seacurvestack)))
#'   }
#'   # CALCULATION
#'   # SUPPOSE TO PUT NEGATIVE VALUES
#' 
#'   # below_sealevel_only <- lapply(isohypse, function(x){
#'   #   if(x < 0){elev >= (seacurvestack + x) & elev < (seacurvestack)}
#'   #   if(x > 0){elev >= seacurvestack & elev < (seacurvestack + x)}
#'   #   if(x == 0){elev >= (seacurvestack)}
#'   #   })
#'   #
#'   # # below_sealevel_only <- lapply(isohypse, function(x) elev >= seacurvestack & elev < (seacurvestack + x))
#'   # # below_sealevel_only <- lapply(isohypse, function(x) elev >= (seacurvestack + x) & elev < (seacurvestack))
#'   # if(max(isohypse) <= 0){above_sealevel <- elev >= (seacurvestack)}
#'   # if(max(isohypse) > 0){above_sealevel <- elev >= (seacurvestack + max(isohypse))}
#' 
#'   #pp <- elev >= (seacurvestack[[53]] - 500) & elev < (seacurvestack[[53]])
#'   if(length(isohypse) == 1 & isohypse==0){ # ONLY ISLAND RECONSTRUCTION
#'     scialla <- elev >= (seacurvestack)
#'     terra::NAflag(scialla) <- 0
#'     } else {
#'     below_sealevel_only <- lapply(isohypse, function(x) elev >= (seacurvestack - x) & elev < (seacurvestack))
#'     above_sealevel <- elev >= (seacurvestack)
#'     c <- paste0('scialla <- ', paste0('below_sealevel_only[[',1:length(below_sealevel_only),']]',collapse=' + '),
#'              ' + (',length(isohypse) + 1,' * above_sealevel)')
#'     scialla <- eval(parse(text = c))
#'     terra::NAflag(scialla) <- 0
#'     scialla <- (scialla - 1)
#'     }
#' 
#' 
#' 
#'   # THE DIRECTORY NAME OF THE NEW DATASET
#'   DIR <- paste0(to_path, name, res_fact,'_',paste0(isohypse,collapse='_'),'/')
#'   # THE PATH NAMES
#'   # IF DIR DOES NOT EXIST, GENERATE THE FOLDER STRUCTURE
#'   if(class(seacurvestack)[1] == 'SpatRaster'){
#'   from_path <- paste0(dirname(dirname(dirname(seacurve_names))),'/')[1]
#'   #to_path <- paste0(dirname(dirname(dirname(dirname(dirname(seacurve))))),'/',DIR)[1]
#' 
#'   paths <- gsub('RSL',name,gsub(gsub('[()]','',from_path),DIR,gsub('[()]','',seacurve_names)))
#'   #paths <- gsub('RSL','SLW',gsub('koene/MAPS/',DIR,seacurve))
#' 
#'   if(!dir.exists(dirname(paths[1]))){
#'     copy.dir.tree(from = from_path, to = DIR)
#'   }
#' 
#'   } else { # DOUBLE CHECK IF IT WORKS FOR BINTANJA
#'     if(!dir.exists(DIR)){
#'       dir.create(DIR)
#'     }
#'     paths <- paste0(DIR,name,stringr::str_pad(as.numeric(names(seacurvestack))/100,3,pad='0'),'.ASC')
#'   }
#'   # WRITE RASTERS
#'   terra::writeRaster(scialla,filename=paths,filetype='AAIGrid',overwrite=T)
#'   return(list(from_path, to_path))
#' }
#' 
#' 
#' #' @title scialla
#' #'
#' #' @author Johannes De Groeve
#' #' @description loop to generate extent maps for different isohypses for every sea level stand using a spatiotemporal sea level curve.
#' #'
#' #' @param r bathymetric model path or spatraster object
#' #' @param curve root directory of the sea level curve
#' #' @param v polygon dataset
#' #' @param res_fact resolution factor compared to the original resolution of the bathymetry
#' #' @param isohypse sea level stand below sea level for which to calculate the sea shelf extent (e.g. -40 to 0; isohypse=40)
#' #' @param tempdir directory where to write temporary files.
#' #' @param path output path where the resulting dataset is stored, a directory will be created named after the isochrone(s)
#' #' @param ncores number of cores to compute the extent maps
#' #' @param name dataset name (ISO)
#' #' @param verbose whether to print messages
#' #' @param mosaic boolean (TRUE/FALSE) whether to mosaic
#' #' @param ZIP boolean (TRUE/FALSE) whether to ZIP
#' #' @param GIF boolean (TRUE/FALSE) whether to create a GIF
#' #' @param e extent coordinates c(xmin,xmax,ymin,ymax) (for GIF)
#' #'
#' #' @return exports a map for every time period for which the sea level stand is known
#' #' @keywords internal
#' #' @export
#' #'
#' scialla <- function(r,
#'                     curve, # TODO **piac v1** test if scialla is working for temporal sea level curves, mosaic won't be necessary for e.g. Lambeck since it can be done globally
#'                     v=NULL,
#'                     res_fact=8,
#'                     isohypse=c(30,150), # TODO **piac v1** extend to above sea level
#'                     tempdir=NULL,
#'                     path='~',
#'                     ncores=parallel::detectCores(),
#'                     name='ISO',
#'                     verbose=F,
#'                     mosaic=T,
#'                     ZIP=F,
#'                     GIF=NULL,
#'                     e=NULL
#'                     ){
#'   # set temp directory
#'   if(is.null(tempdir)){
#'     tempdir <- tempfile(pattern=paste0("GLOB"))
#'     terra::terraOptions(tempdir=tempdir,verbose=TRUE)
#'   } else {
#'     tempdir <- tempfile(pattern=paste0("GLOB"),tmpdir=tempdir)
#'     terra::terraOptions(tempdir=tempdir,verbose=TRUE)
#'   }
#' 
#'   # import gebco raster if not yet imported
#'   if(!class(r)[1] == 'SpatRaster'){
#'     r <- terra::rast(r)
#'   }
#' 
#'   curve_tiles <- list.files(curve,recursive=TRUE, pattern='.ASC$') # list tiles
#'   # check if the dataset is already generated
#'   if(is.null(res_fact) | res_fact == 0){
#'     res_fact <- 0
#'   }
#'   dataset_root <- paste0(path,name,res_fact,'_',paste0(isohypse,collapse='_'),'/')
#'   print(dataset_root)
#'   message(dataset_root)
#'   dataset_tiles <- list.files(dataset_root,recursive=TRUE, pattern='.tiles.zip$|.ASC$') # list tiles
#' 
#'   if(length(dataset_tiles) == 0){ # open - check if the dataset already exists (files present in the expected directory)
#'   # only obtain tiles for subset that intersects with extent
#'   if(!is.null(e)){
#'     lon <- dirname(curve_tiles)
#'     lat <- dirname(lon)
#'     lon<- strsplit(basename(lon),'-')
#'     lat<- basename(lat)
#'     lon_min <- unlist(lapply(lon, function(x) as.numeric(x[1])))
#'     lon_max <- unlist(lapply(lon, function(x) as.numeric(x[2])))
#'     lon_min[which(lon_min >= 180)] <-   (lon_min[which(lon_min >= 180)] - 360) # DOUBLE CHECK IF IT WORKS AT THE EQUATOR
#'     lon_max[which(lon_max > 180)] <-   (lon_max[which(lon_max > 180)] - 360) # DOUBLE CHECK IF IT WORKS AT THE EQUATOR
#' 
#'     lat_min <- as.numeric(gsub('p','',gsub('m','-',lat)))
#'     lat_max <- lat_min + 10
#'     curve_tiles <- curve_tiles[which(lon_min > (e[1] - 10) & lon_max < (e[2] + 10) & lat_min > (e[3] - 10) & lat_max < (e[4] + 10) )]
#'    # curve_tiles_exclude <- curve_tiles[!which(lon_min > (e[1] - 10) & lon_max < (e[2] + 10) & lat_min > (e[3] - 10) & lat_max < (e[4] + 10) )]
#'   }
#' 
#'   looper <- list.files(curve)
#'   looper <- unique(dirname(dirname(curve_tiles)))
#'     for(i in 1:length(looper)){
#'       if(verbose){start <- Sys.time()}
#'       curve_tiles_lat <- curve_tiles[grep(pattern = paste0('^',looper[i],'.*'), curve_tiles)] # subset latitude
#'       curve_tiles_latlon <- split(curve_tiles_lat,f=substr(curve_tiles_lat,1,11)) # split by longitude
#'       seacurve_path_l <- lapply(curve_tiles_latlon, function(x) paste0(curve,x)) # list path names
#' 
#'       # CREATE SHALLOW WATER RASTER FOR A CERTAIN LATITUDE
#'       parallel::mclapply(seacurve_path_l, function(x) scialla_tile_stack(r=r,seacurve=x,res_fact=res_fact,isohypse=isohypse,to_path=path,name=name),mc.cores=ncores)
#'       message(paste0(looper[i], ' exported'))
#'       if(verbose){end <- Sys.time(); message(end - start)}
#'     }
#'   } # close - check if the dataset already exists
#' 
#'   # in case of missing tiles
#'   dataset_tiles <- list.files(dataset_root,recursive=TRUE, pattern='.ASC$') # list tiles
#'   seacurve_path_missing <- curve_tiles[c(!curve_tiles %in% gsub(name,'RSL',dataset_tiles))]
#'   if(length(seacurve_path_missing) > 0){
#'   seacurve_path_missing_l <- split(seacurve_path_missing,f=substr(seacurve_path_missing,1,11)) # split by longitude
#'   seacurve_path_missing_l <- lapply(seacurve_path_missing_l, function(x) paste0(curve,gsub('\\+','',x))) # list path names
#'   parallel::mclapply(seacurve_path_missing_l, function(x) scialla_tile_stack(r=r, seacurve=x, res_fact=res_fact, to_path=path, isohypse=isohypse, name=name),mc.cores=ncores)
#'   } else {
#'     message('there are no missing tiles')
#'   }
#' 
#'   # MOSAIC DATASET
#'   dataset_tiles <- list.files(dataset_root,recursive=TRUE, pattern='.ASC$') # list tiles
#'   seacurve_path_missing <- curve_tiles[c(!curve_tiles %in% gsub(name,'RSL',dataset_tiles))]
#'   if(length(seacurve_path_missing) == 0){
#'     if(mosaic){
#'       message('start mosaic')
#'       if(is.null(e))
#'         {
#'          m <- mosaic(r=r,
#'                 data=dataset_root
#'                 )
#'         } else {
#'         m <- mosaic(r=r,
#'                data=dataset_root,
#'                e=e
#'                )
#' 
#'         }
#' 
#'     }
#'   } else {
#'     message(paste0('cannot mosaic, there are missing tiles in ', dataset_root))
#'   }
#' 
#'   # remove temporary files
#'   unlink(terra::tmpFiles())
#' 
#'   if(ZIP){
#'     #dataset_root <- paste0(path,name,paste0(isohypse,collapse='_'),'/')
#'     message('start creating zip')
#'     zipping(data=dataset_root,
#'             path=dataset_root)
#'   }
#' 
#'   if(GIF){
#'     #dataset_root <- paste0(path,name,paste0(isohypse,collapse='_'),'/')
#'     message('start creating gif')
#'     #if(global){
#'     # giffing(data=dataset_root,
#'     #         path=dataset_root,
#'     #         v=v,
#'     #         e=e,
#'     #         col=c(colorRampPalette(c('blue','lightblue'))(length(isohypse)),'grey')#,
#'     #         #ncores=ncores
#'     #         )
#'     # } else {
#'       giffing(data=dataset_root, # TODO **piac v1** I am quite sure that global argument can be removed
#'               path=dataset_root,
#'               v=v,
#'               e=NULL,
#'               col=c(colorRampPalette(c('blue','lightblue'))(length(isohypse)),'grey')#,
#'               #ncores=ncores
#'               )
#'     #}
#' 
#'   }
#' 
#'   if(!mosaic){
#'     DIR_G <- paste0(dataset_root,'/',gsub('[0-9]','',name),'_mosaic/')
#'     if(dir.exists(DIR_G)){
#'       m <- rast(list.files(DIR_G,full.names = T))
#'       } else {
#'       print(paste0(DIR_G,' does not exist'))
#'     }
#'   }
#' 
#'   return(m)
#' }
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
