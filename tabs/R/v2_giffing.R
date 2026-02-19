#' #' @importFrom terra ext centroids plot
#' #' @importFrom stringi stri_pad_left
#' #'
#' #' @title mosaic
#' #'
#' #' @author Johannes De Groeve
#' #' @description gif of generated iso data
#' #'
#' #' @param data path to the ISO dataset
#' #' @param path path where to save the GIF
#' #' @param v path of a spatial object to extract a name from to add as title
#' #' @param e extent of inset map
#' #' @param title title of inset map
#' #' @param col color of iso-zones
#' #'
#' #' @export
#' #' @keywords internal
#' #'
#' #' @return exports a global mosaic for every time period
#' #'
#' #'
#' giffing <- function(data=dataset_root,
#'                     path=NULL,
#'                     v=NULL, #'../../data/tectonicplates/PB2002_plates.shp'
#'                     e=NULL,
#'                     title=NULL,
#'                     col=c('lightblue','blue','grey')
#'                     #ncores=parallel::detectCores()
#'                     ){
#'   require(stringi)
#'   P <- list.files(data,full.names = T)
#'   P_SLW <- P[grepl('mosaic',P)]
#'   F_SLW <- list.files(P_SLW,full.names = T)
#'   name <- gsub('_','',gsub('[0-9]','',basename(data)))
#' 
#'   # read rasters
#'   R_SLW <- terra::rast(F_SLW)
#' 
#'   # crop inset
#'   if(!is.null(e)){ # open - create inset extent based on extent coordinates
#'     ee <- terra::rast()
#'     terra::ext(ee) <- e
#'     RC_SLW <- terra::crop(R_SLW,ee)
#' 
#'     # name inset based on plate in centroid
#'     if(is.null(title)){ # open - create title of inset based on vector layer if title is NULL
#'       # centroid polygon intersecting vector
#'       centroid <- as.polygons(terra::ext(ee), crs=terra::crs(ee)) %>% terra::centroids()
#' 
#'       if(!is.null(v)){
#' 
#'           # check input of v
#'           if(class(v)[1] %in% c('character','sf')){
#'           v <- terra::vect(v)
#'           } else {
#'           v <- v
#'           }
#' 
#'         } else { # otherwise use default layer in package
#'         v <- vect(plates) # TODO **piac v1** test if default plates is working with the giffing function
#'       }
#'       platename <- terra::intersect(centroid,v)
#'       title <- paste0(platename$PlateName, ' Plate')
#'     }
#'   } # close - create inset extent based on extent coordinates
#' 
#'   M_SLW <- paste0(as.numeric(gsub(name,'',gsub('.tif','',basename(F_SLW))))*100, ' Years BP')
#' 
#'   if(!is.null(path)){
#'   png_path <- paste0(path,'/png/')
#'   if(!dir.exists(png_path)){
#'     dir.create(png_path,recursive = T)
#'   }
#'   png_path_temp <- paste0(png_path,paste0(name, gsub(' ','',title),collapse='_'))
#'   if(!dir.exists(png_path_temp)){
#'     dir.create(png_path_temp,recursive = T)
#'   }
#'   }
#' 
#'   plotname <- dim(R_SLW)[3]:1
#'   for(i in 1:dim(R_SLW)[3]){
#'     if(!is.null(path)){
#'     png(filename=paste0(png_path_temp,'/Rplot',stringi::stri_pad_left(plotname[i], 5, 0),'.png'),width=1200,height=350)
#'     }
#' 
#'     par(mfrow=c(1,2), mar=c(1,2,1,2))
#'     if(is.null(e)){
#'     par(mfrow=c(1,1), mar=c(1,2,1,2))
#'     }
#' 
#'     terra::plot(R_SLW[[i]], col=col, main=M_SLW[i],legend=FALSE)
#' 
#'     if(!is.null(e)){
#'     terra::plot(RC_SLW[[i]], col=col, main=paste0(title,' ',M_SLW[i]))
#'     }
#'     if(!is.null(path)){
#'     dev.off()
#'     }
#'     print(i)
#'   }
#'   #files <- list.files(pattern='.gif','./results/')
#'   if(!is.null(path)){
#'   system(paste0('convert -delay 20 ',
#'                 gsub(" ", "\\ ",png_path_temp,fixed=T),'/Rplot*.png -loop 0 ',
#'                 gsub(" ", "\\ ",path,fixed=T),'/',
#'                 paste0(name,gsub(' ', '',title),collapse='_'),
#'                 '.gif'))
#'   }
#'   # Remove temporary pngs
#'   if(!is.null(path)){
#'   gif_path <- paste0(path,'/',paste0(name,gsub(' ', '',title),collapse='_'),'.gif')
#'   if(file.exists(gif_path)){unlink(png_path,recursive = T)}
#'   }
#'   #if(is.null(path)){return()}
#' }
#' 
