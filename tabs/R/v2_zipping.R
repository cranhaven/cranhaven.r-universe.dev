#' #' @importFrom utils zip
#' #'
#' #' @title zipping
#' #'
#' #' @author Johannes De Groeve
#' #' @description zip the generated iso data
#' #'
#' #' @param data path to the ISO dataset
#' #' @param path path where to save the dataset
#' #'
#' #' @return exports two zip files: ISO_tiles and ISO_mosaic
#' #'
#' #' @keywords internal
#' #' @export
#' #'
#' zipping <- function(data=dataset_root,
#'                     path='~'){
#'   dataset_tiles <- list.files(data,recursive=TRUE) # list tiles
#'   name <- gsub('_','',gsub('[0-9]','',basename(data)))
#' 
#'   glob <- dataset_tiles[grepl('mosaic',dataset_tiles)]
#'   glob <- glob[!grepl('.zip',glob)]
#'   glob <- glob[!grepl('.gif',glob)]
#' 
#'   tile <- dataset_tiles[!grepl('mosaic',dataset_tiles)]
#'   tile <- tile[!grepl('.zip',tile)]
#'   tile <- tile[!grepl('.gif',tile)]
#'   tile <- tile[!grepl('.png',tile)]
#' 
#' 
#'   current_dir <- getwd()
#'   setwd(data)
#' 
#'   utils::zip(zipfile=paste0(path,'/',name,'_mosaic.zip'),
#'       files=glob,
#'       flags = '-r9X')
#'   message('mosaic zip created')
#' 
#'   utils::zip(zipfile=paste0(path,'/',name,'_tiles.zip'),
#'       files=tile,
#'       flags = '-r9X')
#'   message('tiles zip created')
#'   if(file.exists(paste0(path,'/',name,'_tiles.zip')))
#'     {
#'     unlink(list.files('.',pattern='^[m-p][0-9]'), recursive = TRUE)
#'     }
#' 
#'   setwd(current_dir)
#' }
#' 
