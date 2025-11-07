
#' @title Download lake data from internet
#'
#' @description
#' Check that we have local cache of ADK lake data.
#' If it is not locally available, download the data
#' from the internet and prepare it for local use. This only
#' needs to be run once for each install of the package. Note:
#' you will be required to re-download data when a new version of
#' the package is released. This ensures stale data are not being
#' accidentally used.
#' 
#' @import httr
#' @import rappdirs
#' @import utils
#'
#'
#' @export
check_dl_data = function(){
  #download and 
  res = check_dl_file(system.file("extdata/master.csv", package = "adklakedata"), md5check = TRUE)
  if(any(res == 'download')){
    #if download occurred, re-unzip package 
    unzip(zipfile = file.path(local_path(), 'adk_lake_data.zip'), exdir = local_path(), overwrite=TRUE)
  }else if(any(res == 'error')){
    stop('Problem while downloading data files. Please try again later or submit an error report:\n http://github.com/lawinslow/adklakedata/issues')
  }
}

#' @title Set custom local file path
#' 
#' @description 
#' Data files are locally cached (they are too large to be distributed
#' with the CRAN package). These cached files are stored in your user
#' data directory, or a custom directory set using \code{set_local_path}. 
#' 
#' @param path Full path to custom folder, will be created if it doesn't exist. 
#' 
#' 
#' @examples 
#' # set custom path to local temp directory
#' set_local_path(tempdir())
#' 
#' 
#' @export
set_local_path = function(path){
  env$cust_local_path = path
}


#' @title Get local file path
#' 
#' @description 
#' Data files are locally cached (they are too large to be distributed
#' with the CRAN package). These cached files are stored in your user
#' data directory, or a custom directory set using \code{set_local_path}. 
#' 
#' @return Path to local file cache location
#' 
#' 
#' @examples 
#' # set custom path to local temp directory
#' set_local_path(tempdir())
#' 
#' #returns current local path directory
#' local_path()
#' 
#' @import rappdirs
#' 
#' 
#' @export
local_path = function(){
  if(is.null(env$cust_local_path)){
    path = user_data_dir(appname = 'adklakedata')
  }else{
    path = env$cust_local_path
  }
  
  if(!file.exists(path)){
    dir.create(path, recursive = TRUE)
  }
  return(path)
}

