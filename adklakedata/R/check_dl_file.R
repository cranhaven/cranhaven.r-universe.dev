#' @title Verify and download data files
#' 
#' @description 
#' Checks if local data files as defined in master file exist and match MD5 hash. Downloads data if necessary.
#' 
#' @param master_file Character path to master file
#' @param fname Character vector of specific file names to check
#' @param md5check boolean
#' @param dest Character path to download destination
#' 
#' @import httr
#' @import tools
#' @export
check_dl_file = function(master_file, fname = NULL, md5check = TRUE, dest=local_path()){
  files = read.csv(master_file)
  if(!is.null(fname)){
   files = files[files$filename == fname,]
  }
  res = apply(files, 1, function(x){
    
    .check_dl_file(furl=x["URL"], dest=file.path(dest,x["filename"]), md5=x["MD5"], md5check = md5check)
    
  })
  
  return(res)
}

#' @title Internal file download and check function
#' 
#' @description Download check logic for individual file. 
#' Returns status to indicate success or failure of file download. 
#' 
#' @return Status codes are 
#' \tabular{ll}{
#' \strong{code} \tab \strong{meaning} \cr
#' error \tab Failure, download or md5 check problem \cr
#' cache \tab Local cached file used\cr
#' download \tab Downloaded and checked file successfully\cr
#' }
#' 
#' @keywords internal
#' @noRd
.check_dl_file = function(furl, dest, md5, md5check=FALSE){

  if(file.exists(dest) & !md5check){
    return('cache')
    
  }else if(file.exists(dest) & md5check){
    if(md5sum(dest) != md5){
      r = RETRY("GET", url=furl, write_disk(dest, overwrite=TRUE), times = 2, progress())
      stop_for_status(r)
      if(md5sum(dest) == md5){
        return('download')
      }else{
        return('error')
      }
    }else{
      return('cache')
    }

  }else if(!file.exists(dest)){
    r = RETRY("GET", url=furl, write_disk(dest, overwrite=TRUE), times = 2, progress())
    stop_for_status(r)
    if(md5sum(dest) == md5){
      return('download')
    }else{
      return('error')
    }
  }else{
    return('error')
  }
}
