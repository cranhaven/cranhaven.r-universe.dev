#' up level directory
#'
#' @param dir present directory or file path
#' @param end.slash logical
#'
#' @return upper directory
#' @export
#'
upper.dir <- function(dir,end.slash=TRUE){
    (dir <- formal_dir(dir,TRUE))
    last_slash <- stringr::str_locate_all(dir,'/')[[1]][,1] |> last(2)
    if (length(last_slash)==0 | is.na(last_slash)) return(dir)
    left(dir,last_slash) |> formal_dir(end.slash)
}

#' formal directory
#'
#' @param dir one directory
#' @param end.slash logical
#' @return formed directory
#' @export
#'
formal_dir <- function(dir,end.slash=FALSE){
    if (end.slash){
        Trim_right(dir,c('\\\\','/')) |> 
            Replace('\\\\','/') |> 
            paste0('/')
    }else{
        Trim_right(dir,c('\\\\','/')) |> 
            Replace('\\\\','/')
        
    }
}