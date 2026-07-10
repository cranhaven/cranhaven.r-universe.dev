#' Detach package
#'
#' @param x one package name, if missing, detach all packages
#'
#' @return detach one package
#' @export
#'
unlibray <- function(x){
    if (!missing(x)){
        stg=paste0('detach("package:',x,'", unload = TRUE)')
        eval(parse(text = stg))
        message('detach ',x)
    }else{
        detach()
    }
    
}
#' Remove all objects
#'
#' @return empty object
#' @export
#'
rm_all <- function(){
    rm(list = ls(envir = .GlobalEnv),envir = .GlobalEnv)
}