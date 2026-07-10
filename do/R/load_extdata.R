#' Load external data from R package
#'
#' @param package one package name
#' @param file one file name
#'
#' @return path of data
#' @export
#'
load_extdata <- function(package,file){
    system.file('extdata',file,package = package,mustWork = TRUE)
}
