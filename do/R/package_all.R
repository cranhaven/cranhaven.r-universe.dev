#' Get all functions in one package
#'
#' @param x package
#'
#' @return all functions in one package
#' @export
#'
pakcage_all <- function(x){
    ls(getNamespace("foreach"), all.names=TRUE)
}