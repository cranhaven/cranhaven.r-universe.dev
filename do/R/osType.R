#' operation system
#'
#' @return logical
#' @export
#'
#' @examples
#' is.windows()
is.windows <- function(){
    tolower(Sys.info()["sysname"]) == 'windows'
}
#' operation system
#'
#' @return logical
#' @export
#'
#' @examples
#' is.mac()
is.mac <- function(){
    right_equal(tolower(Sys.info()["sysname"]),'darwin')
}
#' operation system
#'
#' @return logical
#' @export
#'
#' @examples
#' is.linux()
is.linux <- function(){
    right_equal(tolower(Sys.info()["sysname"]),'linux')
}