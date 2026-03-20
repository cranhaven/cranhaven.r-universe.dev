#' Package initialization
#'
#' @useDynLib fdars, .registration = TRUE
#' @importFrom methods is
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Package initialization code
}

.onUnload <- function(libpath) {
  library.dynam.unload("fdars", libpath)
}
