#' @importFrom pingr is_online
#' @importFrom pingr is_up
NULL

.onLoad <- function(libname, pkgname) {

  Sys.unsetenv("HTTP_PROXY")

  fourier_is_up <- pingr::is_up("www-fourier.univ-grenoble-alpes.fr")
  no_giac       <- !.giac_is_installed()

  if(fourier_is_up && no_giac) {
    .giac_download()
  }

}
