.onLoad <- function(libname, pkgname) {
  # Force load the dataset
  utils::data("fennica_subset", package = pkgname, envir = parent.env(environment()))
}
