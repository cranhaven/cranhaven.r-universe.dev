.onLoad <- function(libname, pkgname) {
  data("A", package=pkgname, envir=parent.env(environment()))
}