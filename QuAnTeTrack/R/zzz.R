.onLoad <- function(libname, pkgname) {
  utils::data("MountTom", package = pkgname, envir = parent.env(environment()))
  utils::data("PaluxyRiver", package = pkgname, envir = parent.env(environment()))
}
