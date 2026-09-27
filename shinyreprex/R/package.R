#' Enable usage of `<S7_object>@@name` in package code
#'
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
#' @noRd
NULL

.onLoad <- function(libname, pkgname) {
  S7::methods_register()
}
