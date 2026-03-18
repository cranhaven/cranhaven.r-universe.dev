#' .onAttach
#'
#' @param libname character
#' @param pkgname character
#'
#' @references https://r-pkgs.org/r.html
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("IMPORTANT: Please note that functions in this package will soon be deprecated and replaced by functions in package boot.heterogeneity.")
  }
