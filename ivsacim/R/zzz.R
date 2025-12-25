#' @keywords internal
.onAttach <- function(libname, pkgname){
  msg <- paste("Thank you for using our package. The part concerning invalid IV in the package in still in progress and we only support point estimation.")
  packageStartupMessage(msg)
  msg <- paste0('We politely ask you to cite our package using: citation("', pkgname, '"). It will be appreciated. ')
  packageStartupMessage(msg)
}