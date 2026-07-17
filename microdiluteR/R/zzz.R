.onAttach <- function(libname, pkgname){
  options(microdiluteR.connection = stdin())
}