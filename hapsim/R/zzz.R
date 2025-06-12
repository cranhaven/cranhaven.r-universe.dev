.onload <- function(libname, pkgname) {
  library.dynam("hapsim", pkgname, libname)
}

.onUnload <- function(libname) {
}
