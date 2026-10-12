# On load
#===================
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Package DTDAcif. SiDOR Group. University of Vigo")
}


.onUnload <- function (libpath) {
  library.dynam.unload("DTDA.cif", libpath)
}
