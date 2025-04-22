## preliminary code

.onUnload <- function(libpath) library.dynam.unload("synlik", libpath)

.onLoad <- function(lib,pkg) {
   library.dynam("synlik", pkg, lib)
}
