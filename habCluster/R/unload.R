.onUnload <- function (libpath) {
  library.dynam.unload("habCluster", libpath)
}
