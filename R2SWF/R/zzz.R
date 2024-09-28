.onLoad <- function(libname, pkgname) {
    # library.dynam("R2SWF", pkgname, libname);
    .Call(swfInit);
}

.onUnload <- function(libpath) {
    .C(Ming_collectGarbage);
    library.dynam.unload("R2SWF", libpath);
}

