#' @keywords internal
"_PACKAGE"

#' @useDynLib llamaR, .registration = TRUE
#' @import ggmlR
NULL

.onLoad <- function(libname, pkgname) {
    # Set default verbosity to errors only (quiet mode)
    # Users can change with llama_set_verbosity()
    .Call("r_llama_set_verbosity", 1L)
}

.onUnload <- function(libpath) {
    library.dynam.unload("llamaR", libpath)
}
