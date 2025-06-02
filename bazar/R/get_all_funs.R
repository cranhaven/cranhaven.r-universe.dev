#' @title 
#' Functions exported by a package
#' 
#' @description 
#' \code{get_all_funs} provides all the functions exported by a given 
#' installed package. 
#' 
#' @param pkg
#' character. The package of interest. (Must be installed already.)
#' 
#' @return 
#' A character vector, the functions exported. 
#' 
#' @export
#' 
#' @examples 
#' get_all_funs("stats")
#' 
get_all_funs <- 
function(pkg)
{
  stopifnot(length(pkg) == 1L)
  
  if (pkg %in% c("Deducer", "DeducerExtra", "gstat", "rattle", 
                 "rggobi", "RGtk2", "spacetime", "translations")) {
    warning(paste0("get_all_funs() returns 'character(0)' for package ", pkg), 
            call. = FALSE)
    return(character(0L))
  }
  tryCatch(suppressWarnings(getNamespaceExports(pkg)), 
           error = function(e) character(0L))
  #unclass(lsf.str(envir = asNamespace(pkg), all = TRUE))
}
