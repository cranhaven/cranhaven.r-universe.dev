


#' @title Source All R Files under a Directory
#' 
#' @description
#' \link[base]{source} all `*.R` and `*.r` files under a directory.
#' 
#' @param path \link[base]{character} scalar, parent directory of `.R` files
#' 
#' @param ... additional parameters of \link[base]{source}
#' 
#' @returns 
#' Function [sourcePath] does not have a returned value
#' 
#' @export
sourcePath <- function(path, ...) {
  fs <- list.files(path = path, pattern = '\\.R$|\\.r$', full.names = TRUE)
  lapply(fs, FUN = source, ...)
  return(invisible())
}