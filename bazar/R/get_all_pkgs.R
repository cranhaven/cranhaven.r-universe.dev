#' @title 
#' Packages exporting a function 
#' 
#' @description 
#' \code{get_all_pkgs} provides all packages (belonging to a given list of 
#' packages) exported by a given function.
#' 
#' @param fun
#' function or character. The function of interest.
#' 
#' @param packages
#' The packages to look into. If \code{NULL}, the list of currently attached  
#' packages is explored. 
#' 
#' @return 
#' A character vector, the packages. 
#' 
#' @export
#' 
#' @examples 
#' \dontrun{
#' get_all_pkgs("as.fun")
#' get_all_pkgs(as.fun)
#' get_all_pkgs("stats::median")
#' }
#' 
get_all_pkgs <- 
function(fun, 
         packages = NULL)
{
  test <- tryCatch(is.character(fun), error = function(e) FALSE)
  if (!test) {
    fun <- deparse(substitute(fun))
  }
  
  fun <- fun[1L]
  if (grepl(":::", fun)) {
    parts <- strsplit(fun, ":::")[[1L]]
    pkgs <- parts[1L]
  } else if (grepl("::", fun)) {
    parts <- strsplit(fun, "::")[[1L]]
    pkgs <- parts[1L]
  } else {
    if (is.null(packages)) packages <- .packages(all.available = FALSE)
    w <- which(vapply(packages, 
                      FUN = function(pkg) { fun %in% get_all_funs(pkg) }, 
                      FUN.VALUE = logical(1L)))
    pkgs <- packages[w]
  }
  
  as.vector(pkgs)
}
