
#' @importFrom tools package_dependencies
#' @importFrom utils installed.packages
#' @export 
#' @rdname library_with_dep
#' 
require_with_dep <-
function(package, 
         lib.loc = NULL,
         quietly = FALSE,
         warn.conflicts = TRUE, 
         character.only = FALSE, 
         which = "Imports",
         recursive = FALSE, 
         reverse = FALSE, 
         verbose = getOption("verbose"))
{
  if (!character.only) {
    package <- as.character(substitute(package))
  }
  
  ip <- utils::installed.packages()
  pd <- tools::package_dependencies(package, 
                                    ip, 
                                    which = which, 
                                    recursive = recursive, 
                                    reverse = reverse, 
                                    verbose = verbose)
  pd <- pd[[package]]
  for (p in c(package, pd)) {
    require(p, 
            lib.loc = lib.loc, 
            quietly = quietly,
            warn.conflicts = warn.conflicts, 
            character.only = TRUE)
  }
  invisible(NULL)
}
