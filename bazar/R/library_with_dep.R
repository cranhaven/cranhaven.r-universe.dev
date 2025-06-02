#' @title 
#' Loading/Attaching and listing of packages with dependencies
#' 
#' @description 
#' \code{library_with_dep} and \code{require_with_dep} behave 
#' respectively like \code{\link[base]{library}} and 
#' \code{\link[base]{require}}, but also load and attach 
#' dependent packages (typically packages listed in the \code{Imports} 
#' field of the \code{DESCRIPTION} file). 
#' 
#' @param package
#' the name of a package, given as a name or literal character string, 
#' or a character string, depending on whether character.only is 
#' \code{FALSE} (default) or \code{TRUE}.
#' 
#' @param help
#' the name of a package, given as a name or literal character string, 
#' or a character string, depending on whether character.only is 
#' \code{FALSE} (default) or \code{TRUE}.
#' 
#' @param pos
#' the position on the search list at which to attach 
#' the loaded namespace. 
#' Can also be the name of a position on the current 
#' search list as given by \code{\link[base]{search}}().
#' 
#' @param lib.loc
#' character. A vector describing the location of R 
#' library trees to search through, or \code{NULL}. 
#' The default value of \code{NULL} corresponds to all libraries 
#' currently known to \code{\link[base]{.libPaths}}(). 
#' Non-existent library trees are silently ignored.
#' 
#' @param character.only
#' logical. Indicates whether \code{package} or \code{help} 
#' can be assumed to be character strings.
#' 
#' @param logical.return
#' logical. If it is \code{TRUE}, then \code{FALSE} or \code{TRUE} 
#' is returned to indicate success.
#' 
#' @param warn.conflicts
#' logical. If \code{TRUE}, warnings are printed about 
#' conflicts from attaching the new package. 
#' A conflict is a function masking a function, 
#' or a non-function masking a non-function.
#' 
#' @param quietly
#' logical. If \code{TRUE}, no message confirming package 
#' attaching is printed, and most often, 
#' no errors/warnings are printed if package attaching fails.
#' 
#' @param verbose
#' logical. If \code{TRUE}, additional diagnostics are printed.
#' 
#' @param which
#' character. A vector listing the types of dependencies, 
#' a subset of \code{c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")}. 
#' Character string \code{"all"} is shorthand for that vector, 
#' character string \code{"most"} for the same vector without \code{"Enhances"}.
#' 
#' @param recursive
#' logical. Should (reverse) dependencies of (reverse) 
#' dependencies (and so on) be included?
#' 
#' @param reverse
#' logical. If \code{FALSE} (default), 
#' regular dependencies are calculated, otherwise reverse dependencies.
#' 
#' @importFrom tools package_dependencies
#' @importFrom utils installed.packages
#' @export 
#' 
#' @seealso \code{\link[base]{library}} and 
#' \code{\link[base]{require}} from package \pkg{base}; 
#' \code{\link[tools]{package_dependencies}} from \pkg{tools}; 
#' \code{\link[utils]{installed.packages}} from \pkg{utils}. 
#' 
library_with_dep <-
function(package, 
         help, 
         pos = 2, 
         lib.loc = NULL,
         character.only = FALSE, 
         logical.return = FALSE,
         warn.conflicts = TRUE, 
         quietly = FALSE,
         verbose = getOption("verbose"), 
         which = "Imports",
         recursive = FALSE, 
         reverse = FALSE)
{
  if (!character.only) {
    package <- as.character(substitute(package))
    help <- as.character(substitute(help))
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
    library(p, 
            help = help, 
            pos = pos, 
            lib.loc = lib.loc, 
            character.only = TRUE, 
            logical.return = logical.return,
            warn.conflicts = warn.conflicts, 
            quietly = quietly,
            verbose = verbose)
  }
  invisible(NULL)
}
