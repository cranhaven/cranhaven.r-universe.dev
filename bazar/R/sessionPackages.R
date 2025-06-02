#' @title
#' Shows packages attached to the current R session
#'
#' @description
#' The function \code{sessionPackages} prints the list
#' of packages attached to the current R session.
#' 
#' @details 
#' This function reuses part of the code from 
#' \code{\link[utils]{sessionInfo}}. 
#' 
#' @param package
#' a character vector naming installed packages,
#' or \code{NULL} (the default) meaning all attached packages.
#'
#' @return
#' A list with the following components:
#' \itemize{
#'   \item \code{basePkgs}: a character vector of base packages which are attached.
#'   \item \code{otherPkgs} (not always present): a character vector of other attached packages.
#' }
#'
#' @seealso \code{\link{sessionInfo}} from package \pkg{utils}, 
#' \code{\link{R.version}} from package \pkg{base}. 
#'
#' @importFrom utils packageDescription
#' @export
#'
#' @examples
#' sessionPackages()
#'
sessionPackages <-
function(package = NULL)
{
  z <- list()
  if (is.null(package)) {
    package <- grep("^package:", search(), value = TRUE)
    keep <- vapply(package, 
                   FUN = function(x) x == "package:base" ||
                     !is.null(attr(as.environment(x), "path")), 
                   FUN.VALUE = logical(1L))
    package <- sub("^package:", "", package[keep])
  }
  pkgDesc <- lapply(package, FUN = utils::packageDescription)
  if (length(package) == 0)
    stop("no valid packages were specified")
  basePkgs <- vapply(pkgDesc, 
                     FUN = function(x) !is.null(x$Priority) &&
                       x$Priority == "base", 
                     FUN.VALUE = logical(1L))
  z$basePkgs <- package[basePkgs]
  if (any(!basePkgs)) {
    z$otherPkgs <-  package[!basePkgs]
  }
  z
}
