#' @title Get help on a package
#'
#' @description
#' \code{phelp} provides help on an installed package.
#'
#' @details
#' This function provides help on an installed package. The
#' package does not have to be loaded. The package name does
#' not need to be entered with quotes.
#'
#' @param pckg The name of a package
#' @export
#' @return 
#' No return value, called for side effects.
#' @examples
#' phelp(stats)
#'
phelp <- function(pckg) {
  utils::help(package=deparse(substitute(pckg)))
}
