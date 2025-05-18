#' `[` method for `cropr_simulation`
#'
#' This method ensure keeping the `cropr_simulation` attribute when subsetting a
#' `cropr_simulation` list.
#'
#' @param x A `cropr_simulation` list
#' @param ... An index
#'
#' @return A subset of a `cropr_simulation`, keeping its attribute
#' @export
#'
#' @examples
#' path <- file.path(get_examples_path("sti"), "workspace1")
#' sim <- SticsRFiles::get_sim(workspace = path)
#' # sim returns a `cropr_simulation` list
#'
`[.cropr_simulation` <- function(x, ...) {
  l <- unclass(x)[...]
  attr_names <- names(attributes(x))
  attr_names <- attr_names[attr_names != "names"]
  attributes(l)[attr_names] <- attributes(x)[attr_names]
  return(l)
}
