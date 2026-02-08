#' Construct a Dictionary
#' @description Sugar function wrapping `[Dictionary]$new`.
#' @param ... (`ANY`) \cr
#' Named arguments with names corresponding to the items to add to the
#' dictionary, where the keys are the names and the values are the
#' elements. Names must be unique.
#' @param x (`list()`) \cr
#' A named list with the names corresponding to the items to add to the
#' dictionary, where the keys are the list names and the values are the
#' list elements. Names must be unique.
#' @param types (`character()`) \cr
#' If non-NULL then `types` creates a typed dictionary in which all
#' elements of the dictionary must inherit from these `types`. Any class
#' can be given to `types` as long as there is a valid `as.character`
#' method associated with the class.
#' @examples
#' # untyped
#' dct(a = 1, b = 2, c = "a")
#'
#' # typed - class is forced
#' dct(a = 1L, b = 2L, c = 3L, types = "integer")
#'
#' # list constructor
#' dct(x = list(a = 1, b = 2, c = "a"))
#'
#' # with R6
#' d <- dct(a = 1)
#' dct(d = d)
#' @export
dct <- function(..., x = list(...), types = NULL) {
    Dictionary$new(x = x, types = types)
}