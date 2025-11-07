#' @title Style
#'
#' @description
#' Generic style method used for overriding to
#' get style information from various logging objects.
#'
#' @param obj object to extract value from.
#'
#' @return object's value
#' @export
style <- function(obj) {
  UseMethod("style", obj)
}

#' @title Value
#'
#' @description
#' Base method for getting the value of a
#' format object.
#'
#' @param obj object to extract value from.
#' @param ... further arguments passed to or from other methods.
#'
#' @return object's value
#' @export
value <- function(obj, ...) {
  UseMethod("value", obj)
}
