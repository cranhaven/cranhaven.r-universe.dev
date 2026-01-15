passShowAttributes <- function(source, target) {
  showOpts <- attr(source, "showOpts")
  inheritable <- isTRUE(attr(showOpts, "inheritable")) ||
    isTRUE(numberOfVariables(source) >= numberOfVariables(target))
  if(inheritable) {
    attr(target, "showOpts") <- showOpts
  }
  target
}

#' @title (internal) Make a 'ratioOfQsprays' object from a list
#' @description This function is for internal usage. It is exported because
#'   it is also used for internal usage in other packages.
#'
#' @param x list returned by the Rcpp function
#'   \code{returnRatioOfQsprays}
#'
#' @return A \code{ratioOfQsprays} object.
#' @export
#' @importFrom qspray qspray_from_list
ratioOfQsprays_from_list <- function(x) {
  new(
    "ratioOfQsprays",
    numerator   = qspray_from_list(x[["numerator"]]),
    denominator = qspray_from_list(x[["denominator"]])
  )
}

`%||%` <- function(x, y) {
  if(is.null(x)) y else x
}

isInteger <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && as.integer(x) == x
}

isPositiveInteger <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && floor(x) == x
}

isNonnegativeInteger <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && floor(x) == x && x != 0
}

#' @importFrom utils head
#' @noRd
removeTrailingZeros <- function(x) {
  n <- length(x)
  while(x[n] == 0 && n > 0L) {
    n <- n - 1L
  }
  head(x, n)
}

isNamedList <- function (x) {
  is.list(x) && length(names(x)) == length(x)
}
