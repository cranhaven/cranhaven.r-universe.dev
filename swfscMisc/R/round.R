#' @title Rounding Numbers for Data Frames
#' @description Rounds numeric columns in data.frames
#'
#' @param x a data.frame with numeric columns.
#' @param digits integer indicating the number of decimal places (\code{round})
#'   or significant digits (\code{signif}) to be used. See \code{\link[base]{round}} for 
#'   more details.
#' @param ... arguments to be passed to methods.
#'
#' @details Takes a data.frame and returns a data.frame with the specified function 
#'   applied to each numeric column.
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @seealso \code{\link[base]{Round}}
#'
#' @examples
#' data(mtcars)
#' 
#' round(mtcars, 0)
#' 
#' signif(mtcars, 2)
#'
#' @name round
#' @aliases ceiling floor trunc round signif
#' 
NULL

#' @rdname round
#' @export
#' 
ceiling.data.frame <- function(x) {
  for(i in 1:ncol(x)) {
    if(is.numeric(x[[i]])) x[[i]] <- ceiling(x[[i]])
  }
  x
}

#' @rdname round
#' @export
#' 
floor.data.frame <- function(x) {
  for(i in 1:ncol(x)) {
    if(is.numeric(x[[i]])) x[[i]] <- floor(x[[i]])
  }
  x
}

#' @rdname round
#' @export
#' 
trunc.data.frame <- function(x, ...) {
  for(i in 1:ncol(x)) {
    if(is.numeric(x[[i]])) x[[i]] <- ceiling(x[[i]], ...)
  }
  x
}

#' @rdname round
#' @export
#' 
round.data.frame <- function(x, digits = 0) {
  for(i in 1:ncol(x)) {
    if(is.numeric(x[[i]])) x[[i]] <- round(x[[i]], digits = digits)
  }
  x
}

#' @rdname round
#' @export
#' 
signif.data.frame <- function(x, digits = 6) {
  for(i in 1:ncol(x)) {
    if(is.numeric(x[[i]])) x[[i]] <- signif(x[[i]], digits = digits)
  }
  x
}