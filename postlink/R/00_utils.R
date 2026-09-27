# Generic Definitions for Internal Engines
# These generics facilitate the S3 dispatch mechanism described in the design.
# They allow the adjustment object to dictate the specific estimation routine.

#' @keywords internal
fitglm <- function(x, y, family, adjustment, control, ...) UseMethod("fitglm", adjustment)

#' @keywords internal
fitcoxph <- function(x, y, adjustment, control, ...) UseMethod("fitcoxph", adjustment)

#' @keywords internal
fitctable <- function(ftable, adjustment, control, ...) UseMethod("fitctable", adjustment)

#' @keywords internal
fitsurvreg <- function(x, y, dist, adjustment, control, ...) UseMethod("fitsurvreg", adjustment)
