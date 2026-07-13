###############################################################################
#' Test whether a number lies within range `[a,b]`
#'
#' Default values of `a=0; b=1` allow for quick test if
#' `x` is a probability.
#'
#' @param x   values to be tested
#' @param a   lower bound (default 0)
#' @param b   upper bound (default 1)
#'
#' @return Logical vectors. `NA` values in `x` are retained.
#'
#' @author Alex Chubaty
#' @export
#' @rdname inRange
#'
#' @examples
#' set.seed(100)
#' x <- stats::rnorm(4) # -0.50219235  0.13153117 -0.07891709  0.88678481
#' inRange(x, 0, 1)
#'
inRange <- function(x, a = 0, b = 1) {
  if (is.null(x)) return(NULL) # is this desired behaviour?
  if (!is.numeric(x)) {
    if (inherits(x, c("Raster", "SpatRaster"))) {
      x <- as.vector(x[])
    } else {
      stop("x must be numeric.")
    }
  }
  if (!is.numeric(a) || !is.numeric(b)) stop("invalid (non-numeric) bounds.")
  if (is.na(a) || is.na(b)) stop("invalid (NA) bounds.")
  if (a >= b) stop("a cannot be greater than b.")
  return((x - a) * (b - x) >= 0) # nolint
  # NAs will propagate -- is this desired?
}
