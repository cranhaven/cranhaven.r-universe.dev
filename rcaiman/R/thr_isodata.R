#' Compute IsoData threshold
#'
#' @description
#' Compute a threshold using the IsoData algorithm
#' \insertCite{isodata;textual}{rcaiman}, recommended by
#' \insertCite{Jonckheere2005;textual}{rcaiman}.
#'
#' @details
#' Implementation follows the IsoData method by Gabriel Landini, as implemented
#' in [autothresholdr::auto_thresh()]. Unlike that version, this function
#' accepts numeric data over an arbitrary range. `NA` values are ignored.
#'
#' @param x numeric vector or a single-column `matrix` or `data.frame` able to
#'   be coerced to numeric.
#'
#' @return Numeric vector of length one.
#' @export
#'
#' @references \insertAllCited{}
#'
#' @examples
#' caim <- read_caim()
#' thr_isodata(caim$Blue[])
thr_isodata <- function(x) {
  # Coerce matrix or data.frame to numeric
  if (is.matrix(x) | is.data.frame(x)) {
    x <- as.numeric(x)
  }
  # Validate that x is numeric
  if (!is.numeric(x)) {
    stop("`x` must be a numeric vector or coercible to numeric.")
  }
  # Remove NA values
  x <- x[!is.na(x)]
  size <- length(x)

  if (size <= 1) stop("`length(x)` must be greater than one.")
  if (size > 500) {
    message("The threshold value was computed with a subsample of size 500.")
    size <- 500
  }

  if (stats::sd(sample(x, size)) == 0) {
    thr <- x[1]
    warning("sd(x) should be greater than 0.")
  } else {
    thr <- mean(x)
    thr.back <- 0
    while (thr != thr.back) {
      thr.back <- thr
      x0 <- x[x <= thr]
      x1 <- x[x > thr]
      thr <- (mean(x0) + mean(x1)) / 2
    }
  }
  thr
}
