#' Normalize data using min-max rescaling
#'
#' @description
#' Rescale numeric or raster data from an expected range to the range \eqn{[0, 1]}.
#'
#' @param r numeric [terra::SpatRaster-class] or numeric vector. Input data.
#' @param mn numeric vector of length one or `NULL`. Minimum expected value.
#'   If `NULL` (default), uses the minimum of `r`.
#' @param mx numeric vector of length one or `NULL`. Maximum expected value.
#'   If `NULL` (default), uses the maximum of `r`.
#' @param clip logical vector of length one. If `TRUE`, clip the output to
#'   \eqn{[0, 1]} after rescaling. If `FALSE`, values greater than `mx` are scaled
#'   proportionally to values above `1`, and values less than `mn` to values
#'   below `0`.
#'
#' @return Same properties as `r`, with values rescaled to the range \eqn{[0, 1]} if
#'   `mn` and `mx` match the range of `r` or extend beyond it. If `clip = TRUE`,
#'   values will be within \eqn{[0, 1]} even if this implies data loss.
#'
#' @export
#'
#' @examples
#' normalize_minmax(read_caim())
normalize_minmax <- function(r, mn = NULL, mx = NULL, clip = FALSE) {
  handling_r <- c(
    tryCatch(.check_vector(r, "numeric", sign = "any"),
             error = function(e) FALSE),
    tryCatch(.assert_spatraster(r),
             error = function(e) FALSE)
  )
  if (!any(handling_r)) {
    stop("`r` must be either numeric vector or SpatRaster class.")
  }
  .check_vector(mn, "numeric", 1, allow_null = TRUE, sign = "any")
  .check_vector(mx, "numeric", 1, allow_null = TRUE, sign = "any")
  .check_vector(clip, "logical", 1)

  if (is.null(mn)) mn <- .get_min(r)
  if (is.null(mx)) mx <- .get_max(r)

  if (mx == mn) {
    warning("The values 'mn' and 'mx' are equal. Output will be constant or undefined due to division by zero.")
  }

  r <- (r - mn) / (mx - mn)
  if (clip) {
    r[r < 0] <- 0
    r[r > 1] <- 1
  }
  r
}
