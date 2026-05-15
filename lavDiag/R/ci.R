#' Prettify confidence intervals
#'
#' Internal utility: combine estimate and CI bounds into a single formatted string.
#'
#' @param est Numeric vector of point estimates.
#' @param lower Numeric vector of lower bounds.
#' @param upper Numeric vector of upper bounds.
#' @return Character vector of formatted intervals (e.g., `"0.25 [0.10, 0.40]"`).
#' @noRd
#' @keywords internal
.ci <- function(est, lower, upper) {
  if (!is.numeric(est) || !is.numeric(lower) || !is.numeric(upper)) {
    stop("`est`, `lower`, and `upper` must all be numeric vectors")
  }
  if (!(length(est) == length(lower) && length(est) == length(upper))) {
    stop("`est`, `lower`, and `upper` must have the same length")
  }

  out <- character(length(est))
  na_idx <- is.na(est) | is.na(lower) | is.na(upper)

  out[!na_idx] <- paste0(
    .fnum(est[!na_idx]),
    " [",
    .fnum(lower[!na_idx]),
    ", ",
    .fnum(upper[!na_idx]),
    "]"
  )
  out[na_idx] <- NA_character_

  out
}
