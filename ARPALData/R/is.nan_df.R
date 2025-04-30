#' @keywords internal
#' @noRd

is.nan_df <- function(x) {
  do.call(cbind, lapply(x, is.nan))
}

