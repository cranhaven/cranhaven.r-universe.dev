#' @keywords internal
#' @noRd

is.zero_df <- function(x) {
  do.call(cbind, lapply(x, is.zero))
}

