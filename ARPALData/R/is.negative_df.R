#' @keywords internal
#' @noRd

is.negative_df <- function(x) {
  do.call(cbind, lapply(x, is.negative))
}
