#' @keywords internal
#' @noRd

is.infinite_df <- function(x) {
  do.call(cbind, lapply(x, is.infinite))
}
