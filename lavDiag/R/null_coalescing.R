#' Null-coalescing operator (internal)
#'
#' Returns `y` when `x` is `NULL`, otherwise returns `x`.
#'
#' @keywords internal
#' @noRd
#' @usage x %||% y
#' @param x Any object that may be `NULL`.
#' @param y Fallback value used only when `x` is `NULL`.
#' @return `x` if not `NULL`, otherwise `y`.
#'
#' @examples
#' NULL %||% 1  # returns 1
#' 10   %||% 1  # returns 10
`%||%` <- function(x, y) {
  # Only evaluate `y` if needed due to lazy evaluation
  if (is.null(x)) y else x
}
