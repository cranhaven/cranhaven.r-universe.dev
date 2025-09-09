#' quiet_gtsumm
#'
#' Ensuring a quiet list of the grobs data are supplied to cfmDataVis
#'
#' @param x an object of class 'psc'
#' @param ... not used
#' @return A quiet list
#' @export
print.quiet_list <- function(x, ...) {
  cat("A list with names:")
  print(names(x))
}
