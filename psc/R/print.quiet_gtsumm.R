#' quiet_gtsumm
#'
#' Ensuring a quiet list of the grobs data are supplied to cfmDataVis
#'
#' @param x an object of class 'psc'
#' @param ... not used
#' @return A quiet list
#' @export
print.quiet_gtsumm <- function(x, ...) {
  cat("A Summary Table")
  print(names(x))
}
