#' quiet_gglist
#'
#' Ensuring a quiet list of the grobs data are supplied to cfmDataVis
#'
#' @param x an object of class 'psc'
#' @param ... not used
#' @return A quiet list
print.quiet_gglist <- function(x, ...) {
  cat("A list of ggplot objects with names:")
  print(names(x))
}
