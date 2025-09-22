#' Relative Standard Deviation
#'
#' @param values a `numeric` vector of values.
#' @noRd
rsd <- function(values) {
  ifelse(mean(values) != 0, (sd(values) / mean(values)), NA_real_)
}
