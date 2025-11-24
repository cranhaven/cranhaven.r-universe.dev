#' Check which level the input data/vector is
#'
#' The input level is calculated as the mode (most common) hierarchical
#' level in the input vector.
#'
#' @param x - vector of character
#' @param klass_data - - the right formatting to the classification levels
#' @return The hierarchical level of the input data is returned.
#' @keywords internal
levelCheck <- function(x, klass_data) {
  m <- match(x, klass_data$code)
  tab <- table(klass_data[m, ]$level)
  input_level <- as.numeric(names(tab)[which.max(tab)])

  if (is.na(input_level)) {
    stop("Cannot find an input level, please check the input vector")
  }
  return(input_level)
}
