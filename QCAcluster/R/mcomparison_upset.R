#' Comparison of matrices preparing data for plotting
#'
#' @param x List of QCA solutions or configurations
#' derived from multiple truth table analyses performed
#' with \code{\link{QCA}} package
#' @param y QCA solutions as produced
#' with \code{\link[QCA]{minimize}} from \code{\link{QCA}}
#' package
#' @param num Check for numeric input. Default
#' is set to \code{FALSE}. This setting ensures that the function
#' will read data in a numeric format even it is in a
#' different format.
#' @noRd
#'
#' @return A dataframe counting the individual solutions
#' or configurations.
mcomparison_upset <- function(x = all_values, y, num = F) {
  temp <- x %in% y
  
  if (num) {
    return(as.numeric(temp))
  } else {
    return(temp)
  }
}
