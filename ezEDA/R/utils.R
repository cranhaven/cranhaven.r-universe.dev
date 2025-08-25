#' Private utility function: given a possibly non-factor column passed as a quosure, convert into a factor
#' @param data A data frame or tibble
#' @param col_enquo A quosure
#'
#' @return A data frame or tibble with the corresponding column converted to factor if nevessary

col_to_factor <- function(data, col_enquo) {
  col_name <- rlang::as_label(col_enquo)
  if(!is.factor(data[[col_name]])) {
    data[[col_name]] <- factor(data[[col_name]])
  }
  data
}
