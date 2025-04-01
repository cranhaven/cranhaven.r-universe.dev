#' Internal comparison function for aggregation over configurations.
#'
#' @importFrom  stringi stri_detect_fixed
#'
#' @param ls List of QCA models 
#' @noRd
#'
#' @return A list counting the individual models or configurations.
mdetection_upset <- function(list, strng){
  result <- strng %in% list
  return(result)
}
