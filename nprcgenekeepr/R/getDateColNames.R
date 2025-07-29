#' Vector of date column names
#'
#' @return Vector of column names in a standardized pedigree object that are
#' dates.
#'
#' @noRd
getDateColNames <- function() {
  c("birth", "death", "departure", "exit")
}
