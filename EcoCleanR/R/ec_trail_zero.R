#' Trail Zeros from the Coordinate Values
#' @param coord A coordinate value in the numeric format of decimal degree
#'
#' @return A numerical trailed coordinate value.

#' @examples
#' ec_trail_zero(12.7000000)
#' ec_trail_zero(45.000000)
#' @export

ec_trail_zero <- function(coord) {
  coord_num <- as.numeric(coord)
  coord_cleaned <- sub("0+$", "", coord) # Remove trailing zeros
  coord_cleaned <- sub("\\.$", "", coord_cleaned) # Remove decimal point if it's the last character
  return(coord_cleaned)
}
