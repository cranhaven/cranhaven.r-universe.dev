#' Get Decimal Places of Coordinate Values
#'
#' @param coord A coordinate value in the numeric format of decimal degree
#'
#' @return a numerical value which represent the number of decimal places for the coordiante
#' @examples
#' decimal_places(12.7000000)
#' decimal_places(45.67788)
#'
#' @export
#'
decimal_places <- function(coord) {
  if (!is.numeric(coord)) stop("Input must be a numeric vector.")

  coord_no_trail <- sapply(coord, function(x) {
    if (!is.na(x)) {
      ec_trail_zero(x)
    } else {
      NA
    }
  })

  count_decimal_places <- function(coord) {
    if (is.na(coord)) {
      return(NA)
    }
    coord_str <- as.character(coord)
    if (grepl("\\.", coord_str)) {
      return(nchar(sub(".*\\.", "", coord_str)))
    } else {
      return(0)
    }
  }

  dec_places <- sapply(coord_no_trail, count_decimal_places)
  return(dec_places)
}
