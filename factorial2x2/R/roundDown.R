
#' Round down a negative number
#'
#' Rounds a negative number to the nearest, more negative
#' number to a specified decimal place.
#' @param x  number to be rounded
#' @param dig   number of decimal places to be rounded
#' @details \code{roundDown} is used to
#' round a negative Z-statistic critical value, which has (infinitely)
#' many significant digits, to the nearest \code{dig} decimal place
#' which is more extreme
#' than the critical value itself.  This is done to preserve the
#' desired type I error level.
#' @examples
#' roundDown(-1.95456, 2)
#' # [1] -1.96
#' @export roundDown

roundDown <- function(x, dig){
  aux <- round(x, digits = dig)
  ifelse(aux <= x, result <- aux, result <- aux - 10^{-dig})
  result
}


