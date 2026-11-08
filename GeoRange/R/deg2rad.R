#' Converts degrees to radians
#' @param deg - decimal degree to be converted to radians
#' @return Returns the degree in radians
#' @examples
#' deg2rad(45)
#' @references
#' [1]Originally from http://www.r-bloggers.com/great-circle-distance-calculations-in-r/
#' @export
deg2rad <- function(deg) return(deg*pi/180)
