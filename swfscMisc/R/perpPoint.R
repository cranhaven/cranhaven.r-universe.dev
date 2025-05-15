#' @title Perpendicular Point
#' @description Compute the perpendicular point between points and a line
#'   specified by an intercept and slope.
#'
#' @param pts two column matrix of points.
#' @param line two element vector giving intercept and slope of a line.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @export
#' 
perpPoint <- function(pts, line) {
  if(is.vector(pts)) pts <- rbind(pts)
  if(is.data.frame(pts) | is.matrix(pts)) {
    pts <- as.matrix(pts)
  } else {
    stop("'pts' must be a matrix or data frame.")
  }
  if(ncol(pts) != 2) stop("'pts' must have two columns.")
  
  if(!is.vector(line)) stop("'line' must be a vector.")
  if(length(line) != 2) stop("'line' must have two elements.")
  
  intercept <- line[1]
  slope <- line[2]
  denom <- slope ^ 2 + 1
  x.term1 <- pts[, 1] + (slope * pts[, 2]) - (slope * intercept)
  x <- x.term1 / denom
  y.term1 <- pts[, 1] + (slope * pts[, 2])
  y.term2 <- (slope * y.term1) + intercept
  y <- y.term2 / denom
  cbind(perp.x = x, perp.y = y)
}
