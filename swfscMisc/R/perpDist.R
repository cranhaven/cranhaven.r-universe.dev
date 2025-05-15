#' @title Perpendicular Distance
#' @description Calculate the perpendicular distance of a matrix 
#'   of points to a line.
#'
#' @param pts two column matrix of points.
#' @param line either a 2x2 matrix of points defining line or
#'   two element vector giving intercept and slope of line.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' ran.pts <- matrix(runif(10), ncol = 2)
#' x <- perpDist(ran.pts, c(0, 1))
#' x
#' 
#' plot.new()
#' plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
#' abline(a = 0, b = 1)
#' points(ran.pts[, 1], ran.pts[, 2])
#' segments(ran.pts[, 1], ran.pts[, 2], x[, 1], x[, 2], lty = "dashed")
#' points(x[, 1], x[, 2], col = "red")
#' axis(1, pos = 0)
#' axis(2, pos = 0)
#' 
#' @export
#' 
perpDist <- function(pts, line) {
  if(is.vector(pts)) pts <- rbind(pts)
  if(is.data.frame(pts) | is.matrix(pts)) {
    pts <- as.matrix(pts)
  } else {
    stop("'pts' must be a matrix or data frame.")
  }
  if(ncol(pts) != 2) stop("'pts' must have two columns.")
  
  line <- if(is.vector(line)) {
    if(length(line) != 2) {
      stop("if 'line' is a vector, it must have two elements.")
    }
    c(intercept = line[1], slope = line[2])
  } else if(is.data.frame(line) | is.matrix(line)) {
    line <- as.matrix(line)
    if(ncol(line) != 2) {
      stop("if 'line' is a matrix or data frame, it must have two columns.")
    }
    if(nrow(line) != 2) {
      stop("if 'line' is a matrix or data frame, it must have two rows.")
    }
    slope <- diff(line[, 1]) / diff(line[, 2])
    c(intercept = line[1, 2] - (slope * line[1, 1]), slope = slope)
  } else stop("'line' must be a vector, matrix, or data frame.")
  
  intersect.pts <- perpPoint(pts, line)
  
  line.dist <- sapply(1:nrow(pts), function(i) {
    sqrt(sum((pts[i, ] - intersect.pts[i, ]) ^ 2))
  })
  
  cbind(intersect.pts, line.dist = line.dist)
}