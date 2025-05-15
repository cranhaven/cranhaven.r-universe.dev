#' @title Intersecting Point
#' @description Calculates the perpendicular point and distance to a line 
#'   for a series of points.
#' 
#' @param pts two element vector or two column matrix of x and y values of 
#'   points.
#' @param p1,p2 two element vectors of two points laying on line.
#' @param intercept,slope the intercept and slope of the line.
#' 
#' @return A matrix containing columns giving the x and y values of the 
#'   intersecting point on the line, and the distance to each point.
#'   
#' @note The line can be specified by providing either \code{p1} and \code{p2} 
#'   or \code{intercept} and \code{slope}. If \code{intercept} and 
#'   \code{slope} are specified, then \code{p1} and \code{p2} will be ignored.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#'
#' @examples
#' 
#' pts <- cbind(x = runif(5, 0, 10), y = runif(5, 0, 10))
#' 
#' intersectingPoint(pts, p1 = c(-1, -1), p2 = c(60, 60))
#' 
#' intersectingPoint(pts, intercept = 0, slope = 1)
#' 
#' @export
#' 
intersectingPoint <- function(pts, p1 = NULL, p2 = NULL, intercept = NULL, slope = NULL) {
  if(is.vector(pts)) {
    if(!is.numeric(pts) & length(pts) != 2) {
      stop("'pts' must be a two element numeric vector.")
    }
    pts <- rbind(pts)
  } else if(is.matrix(pts)) {
    if(!is.numeric(pts) & ncol(pts) != 2) {
      stop("'pts' must be a two column numeric matrix.")
    }
  } else stop("'pts' must be a vector or matrix.")
  
  if(is.null(intercept) | is.null(slope)) {
    if(is.null(p1) | is.null(p2)) {
      stop("if 'intercept' or 'slope' are NULL, 'p1' and 'p2' must be specified.")
    } else if((!is.numeric(p1) & length(p1) != 2) & (!is.numeric(p2) & length(p2) != 2)) {
      stop("'p1' and 'p2' must be two element numeric vectors.")
    } else {
      slope <- (p2[2] - p1[2]) / (p2[1] - p1[1])
      intercept <- p1[2] - (slope * p1[1])
    }
  } else if(!is.numeric(intercept) | !is.numeric(slope)) {
    stop("'intercept' and 'slope' must be numeric vectors.")
  }
  
  denom <- slope ^ 2 + 1
  x.term1 <- pts[, 1] + (slope * pts[, 2]) - (slope * intercept)
  x <- x.term1 / denom
  y.term1 <- pts[, 1] + (slope * pts[, 2])
  y.term2 <- (slope * y.term1) + intercept
  y <- y.term2 / denom
        
 cbind(
   intersect.x = x, 
   intersect.y = y,
   distance = sapply(1:nrow(pts), function(i) {
     sqrt((pts[i, 1] - x[i]) ^ 2 + (pts[i, 2] - y[i]) ^ 2)
   })
  )
}

