#' Calculates the total distance traveled through a series of points.
#'
#' Computes the cumulative distance between consecutive points in a given set of coordinates. It sums the Euclidean distances between each pair of adjacent 
#' points, providing the total distance traveled along the path defined by the points.
#'
#' @param data An object of class `trajectory` containing a collection of points with coordinates.
#'
#' @return A numeric value representing the total distance traveled through the points.
#'
#' @export

distanceTraveled <- function(data) {
	if(!is.trajectory(data)){
    stop("'data' is not a valid trajectory object.")
  }
  n <- nrow(data$points)
  sumDistance <- 0

  for (i in 1:(n - 1)) {
    distance <- sqrt(sum((data$points[i, ] - data$points[i + 1, ])^2))
    sumDistance <- sumDistance + distance
  }
  return(sumDistance)
}
