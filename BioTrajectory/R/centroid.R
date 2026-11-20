#' Calculate the Centroid of Object
#'
#' This function calculates the centroid of a set of points contained in an object of class 
#' `trajectory`. The centroid is the average of the coordinates of all the points.
#'
#' @param obj An object of class `trajectory` that must contain a component named `points`.
#' @return A numeric vector containing the coordinates of the centroid of the points stored in `obj$points`.
#' @export

centroid <- function(obj) {
  if(!is.trajectory(obj)){
		stop("'obj' is not a valid trajectory object.")
	}
  colMeans(obj$points)
}