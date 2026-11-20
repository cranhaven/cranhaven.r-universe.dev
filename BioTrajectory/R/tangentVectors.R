#' Calculate the tangent cectors of a trajectory object
#'
#' This function calculates the unit tangent vectors at each point of the trajectory represented 
#' by a `trajectory` object. The tangent vectors are computed by taking the difference between 
#' consecutive points and normalizing them to have unit length.
#'
#' @param obj An object of class `trajectory` that contains a component `points`.
#' @return A matrix where each row is a unit tangent vector corresponding to each pair of 
#' consecutive points in `obj$points`.
#' @export

tangentVectors <- function(obj) {
	if(!is.trajectory(obj)){
		stop("'obj' is not a valid trajectory object.")
	}
  coords <- as.matrix(obj$points)
  diffs <- diff(coords)
  norms <- sqrt(rowSums(diffs^2))
  tangents <- diffs / norms
  return(tangents)
}