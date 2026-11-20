#' Reverse the points of a trajectory object and create a new trajectory
#'
#' This function reverses the order of the points in an object of class `trajectory` 
#' and then constructs a new trajectory using the reversed points. The function assumes 
#' the existence of a `trajectory` function that creates a trajectory from the given 
#' x and y coordinates, as well as a time step `delta_time`.
#'
#' @param obj An object of class `trajectory` that contains a component `points`, which is a 
#' matrix or data frame with the coordinates of the points. It is assumed that `points` has 
#' columns `x` and `y` representing the coordinates of the points in the trajectory.
#' @return A new trajectory object created using the reversed points and the original 
#' `delta_time` from `obj`.
#' @export

reverseTrajectory <- function(obj) {
	if(!is.trajectory(obj)){
		stop("'obj' is not a valid trajectory object.")
	}
  reversed_points <- obj$points[nrow(obj$points):1, ]
  trajectory(reversed_points$x, reversed_points$y, obj$delta_time)
}
