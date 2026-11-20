#' Trim a trajectory object to a subset of points
#'
#' This function trims the points of a `trajectory` object by selecting a subset of the points 
#' from the specified `from` to `to` indices. It then creates a new trajectory using the selected 
#' points.
#'
#' @param obj An object of class `trajectory` that contains a component `points`, which is a 
#' data frame with the coordinates of the points in the trajectory.
#' @param from A positive integer indicating the starting index of the points to include. Default is 1.
#' @param to A positive integer indicating the ending index of the points to include. Default is 
#' the total number of rows in `obj$points`.
#' @return A new trajectory object created using the subset of points from the `from` to `to` indices.
#' @export

trimTrajectory <- function(obj, from = 1, to = nrow(obj$points)) {
	if(!is.trajectory(obj)){
		stop("'obj' is not a valid trajectory object.")
	}
  if (from < 1 || to > nrow(obj$points) || from > to) {
    stop("Invalid indices for trimming.")
  }
  sub_points <- obj$points[from:to, ]
  trajectory(sub_points$x, sub_points$y, obj$delta_time)
}