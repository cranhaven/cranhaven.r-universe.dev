#' Subsample the points of a trajectory object and create a new trajectory
#'
#' This function subsamples the points of a `trajectory` object by selecting every `step`-th 
#' point. It then creates a new trajectory using the selected points, adjusting the `delta_time` 
#' accordingly.
#'
#' @param obj An object of class `trajectory` that contains a component `points`. The `points` component must have columns `x` 
#' and `y` representing the trajectory coordinates.
#' @param step A positive integer that defines the step size for subsampling the points. 
#' Default is `2`, meaning every second point will be selected.
#' @return A new trajectory object created using the subsampled points and the adjusted `delta_time`.
#' @export

subsampleTrajectory <- function(obj, step = 2) {
	if(!is.trajectory(obj)){
		stop("'obj' is not a valid trajectory object.")
	}
  idx <- seq(1, nrow(obj$points), by = step)
  trajectory(obj$points$x[idx], obj$points$y[idx], obj$delta_time * step)
}