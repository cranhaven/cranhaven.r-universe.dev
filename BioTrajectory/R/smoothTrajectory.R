#' Apply a moving average filter to the trajectory and create a smoothed trajectory
#'
#' This function applies a moving average filter to the `x` and/or `y` coordinates of a 
#' "trajectory" object. The filter is applied using a window of a specified size. The function 
#' then creates a new trajectory object using the smoothed coordinates. The filter is applied 
#' symmetrically around each point.
#'
#' @param obj An object of class `trajectory` that contains a component `points`, which is a 
#' data frame with the coordinates of the points. It is assumed that `points` has columns `x` 
#' and `y` representing the trajectory coordinates.
#' @param method A character string specifying which coordinate(s) to smooth. Can be one of 
#' `"x"`, `"y"`, or `"both"`. Default is `"both"`, which smooths both the `x` and `y` coordinates.
#' @param window An odd integer specifying the window size for the moving average filter. 
#' The window must be an odd number to ensure a symmetric filter. Default is 3.
#' @return A new trajectory object created using the smoothed `x` and `y` coordinates.
#' @export

smoothTrajectory <- function(obj, method = c("x", "y", "both"), window = 3) {
  if(!is.trajectory(obj)){
    stop("'obj' is not a valid trajectory object.")
  }
  method <- match.arg(method)
  if (window %% 2 == 0) stop("Window size must be odd.")
  
  smooth_vec <- function(v) {
    filter(v, rep(1/window, window), sides = 2)
  }
  
  x <- obj$points$x
  y <- obj$points$y
  
  if (method == "x" || method == "both") {
    x <- as.numeric(smooth_vec(x))
  }
  if (method == "y" || method == "both") {
    y <- as.numeric(smooth_vec(y))
  }

  valid <- complete.cases(x, y)
  trajectory(x[valid], y[valid], obj$delta_time)
}