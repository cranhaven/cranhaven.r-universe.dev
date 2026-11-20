#' Check if the first and last points of a "trajectory" object are within a given tolerance
#'
#' This function checks if the distance between the first and last points in an object of class 
#' `trajectory` is smaller than a specified tolerance `tol`. The distance is calculated using the 
#' Euclidean distance between the two points.
#'
#' @param obj An object of class `trajectory` that contains a component `points`.
#' @param tol A numeric value specifying the tolerance. If the distance between the first and 
#' last points is smaller than this value, the function returns `TRUE`. Default is `1e-6`.
#' @return A logical value (`TRUE` or `FALSE`), indicating whether the distance between the first 
#' and last points is smaller than `tol`.
#' @export

isClosed <- function(obj, tol = 1e-6) {
	if(!is.trajectory(obj)){
		stop("'obj' is not a valid trajectory object.")
	}
    start <- obj$points[1, ]
    end <- obj$points[nrow(obj$points), ]
    sqrt(sum((start - end)^2)) < tol
}