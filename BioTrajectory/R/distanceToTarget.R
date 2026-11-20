#' Calculates distances to a target point
#'
#' Given a dataset of points, it computes the Euclidean distances from each point to a specified target point. 
#' The function also identifies contiguous segments of points that fall within a specified radius around the target.
#'
#' @param data An object of class `trajectory` containing a collection of points with coordinates.
#' @param target A numeric vector representing the coordinates of the target point.
#' @param targetRadious A numeric value indicating the radius around the target point. 
#' Points within this radius are considered to be close to the target. Default is 0.
#' @return A list containing:
#' \item{distance}{A numeric vector of distances from each point in `data` to the `target`.}
#' \item{r}{A list with two components: `start`, indicating the starting indices of contiguous segments 
#' of points within the target radius, and `length`, indicating the lengths of these segments.}
#' \item{target_radious}{The radius around the target point.}
#' @export

distanceToTarget <- function(data, target, targetRadious = 0) {
  if(!is.trajectory(data)){
    stop("'data' is not a valid trajectory object.")
  }
  if (is.null(target)) {
    stop("The 'target' parameter is required but missing.")
  }
  distTarget <- function(v) {
    sqrt(sum((v - target)^2))
  }
  distance <- apply(data$points, 1, distTarget)
  r <- rle(distance <= targetRadious)
  start <- cumsum(c(1, r$lengths))[r$values == TRUE]
  length <- r$lengths[r$values == TRUE]
  names(start) <- NULL
  names(length) <- NULL
  r <- list(start = start, length = length)
  return(invisible(list(distance = distance, r = r, targetRadious = targetRadious)))
}
