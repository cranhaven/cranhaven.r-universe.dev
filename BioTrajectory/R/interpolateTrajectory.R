#' Interpolates a trajectory
#'
#' Given a dataset of points, it interpolates to generate intermediate points between them.
#'
#' @param data An object of class `trajectory` containing a collection of points with coordinates.
#' @param n An integer indicating the number of intermediate points to generate
#' between each pair of points. Default is 4.
#' @return An object of class `trayectoria`, which contains the `x` and `y` coordinates, including
#'         both the original points and any interpolated points. The `trayectoria` object allows users to 
#'         store, manipulate, and analyze the coordinates of the object across frames. If no object is detected 
#'         in a frame, the corresponding coordinates will be set to `NA`.
#' 
#' @export

interpolateTrajectory <- function(data, n = 4) {
  if(!is.trajectory(data)){
    stop("'data' is not a valid trajectory object.")
  }
  N <- nrow(data$points)
  res <- c(data$points$x[1], data$points$y[1])
  for (i in 1:(N - 1)) {
    data1 <- data$points[i + 0:1, ]
    if (data1$x[1] != data1$x[2]) {
      xyI <- approx(data1, n = 2 + n)
      if (data1$x[1] > data1$x[2]) {
        xyI$x <- rev(xyI$x)
        xyI$y <- rev(xyI$y)
      }
    } else {
      xyI <- data.frame(
        x = rep(data1$x[1], 2 + n),
        y = seq(data1$y[1], data1$y[2], length.out = 2 + n)
      )
    }
    temp <- cbind(head(xyI$x, -1), head(xyI$y, -1))
    colnames(temp) <- c("x", "y")
    res <- rbind(res, temp)
  }
  res <- rbind(tail(res, -1), tail(data$points, 1))
  traj <-trajectory(x=res[,1],y=res[,2])
  return(traj)
}