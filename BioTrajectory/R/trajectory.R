#' 
#' @docType class
#' @name trajectory
#' @title Trajectory Class
#' @description
#' A class representing a trajectory of an object over time, including the `x` and `y` coordinates
#' of the object's centroids across frames. It provides methods to manipulate and analyze the 
#' trajectory, such as calculating the length, summarizing the trajectory, printing the summary,
#' and plotting the trajectory.
#' 
#' @details
#' The `Trajectory` class stores the coordinates of an object across multiple frames. The class
#' provides methods to compute various aspects of the trajectory, such as its length and summary 
#' statistics, as well as visualizations. It can also be used to manipulate the trajectory, 
#' such as combining it with another trajectory using the `|` operator.
#'
#' @param x Numeric vector with the 'x' coordinates of the trajectory.
#' @param y Numeric vector with the 'y' coordinates of the trajectory.
#' @param delta_time Control variable with a default value of 1.
#' @export

trajectory <- function(x, y, delta_time = 1) {
  if (length(x) != length(y)) stop("x and y must be of the same length.")
  
  obj <- list(
    points = data.frame(x = x, y = y),
    delta_time = delta_time
  )
  class(obj) <- "trajectory"
  return(obj)
}

#' @title Length of the Trajectory
#' 
#' @description
#' Calculates the total length of the trajectory, which is the sum of the distances between 
#' consecutive points in the `Trajectory` object.
#' 
#' @param x An object of class `Trajectory`.
#' 
#' @return A numeric value representing the total length of the trajectory.
#' 
#' @export

length.trajectory <- function(x) {
  nrow(x$points)
}

#' @title Summary of the Trajectory
#' 
#' @description
#' Generates a summary of the trajectory, including the minimum, maximum, and mean of the `x` 
#' and `y` coordinates, as well as the total number of frames.
#' 
#' @param object An object of class `Trajectory`.
#' @param ... Other parameters to be passed.
#' 
#' @return A summary object containing statistics for both the `x` and `y` coordinates of the 
#'         trajectory.
#' 
#' @export

summary.trajectory <- function(object, ...) {
  s <-list(points=summary(object$points),
           delta_time= object$delta_time,
           length = length.trajectory(object))
  class(s) <- "summary.trajectory"
  return(s)
}

#' @title Print Summary of the Trajectory
#' 
#' @description
#' Prints a detailed summary of the trajectory, including statistics for both the `x` and `y` 
#' coordinates and the total number of frames.
#' 
#' @param x A `summary.trajectory` object, typically returned by the `summary.trajectory` method.
#' @param ... Other parameters to be passed.
#' 
#' @return Prints the summary directly.
#' 
#' @export

print.summary.trajectory <- function(x, ...) {
  cat("Summary of trajectory:\n")
  print(x$points)
  cat("Delta time:", x$delta_time, "\n")
  cat("Total length:", x$length, "\n")
}

#' @title Combine Two Trajectories
#' @name or
#' @description
#' Combines two `Trajectory` objects by concatenating their points. The result is a new trajectory 
#' that contains the coordinates of both input trajectories.
#' 
#' @param tr1 A `Trajectory` object.
#' @param tr2 A `Trajectory` object.
#' 
#' @return A new `Trajectory` object containing the concatenated points of the two input trajectories.
#' @rdname or
#' @export

`|.trajectory` <- function(tr1, tr2) {
  if (tr1$delta_time != tr2$delta_time) {
    stop("Delta times must be equal to concatenate trajectories.")
  }
  new_points <- rbind(tr1$points, tr2$points)
  trajectory(new_points$x, new_points$y, tr1$delta_time)
}

#' @title Convert to Trajectory Object
#' 
#' @description
#' Converts an object of type `data.frame` or similar into a `Trajectory` object. The `data.frame`
#' should contain columns `x` and `y` representing the coordinates.
#' 
#' @param x An object of class `data.frame` with `x` and `y` columns.
#' @param delta_time Control variable with a default value of 1.
#' 
#' @return A `Trajectory` object containing the coordinates from the `data.frame`.
#' 
#' @export

as.trajectory <- function(x, delta_time = 1) {
  if (is.data.frame(x) && all(c("x", "y") %in% names(x))) {
    return(trajectory(x$x, x$y, delta_time))
  } else if (is.matrix(x) && ncol(x) == 2) {
    return(trajectory(x[,1], x[,2], delta_time))
  } else {
    stop("Input must be a data.frame with x and y, or a 2-column matrix.")
  }
}

#' @title Check if Object is a Trajectory
#' 
#' @description
#' Checks if an object is of class `Trajectory`.
#' 
#' @param x An object to check.
#' 
#' @return A logical value (`TRUE` or `FALSE`) indicating whether the object is of class `Trajectory`.
#' 
#' @export

is.trajectory <- function(x){
  inherits(x, "trajectory")
}

#' @title Plot the Trajectory
#' 
#' @description
#' Plots the trajectory by displaying the `x` and `y` coordinates as points on a 2D plot. Optionally, 
#' you can add a line connecting the points.
#' 
#' @param x A `Trajectory` object to plot.
#' @param stepSize An integer specifying the interval for plotting segments of the trajectory. 
#'   If set to 0, the entire trajectory is plotted. Default is 0.
#' @param ... Other parameters to be passed.
#' 
#' @return A plot displaying the trajectory.
#' 
#' @export

plot.trajectory <- function(x, ..., stepSize = 0) {
  data <- x
  colorScheme<-colorRampPalette(c("purple","blue","cyan","yellow","orange","red"))(nrow(data$points))
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar)) 
  if (stepSize == 0) {
    layout(matrix(c(1, 2), nrow = 1, byrow = TRUE), c(3, 1), 3, TRUE)
    par(mai = c(0.8, 0.8, 0.4, 0))
    plot(data$points, pch = 19, col = colorScheme, xlab = "x", ylab = "y", main="Trajectory")
    par(mai = c(0.8, 0, 0.4, 0.8))
    legendPlot(seq_along(data$points$x), colorScheme)
  } else {
    k <- ceiling(nrow(data$points) / stepSize)
    m <- findM(k)
    xlim <- range(data$points$x)
    ylim <- range(data$points$y)
    par(mfrow = m, mai = c(0, 0, 0, 0), oma=c(0,0,1,0))
    for (i in 1:(nrow(data$points) - 1)) {
      if (i %% stepSize == 1) {
        plot(xlim, ylim,
          bty = "o", type = "n",
          axes = FALSE, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
      }
      segments(data$points$x[i], data$points$y[i], data$points$x[i + 1], data$points$y[i + 1],
        col = colorScheme[i], lwd = 10 * (i %% stepSize) / stepSize
      )
      abline(v = xlim[1], h = ylim[1], lwd = 1)
    }
    mtext(paste(c("Trajectory ", stepSize, " steps"), collapse=""), outer = TRUE)
  }
}
