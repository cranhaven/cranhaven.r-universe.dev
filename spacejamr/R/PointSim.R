# Author: Darren Colby
# Date: 1/27/2022
# Purpose: To simulate spatial point processes

# Constructor methods for the PointSim class ------------------------------

#' Validate the input to an instance of a PointSim class
#'
#' @description validates the input from the PointProcess constructors
#'
#' @usage validate_PointSim(points, window, type, seed)
#'
#' @details This function should not be called directly
#'
#' @param points the number of points to simulate
#' @param a window of class spacejamr to use as the spatial extent
#' @param seed an optional seed
#'
#' @return A ppp object from the 'spatstat' package
#'
#' @author Darren Colby
#' Email: dscolby17@gmail.com
#' @noRd
validate_PointSim <- function(points, window, type, seed) {

   stopifnot(inherits(window, c("spacejamr", "owin")))

   if (!is.null(seed)) {set.seed(seed)}  # Optional seed

   # Call simulation helper functions
   point_sim <- type(n = points, win = window$window)

   return(point_sim)

}


#' Validate and set the class attribute of inputs to the PointProcess constructor
#'
#' @description Creates a new PointProcess object
#'
#' @usage new_PointSim(points, window, type, seed)
#'
#' @details This function should not be called directly
#'
#' @param points the number of points to simulate
#' @param window a window object of class spacejamr to use as the spatial extent
#' @param seed an optional seed
#'
#' @return An object of class PointSim that contains a geographical window
#' of class 'owin'. Within this window are four objects. n: the number of
#' simulated points. x: the x coordinates of the simulated points. y: the y
#' coordinates of the simulated points. markformat: an empty place holder.
#'
#' @author Darren Colby
#' Email: dscolby17@gmail.com
#' @noRd
new_PointSim <- function(points, window, type, seed) {

   # Validates the input
   point_sim <- validate_PointSim(points, window, type, seed)

   # Sets the class
   validated_sim <- structure(point_sim, class = "PointSim")

   return(validated_sim)

}


#' Simulate a spatial point process or sequence
#'
#' @description Creates a new Poisson Point Process in a spacejamr object
#'
#' @usage PointSim(points, window, type, seed)
#'
#' @param points the number of points to simulate
#' @param window a spacejamr object to use as the spatial extent
#' @param type the type of simulation. poisson_process simulates a spatial
#' Poisson process. halton simulates a Halton sequence where the maximal number
#' of simulated points is the first argument.
#' @param seed an optional seed
#'
#' @return An object of class PointSim that contains a geographical window
#' of class 'owin'. Within this window are four objects. n: the number of
#' simulated points. x: the x coordinates of the simulated points. y: the y
#' coordinates of the simulated points. markformat: an empty place holder.
#'
#' @examples
#' # Load spacejamr object
#' data("RI")
#'
#' # Poisson process
#' ri_points <- PointSim(points = 10, window = RI, seed = 42)
#'
#' @author Darren Colby \cr
#' Email: dscolby17@@gmail.com
#' @export
PointSim <- function(points, window, type = poisson_process, seed = NULL) {

   # Create a new object
   point_sim <- new_PointSim(points, window, type, seed)

   return(point_sim)
}


# Plot, print, and summary methods for PointSim classes -------------------


#' Plot simulated points from a PointSim object
#'
#' @description Plots the results of points simulated in a PointProcess or
#' HaltonSeq class, whcih obht inherit methods from the PointSim class.
#'
#' @details The returned plot can be refined with standard ggplot2 functions
#'
#' @param x an object of class PointSim or one of its child classes
#' @param y ignored
#' @param ... ignored
#' @param title an optional title. Default is "Simulated Points".
#' @param color an optional color for the simulated points. Default is red.
#'
#' @return A plot of classes 'gg' and 'ggplot'
#'
#' @examples
#' # Load spacejamr object
#' data("RI")
#'
#' ri_points <- PointSim(points = 10, window = RI, seed = 42)
#' plot(ri_points)
#'
#' @author Darren Colby \cr
#' Email: dscolby17@@gmail.com
#' @export
plot.PointSim <- function(x, y, ..., title = "Simulated Points", color = "red") {

   window <- sf::st_as_sf(x$window)  # sf object to pass to ggplot
   points <- as.data.frame(cbind(x$x, x$y))  # dataframe of points

   plot <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = window) +
      ggplot2::geom_point(data = points,
                          ggplot2::aes_string(x = "V1",
                                              y = "V2"),
                          color = color) +
      ggplot2::labs(title = title) +
      ggthemes::theme_map()

   return(plot)

}


#' Print information from a PointSim class
#'
#' @description Print method for both the PointProcess and HaltonSeq classes,
#' which inherit methods from the PointSim class.
#'
#' @param x a PointSim object or a child object
#' @param ... ignored.
#'
#' @return No return value, called for side effects
#'
#' @examples
#' # Load spacejamr object
#' data("RI")
#'
#' ri_points <- PointSim(points = 10, window = RI, seed = 42)
#' print(ri_points)
#'
#' @author Darren Colby \cr
#' Email: dscolby17@@gmail.com
#' @export
print.PointSim <- function(x, ...) {

   cat("PointSim Object\n\n")
   print(x$window)
   cat("\n")
   cat(paste("Points:", x$n, sep = ""))

}


#' Display summary information from a PointSim instance
#'
#' @description Prints a summary of information from either a PointProcess or
#' HaltonSeq object, whcih are both child classes of the PointSim class.
#'
#' @param object a PointSim object
#' @param ... ignored
#'
#' @return No return value, called for side effects
#'
#' @examples
#' # Load spacejamr object
#' data("RI")
#'
#' ri_points <- PointSim(points = 10, window = RI, seed = 42)
#' summary(ri_points)
#'
#' @author Darren Colby \cr
#' Email: dscolby17@@gmail.com
#' @export
summary.PointSim <- function(object, ...) {

   summary(spatstat.geom::ppp(object$x, object$y, object$window))
}
