#' Estimate the Radius of a Circle Fitting Four Points
#'
#' This function estimates the center and radius of a circle that best fits four points provided by the user.
#' The user interacts with the plot to select four points, and the function optimizes the parameters 
#' (center and radius) using a least squares approach.
#'
#' @param frame The plot or frame to display the points and interact with the user.
#'
#' @return A vector of length 3, where:
#'   - The first element is the x-coordinate of the circle center (cx).
#'   - The second element is the y-coordinate of the circle center (cy).
#'   - The third element is the radius of the circle (r).
#'
#' @details
#' The function plots the provided `frame` and then uses the `locator()` function to allow the user 
#' to click on four points. The function then performs optimization using the `optim()` function to 
#' minimize the difference between the selected points and the circle's equation.
#'
#' The objective function (`ftemp`) calculates the sum of squared differences between the points 
#' and the expected distance from the circle's center.
#'
#' @examples
#' if(interactive()){
#' path <- system.file('extdata/data.tiff', package='BioTrajectory')
#' im <- tiff::readTIFF(path)
#' im <- imager::as.cimg(t(im[,,1]))
#' frame <- plot(im)
#' circle_params <- getRadius(frame)
#' print(circle_params)
#' }
#'
#' @export

getRadius <- function(frame) {
  ftemp <- function(X) {
    cx <- X[1]
    cy <- X[2]
    r <- X[3]
    res <- sum(abs((points$x - cx)^2 + (points$y - cy)^2 - r^2))
    return(res)
  }

  plot(frame, main = "Click on the image four times to define the area of interest.")
  points <- locator(n = 4) 

  X0 <- c(mean(points$x), mean(points$y), 0.5 * sqrt(diff(points$x[1:2])^2 + diff(points$y[1:2])^2))

  x <- optim(X0, ftemp)

  circles(x$par[1], x$par[2], x$par[3])
  
  return(x$par)
}
