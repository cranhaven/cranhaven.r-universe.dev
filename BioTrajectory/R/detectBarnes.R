#' Detect Circles in a Maze Image
#'
#' Detects circles representing boards and holes in a given image of a maze.
#' It utilizes edge detection to identify potential circle patterns based on specified
#' radius parameters. The function returns the detected circles' coordinates, along
#' with additional information about the board and hole radii.
#'
#' @param im A matrix representing the image where the circles will be detected.
#' @param boardRadius The expected radius of the board circles.
#' @param holeRadius The expected radius of the hole circles.
#' @param sigma An optional parameter that controls the standard deviation for the
#'   Gaussian filter used in edge detection (default value is 25).
#' @param plot Opcion of plot
#'
#' @return A list containing:
#' \item{c1}{Coordinates of the detected board circle.}
#' \item{boardRadius}{Radius of the board circle.}
#' \item{c2}{A list of coordinates representing the detected hole circles.}
#' \item{holeRadius}{Radius of the hole circles.}
#'
#' @description
#' The function first applies the Canny edge detection algorithm to the input image.
#' It then detects circles corresponding to the board and holes using the
#' \code{detectCircles} function. The function filters the detected hole circles
#' based on their distance from the board circle, ensuring they fall within acceptable
#' ranges relative to the specified radii. Finally, it calculates the median radius
#' and angle differences of the remaining circles and constructs a circular representation
#' for the holes based on these parameters.
#' @examples
#' path <- system.file('extdata/data.tiff', package='BioTrajectory')
#' im <- tiff::readTIFF(path)
#' im <- imager::as.cimg(t(im[,,1]))
#' Barnes <- detectBarnes(im, boardRadius=207, holeRadius=13, sigma=25)
#' @export

detectBarnes <- function(im, boardRadius, holeRadius, sigma = 25, plot = TRUE) {
  px <- cannyEdges(im, alpha = 0.4, sigma = 8)
  c1 <- detectCircles(px, radius = boardRadius, N = 1, sigma)
  c2 <- detectCircles(px, radius = holeRadius, N = 40, sigma)

  filt <- rep(TRUE, nrow(c2))

  ts <- numeric()
  for (i in 1:nrow(c2)) {
    r <- sqrt(sum((c2[i, c("x", "y")] - c1[1, c("x", "y")])^2))
    t <- atan2(
      c2[i, "y"] - c1[1, "y"],
      c2[i, "x"] - c1[1, "x"]
    )

    ts <- c(ts, t)
    if (r + holeRadius >= boardRadius | r - holeRadius <= 0.74 * boardRadius) {
      filt[i] <- FALSE
      next
    }
  }
  c2 <- c2[filt, ]
  ts <- ts[filt]

  or <- order(ts)
  c2 <- c2[or, ]

  if (plot == TRUE)
    showBarnes(im, boardRadius, holeRadius, c1, c2) 

  return(list(c1 = c1, boardRadius = boardRadius, c2 = c2, holeRadius = holeRadius))
}
