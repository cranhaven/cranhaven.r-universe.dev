#' Provides smooth unit colors for SOMs
#'
#' @param grid An object of class \code{somgrid}, such as the \code{grid} element in a \code{kohonen} object.
#' @param phis A vector of three rotation angles. Values for red, green and blue are given by the y-coordinate of the units after rotation 
#' with these three angles, respectively. The default corresponds to (approximate) red colour of the middle unit in the top row, and pure 
#' green and blue colours in the bottom left and right units, respectively. In case of a triangular map, the top unit is pure red.
#' @param offset Defines the minimal value in the RGB colour definition (default is 0). By supplying a value in the range [0, .9],
#' pastel-like colours are provided.
#'
#' @return Returns a matrix with three columns corresponding to red, green and blue. This can be used in the \code{rgb} function to provide colours for the units.
#' @export
#' @seealso \code{\link{plot.missSOM}}
#' @description Function provides colour values for SOM units in such a way that the colour changes smoothly in every direction.
#' @examples 
#' data(wines)
#' som.wines <- imputeSOM(wines, grid = somgrid(5, 5, "hexagonal"))
#' 
#' colour1 <- tricolor(som.wines$grid)
#' plot(som.wines, "mapping", bg = rgb(colour1))
#' colour2 <- tricolor(som.wines$grid, phi = c(pi/6, 0, -pi/6))
#' plot(som.wines, "mapping", bg = rgb(colour2))
#' colour3 <- tricolor(som.wines$grid, phi = c(pi/6, 0, -pi/6), offset = .5)
#' plot(som.wines, "mapping", bg = rgb(colour3))
#'
tricolor <- function(grid,
                     phis = c(0, 2*pi/3, 4*pi/3), # default RGB
                     offset = 0) {
  if (offset < 0 | offset > .9)
    stop("Illegal offset value, should be in [0, .9]")
  
  rgbs <- matrix(0, nrow(grid$pts), 3)
  for (i in seq(along=phis)) {
    Rmat <- matrix(c(cos(phis[i]), sin(phis[i]), -sin(phis[i]), cos(phis[i])),
                   2, 2)
    X <- grid$pts %*% Rmat
    rgbs[,i] <- X[,2]
  }

  rgbs <- sweep(rgbs, 2, apply(rgbs, 2, min), FUN = "-")
  maxima <- apply(rgbs, 2, max) / (1 - offset)

  sweep(rgbs, 2, maxima, FUN = "/") + offset
}
