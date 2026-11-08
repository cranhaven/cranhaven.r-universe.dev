#' Plots the minimum convex hull of a set of coordinates
#' @param xcoord - Array of x-coordinates or longitudinal values
#' @param ycoord - Array of y-coordinates or latitudinal values
#' @param lcolor - String or integer value indicating the color of the convex hull boundary lines
#' @return Plots a minimum convex hull
#' @note This function does not account for the possibility of points crossing the prime meridian and in cases where this occurs the convex hull shown will be incorrect
#' @examples
#' longs<-c(20,20,40,40)
#' lats<-c(-5,5,-5,5)
#' PlotConvexHull(xcoord=longs,ycoord=lats)
#' @importFrom grDevices chull
#' @importFrom graphics plot
#' @importFrom graphics lines
#' @export
PlotConvexHull<-function(xcoord, ycoord, lcolor="blue"){
  hpts <- chull(x = xcoord, y = ycoord)
  hpts <- c(hpts, hpts[1])
  plot(xcoord,ycoord)
  lines(xcoord[hpts], ycoord[hpts], col = lcolor)
}
