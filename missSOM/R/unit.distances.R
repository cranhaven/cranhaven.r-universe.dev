### Calculate distances in a SOM map. Crude and
### slow implementation, but hey.

#' SOM-grid related functions
#'
#' @param grid an object of class \code{somgrid}.
#' @param toroidal logical, whether the grid is toroidal or not. If not provided to the \code{unit.distances} function, the information in 
#' the \code{grid} object will be used.
#'
#' @return Function \code{unit.distances} returns a (symmetrical) matrix containing distances. When \code{grid$n.hood} equals "circular", Euclidean distances are used; for \code{grid$n.hood} is "square" 
#' maximum distances. For toroidal maps (joined at the edges) distances are calculated for the shortest path.
#' @export
#' 
#' @rdname unit.distances
#' @description Function \code{somgrid} (modified from the version in the class package) sets up a grid of units, of a specified size 
#' and topology. Distances between grid units are calculated by function \code{unit.distances}.
#' @examples 
#' mygrid <- somgrid(5, 5, "hexagonal")
#' fakesom <- list(grid = mygrid)
#' class(fakesom) <- "missSOM"
#' 
#' oldpar <- par(mfrow = c(2,1))
#' dists <- unit.distances(mygrid)
#' plot(fakesom, type="property", property = dists[1,],
#'      main="Distances to unit 1", zlim=c(0,6),
#'      palette = rainbow, ncolors = 7)

#' dists <- unit.distances(mygrid, toroidal=TRUE)
#' plot(fakesom, type="property", property = dists[1,],
#'      main="Distances to unit 1 (toroidal)", zlim=c(0,6),
#'      palette = rainbow, ncolors = 7)
#' par(oldpar)
#'
unit.distances <- function(grid, toroidal)
{
  if (missing(toroidal)) toroidal <- grid$toroidal

  if (!toroidal) {
    if (grid$topo == "hexagonal") {
      return(as.matrix(stats::dist(grid$pts)))
    } else {
      return(as.matrix(stats::dist(grid$pts, method="maximum")))
    }
  }

  ## only for toroidal maps:
  np <- nrow(grid$pts)
  maxdiffx <- grid$xdim/2
  maxdiffy <- max(grid$pts[,2])/2
  
  result <- matrix(0, np, np)
  for (i in 1:(np-1)) {
    for (j in (i+1):np) {
      diffs <- abs(grid$pts[j,] - grid$pts[i,])
      if (diffs[1] > maxdiffx)
        diffs[1] <- 2*maxdiffx - diffs[1]
      if (diffs[2] > maxdiffy)
        diffs[2] <- 2*maxdiffy - diffs[2]
      
        if (grid$topo == "hexagonal") {
          result[i,j] <- sum(diffs^2)
        } else {
          result[i,j] <- max(diffs)
        }
    }
  }

  if (grid$topo == "hexagonal") {
    sqrt(result + t(result))
  } else {
    result + t(result)
  }
}
