#' Calculate distances between object vectors in a SOM
#'
#' @param kohobj An object of class \code{missSOM}.
#' @param type Whether to calculate distances between the data objects, or the codebook vectors.
#'
#' @return An object of class \code{dist}, which can be directly fed into (e.g.) a hierarchical clustering.
#' @export
#' @description This function calculates the distance between objects using the distance functions, 
#' weights and other attributes of a trained SOM. This function is used in the calculation of the U matrix in 
#' function \code{plot.missSOM} using the \code{type = "dist.neighbours" argument.}
#' @seealso \code{\link{unit.distances}}, \code{\link{imputeSOM}}
#' @examples 
#' data(wines)
#' 
#' ## Data with no missing values 
#' set.seed(7)
#' sommap <- imputeSOM(scale(wines), grid = somgrid(6, 4, "hexagonal"))
#' obj.dists <- object.distances(sommap, type = "data")
#' code.dists <- object.distances(sommap, type = "codes")
#' 
#' ## Data with missing values 
#' X <- scale(wines)
#' X[1:5, 1] <- NaN
#' sommap <- imputeSOM(X, grid = somgrid(6, 4, "hexagonal"))
#' obj.dists <- object.distances(sommap, type = "ximp")
#' code.dists <- object.distances(sommap, type = "codes")
#'
object.distances <- function(kohobj, type = c("data", "ximp", "codes")) {
  distanceFunctions <- kohobj$dist.fcts
  dist.ptrs <- getDistancePointers(c(distanceFunctions))
  
  type <- match.arg(type)
  # kohobj must return a data matrix
  data <- kohobj[[type]]  


  nvars <- ncol(data)
  nobjects <- nrow(data)

  datamat <- t(data) 
  res <- ObjectDistances(data = datamat,
                         distanceFunctions = dist.ptrs)

  attributes(res) <- list("Size" = nobjects, "Diag" = FALSE, "Upper" = FALSE,
                          "method" = "imputeSOM", "call" = match.call(),
                          "class" = "dist")
  
  res
}
