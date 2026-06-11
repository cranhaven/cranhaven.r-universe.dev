#'
#' @param x An object of class \code{missSOM}.
#' @param ... Currently ignored.
#'
#' @rdname map.missSOM
#' @export
#'
"map" <- function(x, ...)
{
  UseMethod("map")
}

#' Map data to a supervised or unsupervised SOM
#'
#' @param x an object of class \code{missSOM}.
#' @param newdata a \code{matrix} or \code{data.frame}, equal to the \code{data} argument of the \code{imputeSOM} function. 
#' @param maxNA.fraction parameters that usually will be taken from the \code{x} object, but can be supplied by the user as well. 
#' Note that it is not possible to change distance functions from the ones used in training the map. See \code{\link{imputeSOM}} 
#' for more information.
#' @param ... Currently ignored.
#' @description Map a data onto a trained SOM.
#' @seealso \code{\link{imputeSOM}}
#'
#' @return A list with elements 
#' \item{unit.classif}{a vector of units that are closest to the objects in the data.}
#' \item{dists}{distances of the objects to the closest units. Distance measures are the same ones used in training the map.}
#' @export
#' @examples 
#' data(wines)
#' set.seed(7)
#' 
#' training <- sample(nrow(wines), 150)
#' Xtraining <- scale(wines[training, ])
#' somnet <- imputeSOM(Xtraining, somgrid(5, 5, "hexagonal"))
#' 
#' map(somnet, scale(wines[-training, ], 
#'                   center=attr(Xtraining, "scaled:center"), 
#'                   scale=attr(Xtraining, "scaled:scale")))
#'
map.missSOM <- function(x,
                        newdata,
                        maxNA.fraction = x$maxNA.fraction,...)
{
  ## ##########################################################################
  ## Get relevant info from kohonen object
  codes <- x$codes
  
  if (missing(newdata) & !is.null(x$data)) {
    newdata <- x$data
  } else {
    newdata <- check.data(newdata)
  }
 
  dist.ptrs <- getDistancePointers(x$dist.fcts)

  nachecks <- check.empty.columns(newdata, maxNA.fraction = maxNA.fraction)
  newdata <- remove.data.na(newdata, nachecks)

  ## ##########################################################################
  ## Final preparations
  nvars <- ncol(codes)
  ncodes <- nrow(codes)
  nobjects <- nrow(newdata)
  
  
  newdata <- t(newdata)
  codes <- t(codes)

  ## ##########################################################################
  ## Go!
  res <- RcppMap(data = newdata,
                 codes = codes,
                 distanceFunctions = dist.ptrs)
  
  list(unit.classif = res$winners + 1,
       distances = res$unitdistances)
}
