#' Get Result
#' 
#' Getting result from numerical solving algorithm of given \code{\link{ODEnetwork}}.
#' 
#' @param odenet [\code{ODEnetwork}]\cr
#'    Object of class \code{\link{ODEnetwork}}.
#' @return a matrix with columns time and states 1 and 2 of class \code{\link{deSolve}}.
#' @export
#' @examples
#' masses <- 1
#' dampers <- as.matrix(0.1)
#' springs <- as.matrix(4)
#' odenet <- ODEnetwork(masses, dampers, springs)
#' getResult(odenet)
getResult <- function(odenet) {
  UseMethod("getResult")
}

#' @method getResult ODEnetwork
#' @export
getResult.ODEnetwork <- function(odenet) {
  odenet$simulation$results
}
