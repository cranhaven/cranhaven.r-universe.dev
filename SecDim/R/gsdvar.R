#' Generating second-dimension variables for a spatial variable
#'
#' @description Generating second-dimension variables for a spatial variable
#'
#' @param pointlocation A matrix or data frame of point locations
#' @param gridlocation A matrix or data frame of grid locations
#' @param gridvar A matrix or data frame of grid variables
#' @param distbuf A vector of distance buffer values
#' @param quantileprob A vector of quantile probability values
#'
#' @return A data frame containing the selected variables for the second dimension
#'
#' @importFrom geosphere distHaversine
#' @importFrom stats quantile
#'
#' @examples
#' data(obs)
#' data(grids)
#' pointlocation <- obs[sample(nrow(obs), 20), c("Lon", "Lat")]
#' gridlocation <- grids[, c("Lon", "Lat")]
#' gridvar <- grids$Elevation
#' system.time({
#' sdavars <- gsdvar(pointlocation, gridlocation, gridvar,
#'                   distbuf = c(1, 2, 3), quantileprob = c(0, 0.5, 1))
#' })
#'
#' @export
#'

gsdvar <- function(pointlocation, gridlocation, gridvar,
                   distbuf = seq(1, 10, 1), quantileprob = seq(0, 1, 0.1)) {

  samples <- as.data.frame(pointlocation)
  grids <- as.data.frame(gridlocation)

  nbuf <- length(distbuf)
  nprob <- length(quantileprob)

  xx <- data.frame(matrix(NA, nrow(samples), nprob*nbuf))
  buf_names <- paste0("b", distbuf)
  prob_names <- paste0("t", quantileprob)
  namexx <- paste0(rep(buf_names, each = nprob),
                   rep(prob_names, times = nbuf))
  names(xx) <- namexx

  for (i in 1:nrow(samples)) {
    zi <- distHaversine(samples[i, ], grids, r = 6378137) / 1000
    h <- lapply(distbuf, function(x) which(zi < x))


    xxi <- matrix(NA, nbuf, nprob)

    for (t in 1:nbuf) {
      xj <- sapply(quantileprob, function(x) quantile(gridvar[h[[t]]], x))
      xxi[t, ] <- xj
    }
    xx[i, ] = as.vector(t(xxi))
  }

  out <- cbind(pointlocation, xx)

  return(out)
}
