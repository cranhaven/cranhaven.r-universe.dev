#' @title Weighted tiles
#'
#' @description Breaks input vector into n groups. Returns the (optional weighted) tile of an
#' individual observation in vector x.
#'
#' @details Breaks vector x into n sub groups. The main difference with other tile functions (for
#' instance ntile from dplyr) is that those functions break up vector x in exact equal size
#' sub groups. Observations with the same value can end up in different tiles. In this function,
#' observations with the same value always end up in the same tile, therefore sub groups may have
#' different sizes. Especially when the weights argument is used. For a weighted tile function
#' with the same group size, see for instance weighted_ntile from the grattan package.
#'
#' When using a short-length vector (compared to the number of tiles) or with high variance weights,
#' output may be different than anticipated.
#'
#' @param x a numeric vector for which the quantiles are computed. Missing values are left
#' as missing.
#' @param n the number of desired sub groups to break vector x into.
#' @param weights an optional vector of weights of x to be used in the computation of the
#' tiles. Should be NULL or a numeric vector.
#'
#' @return A vector of integers corresponding to the quantiles of vector x.
#'
#' @examples
#' #Break up the income variable in the Mexican Income data set into 10 groups (tiles)
#' data(mex_inc_2008)
#'
#' #unweighted tiles:
#' q <- ntiles.wtd(x=mex_inc_2008$income, n=10)
#'
#' #weighted tiles:
#' qw <- ntiles.wtd(x=mex_inc_2008$income, n=10, weights=mex_inc_2008$factor)
#'
#' @export


ntiles.wtd <- function(x,n,weights=NULL) {
  if (is.null(weights)){

    splits <- 1/n
    tile_splits <- stats::quantile(x, probs=c(seq(splits,1-splits, splits)), na.rm=TRUE)
    tiles <- as.numeric(.bincode(x,breaks=c(-Inf, tile_splits, Inf), right=TRUE, include.lowest = TRUE))

    } else {

  if (!all(weights>=0, na.rm=TRUE)) stop("At least one weight is negative", call.=FALSE)
  if (all(weights == 0, na.rm=TRUE)) stop("All weights are zero", call.=FALSE)

  missing <- !(is.na(x) | is.na(weights))
  x_sel <- x[missing]
  weights_sel <- weights[missing]
  splits <- 1/n
  tile_splits <- Hmisc::wtd.quantile(x_sel, probs=c(seq(splits,1-splits, splits)), weights=weights_sel, na.rm=TRUE)
  tiles <- as.numeric(.bincode(x,breaks=c(-Inf, tile_splits, Inf), right=TRUE, include.lowest = TRUE))
  tiles[(is.na(weights))] <- NA
    }
  return(tiles)
}
