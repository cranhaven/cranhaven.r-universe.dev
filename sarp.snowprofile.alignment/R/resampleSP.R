#' Resample snowprofile
#'
#' Resample an individual snow profile onto a new depth-grid (i.e., height-grid).
#'
#' @details
#' This routine alters how the layer information of snow profiles is *stored* without changing how the profiles appear.
#' Note, however, that only layer properties that are constant within the individual layers will be resampled:
#' i.e., `height`, `hardness`, `gtype`, `ddate` will be resampled. However, `temperature`, for example,
#' will not be resampled, because it is not constant within layers.
#'
#' @importFrom stats approx
#' @import sarp.snowprofile
#'
#' @param x snowprofile (or snowprofileLayers) object
#' @param h Sampling rate (i.e. constant depth increment) in centimeters, if given as scalar (default is 0.5 cm).
#' Layers smaller than the scalar h will not be resolved in the resampled profile.
#' Can also be a vector specifying the desired *height* grid of the resampled profile (useful for non-constant increments).
#' But, be WARNED, that a meaningless grid will produce colorful but senseless output!
#' @param n Number of layers in resampled profile (optional). *A given n will overrule a conflicting h!*
#'
#' @return resampled snowprofile with the same metadata as x, but resampled "layers". **Note** that only
#' the following layer properties will be resampled: `height`, `hardness`, `gtype`, `ddate`.
#' If input was a snowprofileLayers object, the output will be, too.
#'
#' @author fherla
#'
#' @seealso [resampleSPpairs], [mergeIdentLayers]
#'
#' @examples
#'
#' ## (1) constant sampling rate of 1 cm:
#' profileResampled <- resampleSP(SPpairs$A_modeled, h = 1.0)
#'
#' ## compare profile summary before and after resampling:
#' summary(SPpairs$A_modeled)[, c("hs", "nLayers")]
#' summary(profileResampled)[, c("hs", "nLayers", "changes")]
#' head(profileResampled$layers)
#'
#' ## compare profile plots before and after resampling (i.e., appear identical!)
#' opar <- par(no.readonly=TRUE)
#' par(mfrow = c(1, 2))
#' plot(SPpairs$A_modeled, main = "original", ylab = "Snow height")
#' plot(profileResampled, main = "resampled", ylab = "Snow height")
#' par(opar)
#'
#' ## (2) resample to 150 layers:
#' profileResampled <- resampleSP(SPpairs$A_manual, n = 150)
#' summary(profileResampled)[, c("hs", "nLayers", "changes")]
#' head(profileResampled$layers)
#'
#'
#' ## (3) resample onto arbitrarily specified grid
#' ## (issues a warning when the new-grid HS deviates too much from the original HS)
#' irregularGrid <- c(2 + cumsum(c(0, c(10, 15, 5, 1, 3, 30, 50))), 120)
#' profileResampled <- resampleSP(SPpairs$A_manual, h = irregularGrid)
#'
#' @export


resampleSP <- function(x, h = 0.5, n = NULL) {

  ## --- query input object class ----
  if (is.snowprofile(x)) {
    layers <- x$layers
  } else if (is.snowprofileLayers(x)) {
    layers <- x
  }

  ## --- initialize resampled height vector ----
  max_height <- layers$height[nrow(layers)]
  ## (height refers to height of a layer's top interface)
  if (!is.null(n)) {
    ## a given n has highest priority:
    r_n <- n                                  # number of resampled layers
    r_height <- seq(from = max_height/r_n,
                    to = max_height,
                    length.out = r_n)         # resampled height vector
  } else {
    if (length(h) > 1) {
      ## h given as vector directly:
      if (max(h) > 1.1 * max_height) warning(paste0("Your desired height grid exceeds the max snow height (", max_height, " cm) of your given profile by more than 10% (resampled to ", max(h), " cm)"))
      if (max(h) < 0.9 * max_height) warning(paste0("Your desired height grid underestimates the max snow height (", max_height, " cm) of your given profile by more than 10% (resampled to ", max(h), " cm)"))
      r_height <- h                           # resampled height vector
    } else {
      ## h as default or given as constant increment:
      dh <- h
      r_n <- round(max_height/dh)                # number of resampled layers
      r_height <- seq(from = dh, to = dh*r_n,
                      length.out = r_n)          # resampled height vector
    }
  }
  ## don't allow unobservedBasalLayer (i.e., NA layers) to be resampled
  if (hasUnobservedBasalLayer(layers)) r_height <- r_height[r_height >= layers$height[1]]

  ## --- create a map between original and resampled layers ----
  ## using "approx" to retrieve map; this map consequently allows to map non-numeric vectors such as grain type
  ## using "approx" in a left-continous sense, i.e. top interface is targeted
  ## (-> this is done by "f", fully left-continuous at f=1)
  tmp_height <- approx(x = layers$height, y = layers$height, xout = r_height, method = "constant",
                       rule = 2, f = 1)[[2]]
  map <- tryCatch({
    vapply(X = tmp_height, FUN = function(x) which(x == layers$height), FUN.VALUE = 1)
  }, error = function(err) {
    stop("Error, likely caused by malformatted profile! -> validate_snowprofile!")
  })

  r <- droplevels(layers[map, !names(layers) %in% c("height", "thickness", "depth")])
  r$height <- r_height

  ## --- create new snowprofile object ----
  if (is.snowprofile(x)) {
    xout <- x[names(x) != "layers"]
    xout$layers <- snowprofileLayers(layerFrame = r, dropNAs = FALSE, validate = FALSE)
    xout$hs <- max( c(suppressWarnings(max(xout$layers$height)), xout$hs), na.rm = FALSE )

    if ("changes" %in% names(xout)) xout$changes <- paste(xout$changes, " -> resampled")
    else xout$changes  <- "resampled"
    class(xout) <- class(x)
  } else {
    xout <- snowprofileLayers(layerFrame = r, validate = FALSE)
  }

  return(xout)

}
