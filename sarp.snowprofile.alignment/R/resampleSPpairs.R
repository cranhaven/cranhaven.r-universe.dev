#' Resample a pair of profiles
#'
#' Resample a pair of (irregularly layered) profiles onto the smallest common height grid. To reduce data storage
#' this routine can be used to merge layers based on specified layer properties, if the input profiles
#' have been resampled earlier, or if due to other reasons existing layers in the individual profiles can be merged.
#' In summary, this routine alters how the layer information of snow profiles is *stored* without changing how the
#' profiles appear.
#'
#' @details The smallest common height grid is found by
#'
#'  1. extract all unique layer interfaces in both profiles
#'  2. resample each profile with the above height grid, \cr
#'  (!) but set all height values that exceed each max snow height to that max snow height!
#'
#' @seealso [resampleSP], [mergeIdentLayers]
#'
#' @importFrom utils tail
#' @import sarp.snowprofile
#'
#' @param query query snowprofile or snowprofileLayers object
#' @param ref reference snowprofile or snowprofileLayers object
#' @param mergeBeforeResampling shall adjacent layers with identical layer properties be merged? (boolean)
#' @param dims layer properties to consider for a potential merging
#'
#' @return a list with the resampled input objects under the entries `query` and `ref`.
#'
#' @author fherla
#'
#' @examples
#'
#' ## initial situation before mutual resampling:
#' ## two profiles with different snow heights and different numbers of layers
#' summary(SPpairs$A_manual)[, c("hs", "nLayers")]
#' summary(SPpairs$A_modeled)[, c("hs", "nLayers")]
#' opar <- par(no.readonly=TRUE)
#' par(mfrow = c(1, 2))
#' plot(SPpairs$A_manual, main = "Initial profiles before resampling",
#'      ylab = "Snow height", ymax = 272)
#' plot(SPpairs$A_modeled, ylab = "Snow height", ymax = 272)
#'
#' ## resampling:
#' resampledSPlist <- resampleSPpairs(SPpairs$A_manual, SPpairs$A_modeled,
#'                                    mergeBeforeResampling = TRUE)
#'
#' ## two profiles with different snow heights and IDENTICAL numbers of layers
#' summary(resampledSPlist$query)[, c("hs", "nLayers")]
#' summary(resampledSPlist$ref)[, c("hs", "nLayers")]
#' plot(resampledSPlist$query, main = "Profiles after resampling",
#'      ylab = "Snow height", ymax = 272)
#' plot(resampledSPlist$ref, ylab = "Snow height", ymax = 272)
#' par(opar)
#'
#' @export
resampleSPpairs <- function(query, ref, mergeBeforeResampling = FALSE, dims = c("gtype", "hardness")) {

  ## query input object class:
  if (is.snowprofile(query) & is.snowprofile(ref)) {
    ql <- query$layers
    rl <- ref$layers
    isProfile <- TRUE
  } else if (is.snowprofileLayers(query) & is.snowprofileLayers(ref)) {
    ql <- query
    rl <- ref
    isProfile <- FALSE
  }

  ## merge layers:
  if (mergeBeforeResampling) {
    ql <- mergeIdentLayers(ql, properties = dims)
    rl <- mergeIdentLayers(rl, properties = dims)
  }

  ## resample:
  huni <- sort(unique(c(ql$height, rl$height)))
  quMax <- tail(ql$height, n = 1)
  refMax <- tail(rl$height, n = 1)
  hqu <- ifelse(huni < quMax, yes = huni, no = quMax)
  href <- ifelse(huni < refMax, yes = huni, no = refMax)

  if (isProfile) {
    quRES <- resampleSP(query, h = hqu)
    refRES <- resampleSP(ref, h = href)
  } else {
    quRES <- resampleSP(ql, h = hqu)
    refRES <- resampleSP(rl, h = href)
  }

  return(list(query = quRES, ref = refRES))
}
