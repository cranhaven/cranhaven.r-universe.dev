#' Calculate DTW alignment of two snow profiles
#'
#' This is the core function of the package and allows to match layers between pairs of snow profiles to align them. It
#' provides a variety of options, where the default values represent a good starting point to the alignment of most generic
#' profiles.
#'
#' @details
#' The individual steps of aligning snow profiles (which can all be managed from this function):
#'
#'  1. (optional) **Rescale** the profiles to the same height (cf., [scaleSnowHeight])
#'  2. **Resample** the profiles onto the same depth grid. 2 different approaches:
#'      - regular grid with a sampling rate that is provided by the user (recommended, cf., [resampleSP]).
#'      - irregular grid that includes all layer interfaces within the two profiles (i.e., set `resamplingRate = 'irregularInterfaces'`) (cf., [resampleSPpairs])
#'  3. Compute a weighted **local cost matrix** from multiple layer characteristics (cf., [distanceSPlayers])
#'  4. **Match the layers** of the profiles with a call to [dtw::dtw] (eponymous R package)
#'  5. Align the profiles by **warping** the query profile onto the reference profile (cf., [warpSP])
#'  6. (optional) If the function has been called with multiple different boundary conditions (global, top-down, or bottom-up alignments),
#'  the optimal alignment as determined by [simSP] or by the DTW distance will be returned.
#'  7. (optional) Compute a **similarity score** for the two profiles with [simSP]
#'
#'
#' @note
#' Furthermore, the alignment based on grain type information is currently only possible for specific grain types. These grain types
#' require a pre-defined distance or similarity, such as given by [grainSimilarity_align]. If your profile contains other grain types,
#' you are required to define your custom `grainSimilarity` matrix.
#'
#' The package used to require re-scaling of the profiles to identical snow heights. This requirement has been removed in v1.1.0.
#' Profiles therefore can be resampled onto a regular grid, whilst keeping their original total snow heights. The alignment can
#' then be carried out bottom.up or top.down with a relative or absolute window size. If the profiles have different snow heights and a relative
#' window size is provided, the window size is computed using the larger snow height of the two profiles (e.g., Profile A HS 100 cm,
#' Profile B HS 80 cm; window.size = 0.3 translates to an effective window size of +/- 33 cm).
#' See examples for alignments without prior re-scaling.
#'
#' @import dtw
#'
#' @param query The query snow profile to be warped
#' @param ref The reference snow profile to be warped against
#' @param open.end Is an open end alignment desired? Recommended if profiles will not be rescaled.
#' @param checkGlobalAlignment Do you want to check whether a global alignment performs better (i.e.,
#' `open.end = FALSE`), and use the optimal one of the computed alignments? `'auto'` sets to `TRUE` if `simType == "HerlaEtAl2021"` and to `FALSE` otherwise.
#' @param keep.internals Append resampled and aligned snow profiles as well as internal parameters to the output object?
#' @param step.pattern The local slope constraint of the warping path, defaults to Sakoe-Chiba's symmetric pattern
#' described by a slope factor of P = 1, see [dtw::stepPattern]
#' @param resamplingRate Scalar, numeric resampling rate for a regular depth grid. If the profiles have been rescaled prior to calling this routine, set to `NA`.
#' To resample onto the smallest possible mutual (original, likely irregular) depth grid (see Details, bullet point 2.2), set to `'irregularInterfaces'`.
#' @param rescale2refHS Rescale the query snow height to match the ref snow height?
#' @param bottom.up Compute an open.end alignment from the ground upwards?
#' @param top.down Compute an open.end alignment from the snow surface downwards?
#' @param simType the similarity between two profiles can be computed with different approaches, see [simSP]
#' @param ... Arguments passed to [distanceSPlayers], and [dtw::dtw], and [simSP] e.g.
#'
#'   * `dims`, `weights` (defaults specified in \code{\link{distanceSPlayers}})
#'   * `ddateNorm`, numeric, normalize deposition date (default specified in \code{\link{distanceSPlayers}})
#'   * `windowFunction`, default \code{\link{warpWindowSP}}
#'   * `window.size`, `window.size.abs`, `ddate.window.size` (defaults specified in \code{\link{warpWindowSP}})
#'   * `gtype_distMat`, specific to profile alignment, see [distanceSPlayers]
#'   * `gtype_distMat_simSP`, specific to similarity measure in [simSP]
#'   * `prefLayerWeights`, weighting matrix for preferential layer matching, e.g. [layerWeightingMat]
#'   * `nonMatchedSim` Similarity value `[0, 1]` for non-matched layers, see [simSP]. indifference = 0.5, penalty < 0.5
#'   * `nonMatchedThickness` How strongly should the thicknesses of non-matched layers influence the resulting similarity of
#' the profiles? The smaller this (positive!) value, the more influence; and vice versa. See [simSP] for more details.
#'   * `apply_scalingFactor` Setting for [simSP] in case `simType` is `"layerwise"` or `"wsum_scaled"`.
#' @md
#'
#' @return
#' An alignment object of class 'dtwSP' is returned. This is essentially a list with various information about the alignment.
#' If `keep.internals = TRUE`, the resampled snow profiles 'query', 'reference' and 'queryWarped', as well as the
#' 'costMatrix' and 'directionMatrix' are elements of the returned object.
#'
#' @author fherla
#'
#' @seealso [plotSPalignment], [simSP]
#'
#' @references Herla, F., Horton, S., Mair, P., & Haegeli, P. (2021). Snow profile alignment and similarity assessment for aggregating,
#' clustering, and evaluating of snowpack model output for avalanche forecasting. Geoscientific Model Development, 14(1), 239–258.
#' https://doi.org/10.5194/gmd-14-239-2021
#'
#' @examples
#'
#' ## Align a modeled and a manual snow profile, primarily based on default settings:
#' dtwAlignment <- dtwSP(SPpairs$A_modeled, SPpairs$A_manual, open.end = FALSE)
#'
#' ## check out the resulting dtwSP alignment object:
#' summary(dtwAlignment)
#' plotSPalignment(dtwAlignment = dtwAlignment)
#' plotCostDensitySP(dtwAlignment)
#'
#'
#' ## Align profiles from subsequent days without re-scaling them:
#' dtwAlignment <- dtwSP(SPpairs$C_day3, SPpairs$C_day1, resamplingRate = 0.5, rescale2refHS = FALSE,
#'                       window.size.abs = 30)
#' ## Note, per default both bottom.up and top.down alignments have been considered,
#' #  let's check out which one was suited better:
#' dtwAlignment$direction  # i.e., bottom up
#' ## Check it out visually:
#' plotSPalignment(dtwAlignment = dtwAlignment,
#'                 mainQu = "3 Days after...", mainRef = "...the reference profile.")
#' plotCostDensitySP(dtwAlignment, labelHeight = TRUE)
#'
#' @export
dtwSP <- function(query, ref, open.end = TRUE, checkGlobalAlignment = 'auto', keep.internals = TRUE,
                  step.pattern = symmetricP1, resamplingRate = 0.5, rescale2refHS = FALSE,
                  bottom.up = TRUE, top.down = TRUE, simType = "HerlaEtAl2021",
                  ...) {

  ## --- assertion, setup etc ----
  if (!is.snowprofile(query) | !is.snowprofile(ref)) stop("query and ref need to be two snowprofile objects.")
  if (bottom.up == FALSE && top.down == FALSE) stop("Either bottom.up or top.down must be TRUE!")
  if ("ddate.window.size" %in% names(list(...)) & top.down) warning("ddate.window.size currently yields meaningless results for all top.down alignments!")

  if (checkGlobalAlignment == 'auto' & !simType %in% c("HerlaEtAl2021", "simple2")) checkGlobalAlignment <- FALSE
  else if (checkGlobalAlignment == 'auto' & simType %in% c("HerlaEtAl2021", "simple2")) checkGlobalAlignment <- TRUE
  ## which direction to run dtw:
  dirs <- c("bottomUp", "topDown")[which(c(bottom.up, top.down))]
  if (checkGlobalAlignment && open.end) dirs <- c(dirs, "globalAlignment")
  ndirs <- length(dirs)
  if (ndirs > 1) keep.internals <- TRUE  # needed!

  ## --- rescaling ----
  if (rescale2refHS) {
    SC <- scaleSnowHeight(query, ref)
    query <- SC$queryScaled
    fac_trueQueryHeight <- SC$trueHeightFactor
  }

  ## --- resample profiles ----
  if (is.na(resamplingRate)) {  # profiles have been resampled prior to this routine, i.e. no action required
    RES <- list(ref = ref, query = query)
  } else {
    if (is.numeric(resamplingRate) && length(resamplingRate) == 1) {  ## resample profiles based on scalar numeric resamplingRate
      RES <- list(ref = resampleSP(ref, h = resamplingRate),
                  query = resampleSP(query, h = resamplingRate))
    } else  if (resamplingRate == "irregularInterfaces") {  ## profiles will be resampled onto an irregular depth grid that contains all layer interfaces of the two profiles
      if (!tolower(simType) %in% c("simple", "simple2", "herlaetal2021")) stop("Your simType is not yet implemented with the argument combination 'resamplingRate = 'irregularInterfaces' ', sorry!")
      RES <- resampleSPpairs(query, ref)
      RES$ref <- rmZeroThicknessLayers(RES$ref)
      RES$query <- rmZeroThicknessLayers(RES$query)
    } else {
      stop("Unknown resamplingRate specified!")
    }
  }

  ## number of layers:
  nL_r <- nrow(RES$ref$layers)
  nL_q <- nrow(RES$query$layers)

  ## --- calculate dtw alignment ----
  DMat <- array(dim = c(nL_q, nL_r, ndirs))
  DMat[,,1] <- distanceSPlayers(RES$query, RES$ref, ...)
  if (top.down) {
    if (nL_q == nL_r) {
      ## mirror matrix at top-left/bottom-right diagonal:
      ## (saves resources to rearrange DMat, but incorrect warping window for uneven number of layers)
      DMat[,, which(dirs == "topDown")] <- apply(apply(DMat[,,1], 1, rev), 1, rev)
    } else {
      DMat[,, which(dirs == "topDown")] <- distanceSPlayers(RES$query, RES$ref, top.down.mirroring = TRUE, ...)
    }
  }
  if (checkGlobalAlignment) DMat[,, which(dirs == "globalAlignment")] <- DMat[,,1]
  A <- list()
  for (i in seq_along(dirs)) {
    if (dirs[i] %in% c("bottomUp", "topDown")) {
      ## Open End warps:
      ## OE warps are one-sided in the dtw package (i.e. only sub-sequences of the reference ae considered)
      ## in order to get fully symmetric OE warps, the roles of the reference and query are swapped and
      ## the winning pair is determined by the smallest normalized distance
      ## In case of un-rescaled profiles only one of the two solutions is meaningful
      ## (i.e., the smaller profile is a subsequence of the taller one)
      ## EDIT fherla Sep 2021: this assumption is sometimes wrong! when including ddate.window.size, it can occur that the taller
      ## profile is a subsequence of the smaller one, if the smaller profile contains layers from a later date!
      ## However, fixing this means diving into a rabbit hole, and at this stage I am unsure whether it will be all that meaningful
      ## at the end anyways. While I think that it could be fixed, I think other issues would arise with e.g. averageSP like deeper than median HS, etc.
      straight <- 1
      swapped <- 1
      if (nL_q > nL_r) straight <- 0
      if (nL_r > nL_q) swapped <- 0
      straight <- as.logical(straight)
      swapped <- as.logical(swapped)


      if (straight) {
        aTrue <- dtw(DMat[,,i], open.end = open.end, keep.internals = keep.internals, step.pattern = step.pattern, ...)
      } else {
        aTrue <- list(normalizedDistance = Inf)
      }
      ## for swapped roles transpose the distance matrix between the two profiles:
      if (swapped) {
        aSwap <- dtw(t(DMat[,,i]), open.end = open.end, keep.internals = keep.internals, step.pattern = step.pattern, ...)
      } else {
        aSwap <- list(normalizedDistance = Inf)
      }

      ## choose winning pair:
      if (aTrue$normalizedDistance <= aSwap$normalizedDistance) {
        ## aTrue wins
        if ((dirs[i] == "topDown") & (nL_q > nL_r))  OE <- list(imin = aTrue$jmin, openEndType = "imin", openEndType_verbose = "matched_subsequence_of_query")
        else OE <- list(jmin = aTrue$jmin, openEndType = "jmin", openEndType_verbose = "matched_subsequence_of_reference")
        A[[i]] <- c(aTrue[c("costMatrix", "stepPattern", "N", "M", "openEnd", "openBegin", "windowFunction",
                                  "distance", "normalizedDistance", "localCostMatrix")],
                          list(index2 = aTrue$index2, index1 = aTrue$index1,
                               call = match.call(), direction = dirs[i], simType = simType),
                    OE
        )
      } else {
        ## aSwap wins
        ## note, that we want to keep the original reference and query roles nonetheless
        ## i.e. - take true cost matrix (etc.)
        ##      - take distance from swap
        ##      - also take indices from swap, but swap them (i.e. mirrored at the diagonal); and jmin -> imin (except for topDown)
        if ((dirs[i] == "topDown") & (nL_q < nL_r)) OE <- list(jmin = aSwap$jmin, openEndType = "jmin", openEndType_verbose = "matched_subsequence_of_reference")
        else OE <- list(imin = aSwap$jmin, openEndType = "imin", openEndType_verbose = "matched_subsequence_of_query")
        A[[i]] <- c(aSwap[c("stepPattern", "openEnd", "openBegin", "windowFunction",
                            "distance", "normalizedDistance", "M", "N")],
                    list(costMatrix = t(aSwap$costMatrix),
                         localCostMatrix = t(aSwap$localCostMatrix),
                         index2 = aSwap$index1, index1 = aSwap$index2,
                         call = match.call(), direction = dirs[i], simType = simType),
                    OE
        )
      }
    } else if (dirs[i] == "globalAlignment") {
      ## no OE warp / i.e., global alignment:
      if ((is.na(DMat[1,1,i])) | is.na(DMat[nL_q,nL_r,i])) {
        if (isTRUE(open.end)) {
          ## In that case the window size is too small for a global alignment, but the open.end alignment will be ok
          ## i.e., skip this calculation step
          dirs <- dirs[-i]
          ndirs <- length(dirs)
          next
        } else {
          stop("Only open.end alignment possible, enlarge window size if you desire a global alignment!")
        }
      } else {
        A[[i]] <- dtw(DMat[,,i], keep.internals = keep.internals, open.end = FALSE, step.pattern = step.pattern, ...
        )[c("costMatrix", "stepPattern", "N", "M", "openEnd", "openBegin", "windowFunction",
            "jmin", "distance", "normalizedDistance", "localCostMatrix", "index1", "index2")]
        A[[i]] <- c(A[[i]], list(indexRef = A[[i]]$index2, indexQuery = A[[i]]$index1,
                                 openEndType = "jmin", open_EndType_verbose = "matched_full_sequences",
                                 call = match.call(), direction = dirs[i], simType = simType))
      }
    }  # END IF

    ## correct meta information for full sequence matches in open.end warps:
    if ((all(c(max(A[[i]]$index1), max(A[[i]]$index2)) == dim(A[[i]]$localCostMatrix))) &
        (all(c(min(A[[i]]$index1), min(A[[i]]$index2)) == c(1, 1)))) {

      if (hasUnobservedBasalLayer(RES$ref)) A[[i]]$openEndType_verbose <- "matched_subsequence_of_reference"  # openEndType doesn't need to be adjusted until later point!
      else if (hasUnobservedBasalLayer(RES$query)) A[[i]]$openEndType_verbose <- "matched_subsequence_of_query"
      else A[[i]]$openEndType_verbose <- "matched_full_sequences"
    }

    ## correct topDown alignment object (indices and matrices)
    if (A[[i]]$direction == "topDown") {
      ## adjustments that are due to top down per se
      A[[i]]$costMatrix <- apply(apply(A[[i]]$costMatrix, 1, rev), 1, rev)
      A[[i]]$localCostMatrix <- apply(apply(A[[i]]$localCostMatrix, 1, rev), 1, rev)

      ## adjustments that are due to sequence lengths
      if (A[[i]]$openEndType == "imin") {  # nL_q >= nL_r
        A[[i]]$index1 <- A[[i]]$M + 1 - A[[i]]$index1
        A[[i]]$index2 <- A[[i]]$N + 1 - A[[i]]$index2
        A[[i]][A[[i]]$openEndType] <- A[[i]]$M + 1 - A[[i]][A[[i]]$openEndType][[1]]
        ## adjustment due to special case: topDown, different sequence lengths, and unobservedBasalLayer in reference
        if (A[[i]]$openEndType_verbose == "matched_subsequence_of_reference") A[[i]]$openEndType <- "jmin"

      } else if (A[[i]]$openEndType == "jmin") {  # nL_q <= nL_r
        A[[i]]$index1 <- A[[i]]$N + 1 - A[[i]]$index1
        A[[i]]$index2 <- A[[i]]$M + 1 - A[[i]]$index2
        A[[i]][A[[i]]$openEndType] <- A[[i]]$M + 1 - A[[i]][A[[i]]$openEndType][[1]]
      }
    }
    ## append profiles to alignment object
    if (keep.internals) {
      A[[i]]$query <- RES$query
      A[[i]]$reference <- RES$ref
      A[[i]] <- warpSP(A[[i]])
      if (rescale2refHS) A[[i]]$fac_trueQueryHeight <- fac_trueQueryHeight
    }
  }  # END LOOP

  ## choose whether bottomUp / TopDown / globalAlignment performed better (based on external function call simSP):
  if (ndirs > 1) {
    sim <- rep(NA, times = ndirs)
    for (i in seq_along(dirs)) {
      A[[i]]["sim"] <- sim[i] <-  simSP(RES$ref, A[[i]]$queryWarped, ...)
    }
    win <- which.max(sim)
  } else {
    win <- 1
    A[[win]]["sim"] <- simSP(RES$ref, A[[win]]$queryWarped, ...)
  }

  ## modify local cost matrix to resemble step pattern constraints:
  A[[win]]$localCostMatrix[is.na(A[[win]]$costMatrix)] <- NA

  ## inherits new class due to changes made to object, but keeps dtw class to use existing functionality (esp. dtwPlotDensity())
  class(A[[win]]) <- append("dtwSP", class(A[[win]]))



  return(A[[win]])

}
