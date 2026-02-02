#' Warp one snow profile onto another one
#'
#' After the DTW alignment of two profiles, the maps between the two profiles can be used to
#' warp one profile onto the other profile. In other words, the layer thicknesses of the warped profile
#' are adjusted to optimally align with the corresponding layers of the other profile.
#'
#' After this procedure, the thickness of some layers can be zero, which leads to the layers disappearing.
#'
#' This function is automatically called in `dtwSP(..., keep.internals = TRUE)` to warp the query profile
#' onto the reference profile.
#'
#' *Whom* to warp: There exist 8 different options, 4 for warping the query onto the ref and 4 for vice versa.
#' The 4 options for warping the query onto the ref are:
#'
#'   - global alignment / partial alignment where entire query is matched to subsequence of ref ("jmin")
#'   - partial alignment where entire ref is matched to subsequence of query ("imin")
#'   - partial top down alignment where entire query is matched to subsequence of ref ("jminTopDown")
#'   - partial top down alignment where entire ref is matched to subsequence of query ("iminTopDown")
#'
#' For the other case, warping the ref onto the query, only the equivalent of the first option is implemented.
#'
#' For developers: Including new variables in the output of warped profiles can easily be done by inserting a respective command
#' at the end of this function. There are many example variables added already.
#'
#' @param alignment DTW alignment object from [dtwSP] containing the two profiles (i.e., called `dtwSP(..., keep.internals = TRUE)`)
#' @param whom whom to warp? "query" (= "jmin"), "imin", "queryTopDown" (= "jminTopDown"), "iminTopDown", "ref";
#' if 'NA' the routine determines that itself from the structure of the alignment object. (see Details)
#'
#' @return Returns the input alignment object including the element alignment$queryWarped (or $referenceWarped),
#' which are the warped snow profiles. The class of the alignment object is altered to "dtwSP", but still inherits "dtw".
#'
#' @author fherla
#'
#' @examples
#'
#' ## first align profiles
#' alignment <- dtwSP(SPpairs$A_modeled, SPpairs$A_manual, open.end = FALSE)
#'
#' ## warp reference profile onto query profile:
#' refWarped <- warpSP(alignment, whom = "ref")$referenceWarped
#' opar <- par(no.readonly =TRUE)
#' par(mfrow = c(1, 2))
#' plot(alignment$query, main = "query")
#' plot(refWarped, main = "warped reference")
#' par(opar)
#'
#' @export
warpSP <- function(alignment, whom = NA) {

  ## find out whom to warp from the alignment:
  if (is.na(whom)) {
    whom <- alignment$openEndType
  }
  if (alignment$direction == "topDown") whom <- paste0(whom, "TopDown")

  q <- alignment$query
  r <- alignment$reference

  ## --- Warp QUERY ----------------------------------------------------------------------------------------------------
  if (whom %in% c("query", "jmin")) {
    ## warping approach analogously to example(dtw) using dtw-object index vectors:
    qmod <- q[names(q) != "layers"]
    qmodHeight <- r$layers$height[alignment$index2]
    qmod$hs <- max(qmodHeight)
    qmod$maxObservedDepth <- r$layers$depth[alignment$index2][1] + r$layers$thickness[alignment$index2][1]
    qmod$layers <- snowprofileLayers(gtype = q$layers$gtype[alignment$index1],
                                     hardness = q$layers$hardness[alignment$index1],
                                     queryLayerIndex = alignment$index1,
                                     height = qmodHeight,
                                     maxObservedDepth = qmod$maxObservedDepth,
                                     hs = qmod$hs,
                                     validate = FALSE)
    class(qmod) <- class(q)
    qmod <- rmZeroThicknessLayers(qmod)
    alignment$queryWarped <- qmod

  } else if (whom == "imin") {
    ## OE warp that matched the subsequence of the query profile:
    ## i.e. warp query, but stack non-matched layers on top
    qmod <- q[names(q) != "layers"]
    qmodHeight <- q$layers$height[alignment$index2]
    qmodStackedHeightFINAL <- c(qmodHeight,
                                qmodHeight[length(qmodHeight)] - q$layers$height[alignment$imin] + q$layers$height[-(1:alignment$imin-1)])
    qmod$maxObservedDepth <- min(r$maxObservedDepth + max(qmodStackedHeightFINAL) - max(qmodHeight), max(qmodStackedHeightFINAL), na.rm = TRUE)
    qmod$layers <- snowprofileLayers(gtype = as.factor(c(as.character(q$layers$gtype[alignment$index1]),
                                                    as.character(q$layers$gtype[-(1:alignment$imin-1)]))),
                                     hardness = c(q$layers$hardness[alignment$index1],
                                                  q$layers$hardness[-(1:alignment$imin-1) ]),
                                     queryLayerIndex = c(alignment$index1, seq(nrow(q$layers))[-(1:alignment$imin-1)]),
                                     height = qmodStackedHeightFINAL,
                                     maxObservedDepth = qmod$maxObservedDepth,
                                     validate = FALSE)
    class(qmod) <- class(q)
    qmod <- rmZeroThicknessLayers(qmod)
    qmod$hs <- max(qmod$layers$height)  # in this case, it is easier to retrieve hs after creating the layers..
    alignment$queryWarped <- qmod

    ## :::::::::::::::::::::::::::::::::::::::::::::::::::
    ## Top Down
    ## :::::::::::::::::::::::::::::::::::::::::::::::::::
  } else if (whom %in% c("queryTopDown", "jminTopDown")) {
    ## need to reverse vectors in top down:
    qmod <- q[names(q) != "layers"]
    qmodHeight <- rev(r$layers$height[alignment$index2])
    qmod$hs <- max(qmodHeight)
    if (hasUnobservedBasalLayer(r)) qmod$maxObservedDepth <- rev(r$layers$depth[alignment$index2])[2] + rev(r$layers$thickness[alignment$index2])[2]
    else qmod$maxObservedDepth <- rev(r$layers$depth[alignment$index2])[1] + rev(r$layers$thickness[alignment$index2])[1]
    qmod$layers <- snowprofileLayers(gtype = rev(q$layers$gtype[alignment$index1]),
                                     hardness = rev(q$layers$hardness[alignment$index1]),
                                     queryLayerIndex = rev(alignment$index1),
                                     height = qmodHeight,
                                     maxObservedDepth = qmod$maxObservedDepth,
                                     hs = qmod$hs,
                                     validate = FALSE)
    class(qmod) <- class(q)
    qmod <- rmZeroThicknessLayers(qmod)

    alignment$queryWarped <- qmod


  } else if (whom == "iminTopDown") {
    ## need to reverse vectors and undo stacking:
    qmod <- q[names(q) != "layers"]
    qmodHeight <- rev(q$layers$height[alignment$index2])
    qmod$hs <- max(qmodHeight)
    if (hasUnobservedBasalLayer(q)) qmod$maxObservedDepth <- rev(q$layers$depth[alignment$index2])[2] + rev(q$layers$thickness[alignment$index2])[2]
    else qmod$maxObservedDepth <- rev(q$layers$depth[alignment$index2])[1] + rev(q$layers$thickness[alignment$index2])[1]
    qmod$layers <- snowprofileLayers(gtype = rev(q$layers$gtype[alignment$index1]),
                                     hardness = rev(q$layers$hardness[alignment$index1]),
                                     queryLayerIndex = rev(alignment$index1),
                                     height = qmodHeight,
                                     maxObservedDepth = qmod$maxObservedDepth,
                                     hs = qmod$hs,
                                     validate = FALSE)
    class(qmod) <- class(q)
    qmod <- rmZeroThicknessLayers(qmod)

    alignment$queryWarped <- qmod

    ## --- Warp REF ----------------------------------------------------------------------------------------------------
  } else if (whom %in% c("ref", "reference")) {
    warning("warpSP: warping the reference object is currently in beta stage, check whether output makes sense!")
    ## analogous to "query" (but with indices swapped):
    rmod <- r[names(r) != "layers"]
    rmodHeight <- q$layers$height[alignment$index1]
    rmod$hs <- max(rmodHeight)
    rmod$maxObservedDepth <- q$layers$depth[alignment$index1][1] + q$layers$thickness[alignment$index1][1]
    rmod$layers <- snowprofileLayers(gtype = r$layers$gtype[alignment$index2],
                                     hardness = r$layers$hardness[alignment$index2],
                                     refLayerIndex = alignment$index2,
                                     height = rmodHeight,
                                     maxObservedDepth = rmod$maxObservedDepth,
                                     hs = rmod$hs,
                                     validate = FALSE)
    class(rmod) <- class(r)
    rmod <- rmZeroThicknessLayers(rmod)

    alignment$referenceWarped <- rmod


  } else {
    stop(paste0("warpSP: Don't know whom to warp! -> ", whom, "?"))
  }

  ## --- RETURN ----
  if (!inherits(alignment, "dtwSP")) class(alignment) <- append("dtwSP", class(alignment))

  ## clean up data.frame rownames and append other layer properties to warped profile
  if ("queryWarped" %in% names(alignment)) {
    rownames(alignment$queryWarped$layers) <- seq(nrow(alignment$queryWarped$layers))
    if ("ddate" %in% names(alignment$query$layers)) alignment$queryWarped$layers$ddate <- alignment$query$layers$ddate[alignment$queryWarped$layers$queryLayerIndex]
    if ("bdate" %in% names(alignment$query$layers)) alignment$queryWarped$layers$bdate <- alignment$query$layers$bdate[alignment$queryWarped$layers$queryLayerIndex]
    if ("gsize" %in% names(alignment$query$layers)) alignment$queryWarped$layers$gsize <- alignment$query$layers$gsize[alignment$queryWarped$layers$queryLayerIndex]
    if ("density" %in% names(alignment$query$layers)) alignment$queryWarped$layers$density <- alignment$query$layers$density[alignment$queryWarped$layers$queryLayerIndex]
    if ("temperature" %in% names(alignment$query$layers)) alignment$queryWarped$layers$temperature <- alignment$query$layers$temperature[alignment$queryWarped$layers$queryLayerIndex]
    if ("ogs" %in% names(alignment$query$layers)) alignment$queryWarped$layers$ogs <- alignment$query$layers$ogs[alignment$queryWarped$layers$queryLayerIndex]
    if ("tsa" %in% names(alignment$query$layers)) alignment$queryWarped$layers$tsa <- alignment$query$layers$tsa[alignment$queryWarped$layers$queryLayerIndex]
    if ("tsa_interface" %in% names(alignment$query$layers)) alignment$queryWarped$layers$tsa_interface <- alignment$query$layers$tsa_interface[alignment$queryWarped$layers$queryLayerIndex]
    if ("rta" %in% names(alignment$query$layers)) alignment$queryWarped$layers$rta <- alignment$query$layers$rta[alignment$queryWarped$layers$queryLayerIndex]
    if ("rta_interface" %in% names(alignment$query$layers)) alignment$queryWarped$layers$rta_interface <- alignment$query$layers$rta_interface[alignment$queryWarped$layers$queryLayerIndex]
    if ("layerOfInterest" %in% names(alignment$query$layers)) alignment$queryWarped$layers$layerOfInterest <- alignment$query$layers$layerOfInterest[alignment$queryWarped$layers$queryLayerIndex]
    if ("scalingFactor" %in% names(alignment$query$layers)) alignment$queryWarped$layers$scalingFactor <- alignment$query$layers$scalingFactor[alignment$queryWarped$layers$queryLayerIndex]
    if ("p_unstable" %in% names(alignment$query$layers)) alignment$queryWarped$layers$p_unstable <- alignment$query$layers$p_unstable[alignment$queryWarped$layers$queryLayerIndex]
    if ("slab_rhogs" %in% names(alignment$query$layers)) alignment$queryWarped$layers$slab_rhogs <- alignment$query$layers$slab_rhogs[alignment$queryWarped$layers$queryLayerIndex]
  } else if ("referenceWarped" %in% names(alignment)) {
    rownames(alignment$referenceWarped$layers) <- seq(nrow(alignment$referenceWarped$layers))
  }

  return(alignment)
}
