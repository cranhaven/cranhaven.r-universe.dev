#' Backtrack layers from average or summary profile
#'
#' An average profile as computed by [dbaSP] summarizes the prevalent layer properties of the entire profile set. To better
#' understand the distribution of layer properties within the set, use this function to retrieve layers of interest from the individual profiles of the original profile set.
#'
#' @param avgProfile an average profile as per [dbaSP]
#' @param profileSet the profile set that is averaged by `avgProfile`. Optimally, it is the resampled profile set as returned by [dbaSP] or [averageSP], see parameter `computationByHeight`
#' if that resampled profile set is not available anymore.
#' @param layer the height or row number of the layer to retrieve the distribution for (given as height or row number of the average profile). If layer
#' is `NA`, all layers from the avgProfile are considered.
#' @param layer_units either `"row#"` or `"cm"`
#' @param condition a condition that subsets which layers are returned. E.g., only layers with a specific grain type, etc.. Note that the condition needs to be substituted in the
#' function call, e.g. `condition = substitute(gtype == "SH")`. In most cases, it's best to subset the data.frame manually after this function has been called. A *secret* and *dangerous* trick is
#' to use `condition = substitute(gtype %in% return_conceptually_similar_gtypes(as.character(avgProfile$layers$gtype[lidx])))` to get the very same layers that have been used to compute the median
#' layer properties which are included in the avgProfile$layers.
#' @param computationByHeight There are two ways of how to backtrack layers that were aligned to `avgProfile$layers`. The first and safest approach is by index, which requires the
#' resampled `profileSet` as returned by [dbaSP] or [averageSP]. The second approach is by layer height, which should yield the same results (beta phase: still bugs possible, check yourself!)
#' and allows to backtrack the layers even if the resampled profileSet is not available anymore, but only the original unmodified set which was used to create the average profile.
#'
#' @return This function returns a list of data.frames with the backtracked layers. Each (named) list item corresponds to a specific layer height (cm).
#'
#' @examples
#' ## See Vignette for examples.
#'
#' @author fherla
#'
#'
#'
#' @export
backtrackLayers <- function(avgProfile, layer = NA, profileSet = NULL, layer_units = "row#", condition = NULL, computationByHeight = FALSE) {

  ## ---assertions & initializations----
  if (is.null(profileSet)) {
    if (inherits(avgProfile, "avgSP")) {
      if ("set" %in% names(avgProfile)) profileSet <- avgProfile$set
      else stop("avgProfile is of class avgSP, but does not contain a set. Please provide a profileSet!")
    } else {
      stop("Please provide a profileSet!")
    }
  }
  if (inherits(avgProfile, "avgSP")) avgProfile <- avgProfile$avg
  if (!is.snowprofile(avgProfile)) stop("avgProfile needs to be a snowprofile or avgSP object")
  if (avgProfile$type != "aggregate") stop("avgProfile needs to be of type 'aggregate'")
  if (!"backtrackingTable" %in% names(avgProfile)) stop("avgProfile has no layer 'backtrackingTable'")
  if (!is.snowprofileSet(profileSet)) stop("profileSet needs to be a snowprofileSet")

  if (layer_units == "row#") layerID <- layer
  else if (layer_units == "cm") layerID <- avgProfile$backtrackingTable$layerID[which(match_with_tolerance(avgProfile$backtrackingTable$height, layer))]
  else stop("layer_units need to be 'row#' or 'cm'")

  if (all(is.na(layer))) layerID <- seq(nrow(avgProfile$layers))
  layerID <- unique(layerID)

  ## ---retrieve backtracked layers----

  height_list <- lapply(layerID, function(lidx) {
    ## corresponding layers of profile set:
    layerMaps <- avgProfile$backtrackingTable[avgProfile$backtrackingTable$layerID == lidx, ]
    backtrackedLayers <- data.table::rbindlist(lapply(seq(nrow(layerMaps)), function(i) {
      ## Two computation methods
      ## by height of backtracked layers: unsafer, but possible with non-resampled original profileSet
      if (computationByHeight) {
        mostLikelyThisLayer <- tryCatch({
          min(which(profileSet[[layerMaps$queryID[i]]][["layers"]][["height"]] >= layerMaps$queryLayerHeight[i]))  # lowest layer >= resampled queryLayerHeight
        }, warning = function(warn) {  # doesn't exist --> = surface layer, which got pruned during resampling
          max(which(profileSet[[layerMaps$queryID[i]]][["layers"]][["height"]] <= layerMaps$queryLayerHeight[i]))  # highest layer <= resampled queryLayerHeight
        }, error = function(err) {
          message("Error occurred:")
          message(err)
          message("Investigate interactively! Debugging mode started: ")
          browser()
        })
        cbind(data.table::as.data.table(profileSet[[layerMaps$queryID[i]]][c("station", "datetime", "zone", "elev", "band",
                                                                 "angle", 'aspect', "hs", "station_id", "date")]),  # profile meta data
              profileSet[[layerMaps$queryID[i]]][["layers"]][mostLikelyThisLayer, ])                                # layer details
      } else {
      ## via queryLayerID: super safe, but resampled $set from averageSP necessary!
        cbind(data.table::as.data.table(profileSet[[layerMaps$queryID[i]]][c("station", "datetime", "zone", "elev", "band",
                                                                 "angle", 'aspect', "hs", "station_id", "date")]),  # profile meta data
              profileSet[[layerMaps$queryID[i]]][["layers"]][layerMaps$queryLayerID[i], ])                          # layer details
      }


    }), fill = TRUE)
    if (!is.null(condition)) backtrackedLayers <- subset(backtrackedLayers, eval(condition))
    return(as.data.frame(backtrackedLayers))
  })

  names(height_list) <- avgProfile$layers$height[layerID]
  return(height_list)
}
