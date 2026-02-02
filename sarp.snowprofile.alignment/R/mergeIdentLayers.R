#' Merge layers with identical properties
#'
#' Merge adjacent layers that have identical properties, such as grain type, hardness etc..
#'
#' @import sarp.snowprofile
#'
#' @param x a snowprofile or snowprofileLayers object with *height* grid information
#' @param properties a character array of layer properties that are considered when searching for identical layers
#' (e.g., `hardness`, `gtype`, ...)
#'
#' @return A new `snowprofileLayers` object will be returned with the dimensions `height`, `hardness`, `gtype` and any other
#' properties given in 'properties'. Depth and thickness information will be auto-calculated. For snowprofile objects, the
#' field 'changes' will be initialized or extended.
#'
#' @author fherla
#'
#' @examples
#'
#' ## Merge identical layers based on hardness and grain type:
#' fewerLayers <- mergeIdentLayers(x = SPpairs$A_modeled, properties = c("hardness", "gtype"))
#' summary(SPpairs$A_modeled)[, c("hs", "nLayers")]
#' summary(fewerLayers)[, c("hs", "nLayers")]
#'
#' ## compare profile plots before and after merging (i.e., appear identical!)
#' opar <- par(no.readonly =TRUE)
#' par(mfrow = c(1, 2))
#' plot(SPpairs$A_modeled, main = "original", ylab = "Snow height")
#' plot(fewerLayers, main = "merged layers", ylab = "Snow height")
#' par(opar)
#'
#' @export

mergeIdentLayers <- function(x, properties = c("hardness", "gtype")) {

  ## --- assertions and initialization ----
  ## class assertion and layer assignment
  if (is.snowprofile(x)) {
    layers <- x$layers
  } else if (is.snowprofileLayers(x)) {
    layers <- x
  } else {
    stop("x needs to be a snowprofile or snowprofileLayers object!")
  }
  if (!"height" %in% names(layers)) stop("Layers object has no 'height'. Currently not yet implemented for 'depth' information.")
  if (!all(properties %in% names(layers))) stop("One or more layer properties are not availabe in the provided object!")

  ## --- merge ----
  ## calculate the indices of the rows that contain the maximum height of the layers that have identical properties
  np <- length(properties)
  nl <- nrow(layers)
  if (nl == 1) {
    # warning(paste0("mergeIdentLayers: Only one layer, nothing to merge."))
    return(x)
  }
  diffAr <- matrix(nrow = nl-1, ncol = np)
  for (i in seq(np)) {
    diffAr[, i] <- diff(layers[properties[i]][, 1])  # nl-1 x np array; zeros correspond to layers with constant properties
  }
  rowStd <- apply(diffAr, MARGIN = 1, sd_sample_uncorrected, na.rm = TRUE)
  redundants <- which(rowSums(diffAr) == 0 & rowStd == 0)  # reduce dimensions to one; zeros correspond to layers where all properties are constant
  layerSeq <- seq(nl)
  keepIndices <- layerSeq[!(layerSeq %in% redundants)]  # extract indices of non-redundant 'parent' layers

  ## --- new object ----
  ## construct new snowprofileLayers object
  layerFrameDims <- unique(c(c("height", "hardness", "gtype"), properties))
  layerFrame <- data.frame(lapply(layerFrameDims, function(x, layers) layers[x][keepIndices, 1], layers = layers))
  colnames(layerFrame) <- layerFrameDims
  newLayers <- snowprofileLayers(layerFrame = layerFrame, dropNAs = FALSE, validate = FALSE)

  ## if x is a snowprofile object --> write information to "changes"
  ## and update x$layers with layers
  if (is.snowprofile(x)) {
    x$layers <- newLayers  # update
    if ("changes" %in% names(x)) {
      old_changes <- paste0(x$changes, " -> ")
    } else  {
      old_changes <- ""
      x$changes <- paste0(old_changes, "mergeIdentLayers (", paste(properties, collapse = " & "), ")")
    }
  } else {
    x <- newLayers
  }

  return(x)
}
