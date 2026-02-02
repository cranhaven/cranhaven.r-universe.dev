#' Remove layers with a thickness of 'zero cm'
#'
#' Find layers in a snow profile that are zero cm thick (i.e. height vector stays constant for some layers, even
#' though grain types or hardness may change). Then, either remove those layers, or reset them with the layer
#' characteristics of the lower adjacent (non-zero-thickness) layer. In the latter case (i.e., reset), the number of
#' layers won't change, but those non-zero thickness layers will be made ineffective.
#' This procedure is particularly necessary for warping snow profiles (cf., [dtwSP], [warpSP]).
#'
#' @param x A `snowprofile` or `snowprofileLayers` object
#' @param rm.zero.thickness Want to remove zero-thickness layers from profile? boolean, default TRUE. If FALSE, those
#' zero-thickness layers will be reset to the lower adjacent (non-zero-thickness) layer; thus, the number of layers
#' won't be changed.
#'
#' @return A modified copy of the input object. For snowprofile objects, the field `$changes` will be initialized or
#' extended.
#'
#' @author fherla
#'
#' @export
rmZeroThicknessLayers <- function(x, rm.zero.thickness = TRUE) {

  ## --- assertions and initialization ----
  ## class assertion and layer assignment
  if (is.snowprofile(x)) {
    layers <- x$layers
  } else if (is.snowprofileLayers(x)) {
    layers <- x
  } else {
    stop("x needs to be a snowprofile or snowprofileLayers object!")
  }

  ## --- remove/reset redundant layers ----
  ## preliminary indices of redundant layers to keep
  keep.doubles <- which(diff(layers$height) == 0)

  if (length(keep.doubles) == 0) {  ## keep.doubles is empty if there are no redundant layers --> routine finished
    return(x)

  } else {  ## there are indeed redundant layers
    ## indices of redundant layers to remove/reset, i.e. next (= upper) layer from keep.doubles
    rms.doubles <- keep.doubles + 1
    ## removing is trivial:
    if (rm.zero.thickness) {
      layers <- layers[-rms.doubles, ]
      detail <- "(layers removed)"
    }
    ## resetting is more work:
    ## implement with loop (couldn't find better solution), but use an "intelligent" loop that skips non-necessary steps:
    else {
      tmp_map <- match(diff(keep.doubles), 1)  # keep indices of the first 1 of the multiple sequences of 1s in tmp_map!
                                               # subsequent 1s represent stacked zero-thickness layers

      ## initialize counter & max counter value and then LOOP:
      imx <- length(tmp_map)
      i <- 1
      while (i < imx) {
        tmp_keep <- (i -1) + match(1, tmp_map[i:imx])  # first 1 in section of tmp_map
        if (is.na(tmp_keep)) i <- imx + 1  # i.e. exit loop
        else {
          ## subsequent first NA in tmp_map:
          tmp_step <- tmp_keep + match(NA, tmp_map[(tmp_keep + 1):length(tmp_map)])
          if (is.na(tmp_step)) tmp_step <- imx + 1
          ## reset according values in keep.doubles:
          keep.doubles[tmp_keep:tmp_step] <- keep.doubles[tmp_keep]
          ## advance counter
          i <- tmp_step + 1
        }
      }
      ## reset redundant layers to next !non-zero-thickness! layer:
      layers[rms.doubles, ] <- layers[keep.doubles, ]
      detail <- "(layers overwritten)"
    }

    ## if x is a snowprofile object --> write information to "changes"
    ## and update x$layers with layers
    if (is.snowprofile(x)) {
      x$layers <- layers
      if ("changes" %in% names(x)) {
        old_changes <- paste0(x$changes, " -> ")
      } else  {
        old_changes <- ""
        x$changes <- paste0(old_changes, "unique ", detail)
      }
    } else {
      x <- layers
    }

    return(x)
  }
}
