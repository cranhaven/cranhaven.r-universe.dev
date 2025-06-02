#' A function primarily aimed at developers
#' The original 'localMaximum' from the MassSpecWavelet package, which is now 'localMaximumR',
#'   calls 'localMaximumSlidingWindowR' to address bugs in the original 'localMaximumSlidingWindow'
#'   C function in the MassSpecWavelet package.
#' @noRd

getLocalMaximumCWTR <- function(wCoefs, minWinSize = 5, amp.Th = 0, isAmpThreshRelative = FALSE, exclude0scaleAmpThresh = FALSE) {
    localMax <- matrix(NA_integer_, nrow = nrow(wCoefs), ncol = ncol(wCoefs))
    scales <- as.numeric(colnames(wCoefs))

    if (isTRUE(isAmpThreshRelative)) {
        if (isTRUE(exclude0scaleAmpThresh) && isTRUE("0" %in% colnames(wCoefs))) {
            amp.Th <- max(wCoefs[,colnames(wCoefs) != "0", drop = FALSE]) * amp.Th
        } else {
            amp.Th <- max(wCoefs) * amp.Th
        }
    }

    for (i in seq_along(scales)) {
        scale.i <- scales[i]
        winSize.i <- scale.i * 2 + 1
        if (winSize.i < minWinSize) {
            winSize.i <- minWinSize
        }
        localMax[, i] <- localMaximumR(wCoefs[, i], winSize.i)
    }
    # Set the values less than peak threshold as 0
    localMax[wCoefs < amp.Th] <- 0L
    colnames(localMax) <- colnames(wCoefs)
    rownames(localMax) <- rownames(wCoefs)
    return(localMax)
}
