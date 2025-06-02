#' A function primarily aimed at developers
#' The original 'getLocalMaximumCWT' from the MassSpecWavelet package has been revised
#' and renamed to 'getLocalMaximumCWTR' to address issues in one of its downstream functions.
#' @noRd

# Ensure that the required functions are loaded from your NMRphasing package
# and other necessary packages (e.g., signal for baseline estimation)

#' @importFrom signal sgolayfilt
#' @importFrom MassSpecWavelet prepareWavelets
#' @importFrom MassSpecWavelet cwt
#' @importFrom MassSpecWavelet getRidge
#' @importFrom MassSpecWavelet tuneInPeakInfo
#' @importFrom MassSpecWavelet identifyMajorPeaks

peakDetectionCWTR <- function(ms, scales = c(1, seq(2, 30, 2), seq(32, 64, 4)), SNR.Th = 3, nearbyPeak = TRUE,
                             peakScaleRange = 5, amp.Th = 0.01, minNoiseLevel = amp.Th / SNR.Th, ridgeLength = 24,
                             peakThr = NULL, tuneIn = FALSE, ..., exclude0scaleAmpThresh = FALSE,
                             getRidgeParams = list(gapTh = 3, skip = 2)) {
    otherPar <- list(...)
    if (minNoiseLevel > 1) names(minNoiseLevel) <- "fixed"
    ## Perform Continuous Wavelet Transform
    prep_wav <- MassSpecWavelet::prepareWavelets(
        mslength = length(ms),
        scales = scales,
        wavelet = "mexh",
        wavelet_xlimit = 8,
        wavelet_length = 1024L,
        extendLengthScales = FALSE
    )

    wCoefs <- MassSpecWavelet::cwt(ms, prep_wav)
    scales <- prep_wav$scales

    ## Attach the raw data as the zero level of decomposition
    wCoefs <- cbind(as.vector(ms), wCoefs)
    colnames(wCoefs) <- c(0, scales)

    ## -----------------------------------------
    ## Identify the local maximum by using a slide window
    ## The size of slide window changes over different levels, with the coarse level have bigger window size
    if (is.null(amp.Th)) {
        amp.Th <- 0
    } else if (is.null(names(amp.Th)) || names(amp.Th) != "fixed") {
        if (isTRUE(exclude0scaleAmpThresh)) {
            amp.Th <- max(wCoefs[,colnames(wCoefs) != "0", drop = FALSE]) * amp.Th
        } else {
            amp.Th <- max(wCoefs) * amp.Th
        }
    }
    localMax <- getLocalMaximumCWTR(wCoefs, amp.Th = amp.Th)
    colnames(localMax) <- colnames(wCoefs)

    ## In order to fastern the calculation, we can filter some local maxima with small amplitude
    ## In this case a baseline estimation was performed.
    if (!is.null(peakThr)) {
        if (!requireNamespace("signal", quietly = TRUE)) {
            stop('The use of peakThr= argument in MassSpecWavelet::peakDetectionCWT() requires to install the "signal" package. Please use install.packages("signal")')
        }
        if ("fl" %in% names(otherPar)) {
            filterLength <- otherPar$fl
        } else {
            filterLength <- 1001
        }
        if (filterLength %% 2 == 0) {
            warning("filter length in peakDetectionCWT(fl=) needs to be odd (increasing it by 1)")
            filterLength <- filterLength + 1
        }
        if ("forder" %in% names(otherPar)) {
            fOrder <- otherPar$forder
        } else {
            fOrder <- 2
        }
        ## Baseline estimation using Savitzky Golay Filter
        ## this part was added by Steffen Neumann
        sg <- signal::sgolayfilt(ms, p = fOrder, n = filterLength)
        localMax[(ms - sg) < peakThr, ] <- 0
    }
    ## remove the parameters in otherPar that were passed to the Savitzky-Golay filter
    otherPar <- otherPar[!(names(otherPar) %in% c("fl", "forder", "dorder"))]

    ## -----------------------------------------
    ## Identify the ridges from coarse level to more detailed levels
    ridgeList <- do.call(MassSpecWavelet::getRidge, c(list(localMax), getRidgeParams))

    ## -----------------------------------------
    ## Identify the major peaks and their nearby peaks
    majorPeakInfo <- do.call(MassSpecWavelet::identifyMajorPeaks, c(list(
        ms = ms, ridgeList = ridgeList, wCoefs = wCoefs, SNR.Th = SNR.Th, peakScaleRange = peakScaleRange,
        nearbyPeak = nearbyPeak, minNoiseLevel = minNoiseLevel, ridgeLength = ridgeLength
    ), otherPar))

    if (tuneIn) {
        refinedPeakInfo <- MassSpecWavelet::tuneInPeakInfo(ms, majorPeakInfo)
        return(list(majorPeakInfo = refinedPeakInfo, ridgeList = ridgeList, localMax = localMax, wCoefs = wCoefs[, -1], oldPeakInfo = majorPeakInfo))
    } else {
        return(list(majorPeakInfo = majorPeakInfo, ridgeList = ridgeList, localMax = localMax, wCoefs = wCoefs[, -1]))
    }
}
