#' A function rather aimed at developers
#' @noRd
#'
peakSearch=function(datin){ ### datin is a vector
  peakInfo = peakDetectionCWTR(datin)
  majorPeakInfo = peakInfo$majorPeakInfo
  peakIndex = majorPeakInfo$peakIndex
  return(peakIndex)
}
