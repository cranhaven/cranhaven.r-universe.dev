#' Detect peaks
#'
#' @param spec    MALDIquant::MassSpectrum or list thereof
#' @param SNR     Numeric, signal to noise value
#' @param method  Character, method see MALDIquant::detectPeaks
#' @param halfWindowSize Numeric, defines width of window for peak detection. See `MALDIquant::detectPeaks()`.
#'
#' @details
#' Just a wrapper around MALDIquant::detectPeaks to ensure that the returned peak list is named.
#'
#' @return
#' List of MALDIquant::MassPeaks with the same names as `spec`
#' @noRd
.detectPeaks <- function(spec, SNR, method = "SuperSmoother", halfWindowSize = 20) {

  if(!(isMassSpectrum(spec)| isMassSpectrumList(spec))) {
    if(isMassPeaks(spec) | isMassPeaksList(spec)) {
      message("Peaks are already present, skipping detection.\n")
      return(spec)
    }
    stop("No valid spectra or peaks found.\n")
  }

  peaks <- detectPeaks(spec,
                       SNR = SNR,
                       method = method,
                       halfWindowSize = halfWindowSize)
  names(peaks) <- names(spec)

  return(peaks)
}
