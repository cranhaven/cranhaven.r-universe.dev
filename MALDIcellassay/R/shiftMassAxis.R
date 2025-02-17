#' Shift mass axis
#'
#' @param spec    List of MALDIquant::MassSpectrum or MALDIquant::MassPeaks
#' @param mzdiff  Numeric vector, see getMzShift()
#'
#' @return List of MALDIquant::MassSpectrum or MALDIquant::MassPeaks with shifted mass axis.
#'
#' @importFrom MALDIquant isMassSpectrum isMassPeaks isMassSpectrumList isMassPeaksList
#' @export
#' @examples
#' data(Blank2022spec)
#' # raw mz
#' head(Blank2022spec[[1]]@mass)
#' 
#' # shifted mz
#' shifted <-shiftMassAxis(Blank2022spec[1:2], c(0.5, 0.5))
#' head(shifted[[1]]@mass)
#' 
shiftMassAxis <- function(spec, mzdiff) {
  if (isMassSpectrum(spec) || isMassPeaks(spec)) {
    spec@mass <- spec@mass + mzdiff
    return(spec)
  }

  if (isMassSpectrumList(spec) || isMassPeaksList(spec)) {
    if (!(length(spec) == length(mzdiff))) {
      stop("length(spec) != length(mzdiff) !\n")
    }
    for (i in 1:length(spec)) {
      spec[[i]]@mass <- spec[[i]]@mass + mzdiff[i]
    }
    return(spec)
  }
  stop("spec needs to be a MALDIquant::MassSpectrum or MALDIquant::MassPeaks or a list of these. \n")
}
