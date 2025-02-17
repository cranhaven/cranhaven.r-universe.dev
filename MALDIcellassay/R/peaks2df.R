#' Convert a list of peaks to a data.frame
#'
#' @param peaks (list of) MALDIquant::MassPeaks
#'
#' @return  Data.frame with peak data
#' @export
#' @examples
#' data(Blank2022peaks)
#' 
#' peakdf <- peaks2df(Blank2022peaks[1:2])
#' head(peakdf)
#' 
peaks2df <- function(peaks) {
  if (!MALDIquant::isMassPeaksList(peaks)) {
    if (!MALDIquant::isMassPeaks(peaks)) {
      stop("No peaks provided.\n")
    }
  }

  if (is.null(names(peaks))) {
    names(peaks) <- rep("noName", length(peaks))
  }


  peak_df <- data.frame(
    ID = character(),
    plotIdx = integer(),
    peakIdx = character(),
    mz = double(),
    int = double(),
    SNR = double()
  )

  for (i in 1:length(peaks)) {
    df <- data.frame(
      ID = paste0(names(peaks[i])),
      plotIdx = i,
      peakIdx = paste0(i, "_", 1:length(MALDIquant::mass(peaks[[i]]))),
      mz = MALDIquant::mass(peaks[[i]]),
      int = MALDIquant::intensity(peaks[[i]]),
      SNR = MALDIquant::snr(peaks[[i]])
    )
    peak_df <- rbind(df, peak_df)
  }
  return(peak_df)
}
