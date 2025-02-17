#' Extract intensity using peaks as template
#'
#' @param mz    numeric, mz values to be extracted from the peaks/spectra
#' @param peaks MALDIquant::MassPeaks list
#' @param spec  MALDIquant::MassSpectrum list
#' @param tol   numeric, tolerance in Da
#'
#' @return MALDIquant::MassPeaks list with extracted intensities from spec at m/z of peaks = pseudo peaks.
#' Useful in combination with sdMassSpectrum to get standard deviation of peaks as intensity matrix.
#' @export
#' 
#' @importFrom MALDIquant snr
#' @examples
#' data(Blank2022peaks)
#' data(Blank2022spec)
#' 
#' int <- extractIntensity(mz = c(409, 423, 440), 
#'                         peaks = Blank2022peaks, 
#'                         spec = Blank2022spec, 
#'                         tol = 0.2)
#' head(int)
extractIntensity <- function(mz, peaks, spec, tol) {
  if(!(length(peaks) == length(spec))) {
    stop("length of peaks and spec must match.\n")
  }

  res_peaks <-purrr::map(seq_along(peaks),
                         function(i) {
                           mz_peaks <- mass(peaks[[i]])
                           mz_spec <- mass(spec[[i]])

                           peak_mz_idx <- match.closest(x = mz,
                                                        table = mz_peaks,
                                                        tolerance = tol)
                           spec_mz_idx <- match.closest(x = mz,
                                                        table = mz_spec,
                                                        tolerance = Inf)

                           # extract intensity from peaks if possible
                           # otherwise use noise from spectra
                           # resulting in a dense intensity matrix
                           new_int <- ifelse(!is.na(peak_mz_idx),
                                             yes = intensity(peaks[[i]])[peak_mz_idx],
                                             no = intensity(spec[[i]])[spec_mz_idx])

                           new_snr <- ifelse(!is.na(peak_mz_idx),
                                             yes = snr(peaks[[i]])[peak_mz_idx],
                                             no = NA_integer_)

                           createMassPeaks(mass = mz,
                                           intensity = new_int,
                                           snr = new_snr,
                                           metaData = metaData(peaks[[i]]))
                         })
  names(res_peaks) <- names(peaks)
  return(res_peaks)
}
