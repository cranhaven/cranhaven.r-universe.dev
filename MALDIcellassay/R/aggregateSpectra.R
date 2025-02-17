#' Preprocessing function to average spectra, detect and bin peaks before turning them into an intensity matrix
#'
#' @param spec                List of MALDIquant::MassSpectrum
#' @param averageMethod       Character, method for aggregation: "mean", "median" or "sum"
#' @param SNR                 Numeric, Signal noise value for peak detection
#' @param monoisotopicFilter  Logical, filter monoisotopic peaks
#' @param binTol              Numeric, tolerance for binning
#' @param normMz              Numeric, m/z value used for normalization/re-calibration
#' @param normTol             Numeric, tolerance around `normMz` in Da.
#' @param halfWindowSize      Numeric, halfWindowSize for peak detection.
#' @param peakMethod          Character, method for peak detection. Either "SuperSmoother" or "MAD".
#' @param verbose             Logical, print logs to the console.
#'
#' @return
#' List of lists with intensity matrix, average spectra and average peaks
#' 
#' @importFrom MALDIquant monoisotopicPeaks
#' @noRd
.aggregateSpectra <- function(spec,
                              averageMethod,
                              SNR,
                              monoisotopicFilter,
                              binTol,
                              normMz,
                              normTol,
                              halfWindowSize,
                              peakMethod = "SuperSmoother",
                              verbose = TRUE) {
  nm <- names(spec)
  stopifnot(!is.null(nm))
  stopifnot(isMassSpectrumList(spec))
  
  avg_spec <- averageMassSpectra(spec,
                                 labels = nm,
                                 method = averageMethod)
  
  if(verbose) {
    cat(timeNow(),
        "building intensity matrix and applying variance filter... \n")
  }
  
  peaks <- .detectPeaks(avg_spec, method = peakMethod, 
                        SNR = SNR, 
                        halfWindowSize = halfWindowSize)
  
  if(monoisotopicFilter) {
    if(verbose) {
      cat(timeNow(),
          "Filtering monoisotopic peaks...\n")
    }
    
    # set it to be less restrictive then default settings
    peaks_mono <- monoisotopicPeaks(peaks,
                                    size = 2L:10L,
                                    minCor = 0.85,
                                    tolerance = 1e-3)
    
    # check if normMz is still in spectra
    # re-add it otherwise
    
    peaks <- unlist(
      map(seq_along(peaks_mono),
          function(i) {
            mz_mono <- mass(peaks_mono[[i]])
            idx_mono <- match.closest(x = normMz,
                                      table = mz_mono,
                                      tolerance = normTol)
            
            if(!is.na(idx_mono)) {
              # if normMz present use peaks as they are
              return(peaks_mono[i])
            }
            
            int_mono <- intensity(peaks_mono[[i]])
            snr_mono <- intensity(peaks_mono[[i]])
            int <- intensity(peaks[[i]])
            snr <- snr(peaks[[i]])
            
            # get mz idx from peaks before monoisotopic filter
            mz <- mass(peaks[[i]])
            idx <- match.closest(x = normMz,
                                 table = mz,
                                 tolerance = normTol)
            
            if(is.na(idx)) {
              # if normMz is also missing on original data
              # return the unchanged monoisotopic peaks
              return(peaks_mono[[i]])
            }
            
            # re-add normMz to end of mz/intensity/snr vector
            mz_mono <- append(mz_mono, mz[idx])
            int_mono <- append(int_mono, int[idx])
            snr_mono <- append(snr_mono, snr[idx])
            
            # order vector accending
            ord <- order(mz_mono)
            
            res <- createMassPeaks(mass = mz_mono[ord],
                                   intensity = int_mono[ord],
                                   snr = snr_mono[ord],
                                   metaData = metaData(peaks_mono[[i]]))
            
            return(res)
          })
    )
  }
  
  peaksBinned <- binPeaks(peaks, tolerance = binTol)
  
  # perform variance filtering
  intmat <- intensityMatrix(peaksBinned, avg_spec)
  
  if(verbose) {
    cat("      Found", dim(intmat)[2], "peaks in total.\n")
  }
  
  rownames(intmat) <- names(avg_spec)
  
  return(list(intmat = intmat,
              avgPeaksBinned = peaksBinned,
              avgSpec = avg_spec))
}
