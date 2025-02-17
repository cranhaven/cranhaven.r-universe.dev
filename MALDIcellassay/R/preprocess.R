#' Internal function to perform preprocessing of spectra
#'
#' @param peaks_single        List of MALDIquant::MassPeaks objects.
#' @param spec                List of MALDIquant::MassSpectrum objects.
#' @param SinglePointRecal    Logical, perform single point re-calibration
#' @param normMz              Numeric, m/z used for normalization and re-calibration.
#' @param normTol             Numeric, tolerance around `normMz` in Da.
#' @param normMeth            Character, normalization method. Options are: "TIC", "median", "PQN" and "mz".
#' @param alignTol            Numeric, tolerance for alignment in Da.
#' @param allowNoMatches      Logical, allow no matches for normalization using "mz" method and/or re-calibration.
#' @param verbose             Logical, print logs to the console.
#' 
#' @importFrom MALDIquant determineWarpingFunctions warpMassSpectra warpMassPeaks referencePeaks
#' @noRd
.preprocess <- function(peaks_single,
                        spec,
                        SinglePointRecal,
                        normMz,
                        normTol,
                        normMeth,
                        alignTol,
                        allowNoMatches,
                        verbose = TRUE) {
  if (SinglePointRecal) {
    # perform single point mass recalibration
    mzShift <- getMzShift(
      peaks = peaks_single,
      tol = normTol,
      targetMz = normMz,
      tolppm = FALSE, 
      verbose = verbose
    )

    spec <- shiftMassAxis(spec[mzShift$specIdx],
                          mzShift$mzshift)
    peaks_single <- shiftMassAxis(peaks_single[mzShift$specIdx],
                                  mzShift$mzshift)
    included_idx_recal <- mzShift$specIdx

  } else {
    mzShift <- list("mzshift" = 0)
    included_idx_recal <- 1:length(spec)
  }

  #### normalization ####
  if(verbose) {
    cat(timeNow(), "normalizing... \n")  
  }
  
  norm <- normalize(spec = spec,
                    peaks = peaks_single,
                    normMeth = normMeth,
                    normMz = normMz,
                    normTol = normTol)

  spec <- norm$spec
  peaks_single <- norm$peaks

  current_names <- names(spec)


  #### alignment ####
  if(alignTol > 0) {
    if(verbose) {
      cat(timeNow(), "aligning spectra... \n")
    }
    
    wf <- determineWarpingFunctions(l = peaks_single,
                                    reference = referencePeaks(peaks_single,
                                                               minFrequency = 0.75,
                                                               tolerance = 0.002),
                                    tolerance = alignTol,
                                    method = "linear",
                                    allowNoMatches = allowNoMatches)

    spec <- warpMassSpectra(spec,
                            w = wf,
                            emptyNoMatches = allowNoMatches)
    names(spec) <- current_names

    peaks_single <- warpMassPeaks(peaks_single,
                                  w = wf,
                                  emptyNoMatches = allowNoMatches)
    names(peaks_single) <- current_names
  }

  return(list(spec = spec,
              singlePeaks = peaks_single,
              idx = norm$idx,
              mzShift = mzShift$mzshift,
              normFac = norm$factor))
}
