#' Fit dose-response curves
#'
#' @param spec                List of MALDIquant::MassSpectrum
#' @param varFilterMethod     Character, function applied for high variance filtering. One of the following options `mean` (default), `median`, `q25`, `q75` or `none` (no filtering).
#' @param unit                Character, unit of concentration. Used to calculate the concentration in Moles so that pIC50 is correct.
#'                            Set to "M" if you dont want changes in your concentrations.
#' @param monoisotopicFilter  Logical, filter peaks and just use monoisotopic peaks for curve fit.
#' @param averageMethod       Character, aggregation method for average mass spectra ("mean" or "median")
#' @param normMz              Numeric, mz used for normalization AND for single point recalibration.
#' @param normTol             Numeric, tolerance in Dalton to match normMz
#' @param alignTol            Numeric, tolerance for spectral alignment in Dalton.
#' @param binTol              Numeric, tolerance for binning of peaks.
#' @param SNR                 Numeric, signal to noise ratio for peak detection.
#' @param halfWindowSize      2ction. See `MALDIquant::detectPeaks()`.
#' @param allowNoMatches      Logical, if normMz can not be found in a spectrum, proceed and exclude spectrum or stop
#' @param normMeth            Character, normalization method. Can either be "TIC", "PQM", "median" or "mz". If "mz" then the normMz is used. If none no normalization is done.
#' @param SinglePointRecal    Logical, perform single point recalibration to normMz
#' @param verbose             Logical, print logs to console.
#'
#' @return
#' Object of class `MALDIassay`.
#' The most important slot is `fits` which contains the IC50 curve fits.
#' @export
#'
#' @importFrom MALDIquant removeBaseline calibrateIntensity alignSpectra averageMassSpectra detectPeaks binPeaks intensityMatrix match.closest createMassPeaks metaData
#' @importFrom nplr nplr convertToProp getXcurve getYcurve getFitValues getX getY getEstimates getGoodness
#' @importFrom dplyr summarise mutate group_by %>% arrange left_join rename bind_rows filter pull slice_head slice_tail
#' @importFrom tibble tibble as_tibble
#' @importFrom tidyr gather
#' @importFrom methods new
#' @importFrom stats var
#' @importFrom ggplot2 ggplot geom_line geom_point scale_x_continuous theme_bw theme element_text labs aes ggsave geom_vline
#' 
#' @examples
#' data(Blank2022spec)
#' 
#' fitCurve(spec = Blank2022spec,
#'          SinglePointRecal = TRUE, 
#'          normMz = 760.585, 
#'          alignTol = 0.1, 
#'          normTol = 0.1,
#'          varFilterMethod = "mean") 

fitCurve <- function(spec,
                     unit = c("M", "mM", "uM", "nM", "pM", "fM"),
                     varFilterMethod = c("mean", "median", "q25", "q75", "none"),
                     monoisotopicFilter = FALSE,
                     averageMethod = c("mean", "median", "sum"),
                     normMz = NULL,
                     normTol = 0.1,
                     alignTol = 0.01,
                     binTol = 0.0002,
                     SNR = 3,
                     halfWindowSize = 3,
                     allowNoMatches = TRUE,
                     normMeth = c("mz", "TIC", "PQN", "median", "none"),
                     SinglePointRecal = TRUE,
                     verbose = TRUE) {

  ##### match & evaluate arguments ####
  normMeth <- match.arg(normMeth)
  unit <- match.arg(unit)
  averageMethod <- match.arg(averageMethod)
  varFilterMethod <- match.arg(varFilterMethod)

  unitFactor <- switch (unit,
                        "M" = 1,
                        "mM" = 1e-3,
                        "uM" = 1e-6,
                        "nM" = 1e-9,
                        "pM" = 1e-12,
                        "fM" = 1e-15
  )

  if(normMeth == "mz" & is.null(normMz)) {
    stop("Normalization to m/z is not possible when no m/z was supplied.\n")
  }


  # check if spectra are named
  if(length(names(spec)) < 1) {
    stop("Spectra are not named.
         Name spectra with concentrations\n")
  }


  # check if spectra names are concentrations
  if(any(is.na(as.numeric(names(spec))))) {
    stop("No concentrations provided.
         Name spectra with concentrations.")
  }

  names(spec) <- as.numeric(names(spec)) * unitFactor

  nm <- names(spec)

  # check spectra for problematic meta data and remove it of needed
  spec <- .repairMetaData(spec)

  # make sure that spectra are in ascending order in regards to concentration
  order <- order(as.numeric(nm))
  nm <- nm[order]
  spec <- spec[order]


  if(!length(nm) == length(spec)) {
    stop("No concentrations provided.
         Either name spectra with concentrations or use 'conc' argument.")
  }

  spots <- extractSpots(spec)

  #### re-calibration ####
  peaks_single <- .detectPeaks(spec, SNR = SNR, method = "SuperSmoother", halfWindowSize = halfWindowSize)

  prc <- .preprocess(peaks_single = peaks_single,
                     spec = spec,
                     SinglePointRecal = SinglePointRecal,
                     normMz = normMz,
                     normTol = normTol,
                     normMeth = normMeth,
                     alignTol = alignTol,
                     allowNoMatches = allowNoMatches, 
                     verbose = verbose)

  #### average spectra ####
  if(verbose) {
    cat(timeNow(), "calculating", averageMethod, "spectra... \n")
  }
  

  avg <- .aggregateSpectra(spec = prc$spec,
                           averageMethod = averageMethod,
                           SNR = SNR,
                           monoisotopicFilter = monoisotopicFilter,
                           binTol = binTol,
                           normMz = normMz,
                           normTol = normTol,
                           halfWindowSize = halfWindowSize,
                           verbose = verbose)

  # single spectra data
  allmz <- as.numeric(colnames(avg$intmat))

  singlePeaks <- extractIntensity(mz = allmz,
                                  peaks = prc$singlePeaks,
                                  spec = prc$spec,
                                  tol = normTol*0.5)

  # fit curves
  if(verbose) {
    cat(timeNow(), "fitting curves... \n")
  }
  
  res_list <- calculateCurveFit(intmat = avg$intmat,
                                idx = filterVariance(apply(avg$intmat, 2, var),
                                                     method = varFilterMethod,
                                                     verbose = verbose),
                                verbose = verbose)

  # peak statistics
  stat_df <- calculatePeakStatistics(curveFits = res_list,
                                     singlePeaks = singlePeaks,
                                     spec = prc$spec)
  if(verbose) {
    cat(timeNow(), "Done!", "\n")
  }
 

  res_class <- new("MALDIassay",
                   avgSpectra = avg$avgSpec,
                   avgPeaks = avg$avgPeaksBinned,
                   singlePeaks = singlePeaks,
                   singleSpecSpots = spots,
                   normFactors = prc$normFac,
                   mzShifts = prc$mzShift,
                   fits = res_list,
                   stats = stat_df,
                   included_specIdx = prc$idx,
                   settings = list(
                     Conc = as.numeric(nm),
                     normMz = normMz,
                     normTol = normTol,
                     varFilterMethod = varFilterMethod,
                     monoisotopicFilter = monoisotopicFilter,
                     alignTol = alignTol,
                     SNR = SNR,
                     normMeth = normMeth,
                     binTol = binTol,
                     SinglePointRecal = SinglePointRecal
                   )
  )

  return(res_class)
}
