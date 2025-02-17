#' Extract the concentrations used in a MALDIassay
#'
#' @param object         Object of class MALDIassay
#'
#' @return
#' Numeric vector, concentrations used in a MALDIassay
#' @export
#' @examples 
#' # see example for `fitCurve()` to see how this data was generated
#' data(Blank2022res)
#' head(getConc(Blank2022res))

getConc <- function(object) {
  stopIfNotIsMALDIassay(object)
  conc <- as.numeric(object@settings$Conc[object@included_specIdx])
  return(conc)

}

#' get direction of curve
#'
#' @param model nplr model
#'
#' @return Numeric direction (positive or negative)
#'
#' @noRd
getDirection <- function(model) {
  y <- getYcurve(model)
  
  y0 <- y[1]
  y1 <- y[length(y)]
  
  return(y1 - y0)
}

#' Extract the intensities of single spectra for a given mzIdx
#'
#' @param object         Object of class MALDIassay
#' @param mz_idx         Integer, index of mz
#'
#' @return
#' Numeric vector, intensities of mzIdx
#' @export
#' @examples
#' 
#' # see example for `fitCurve()` to see how this data was generated
#' data(Blank2022res)
#' head(getSingleSpecIntensity(Blank2022res, 2))
getSingleSpecIntensity <- function(object, mz_idx) {
  s <- getSinglePeaks(object)
  mz <- mass(s[[1]]) # all single spectra have same mass axis
  targetMz <- getMzFromMzIdx(object, mz_idx)
  idx <- match.closest(targetMz, mz)

  int <- vapply(s,
                function(x) {
                  ints <- intensity(x)
                  return(ints[idx])
                },
                numeric(1))
  return(int)
}

#' Get the intensity matrix of single spectra for all fitted curves
#'
#' @param object          Object of class MALDIassay
#' @param avg             Logical, return single spectra intensity matrix (default) or average spectra intensity matrix
#' @param excludeNormMz   Logical, exclude normMz from intensity matrix.
#'
#' @details
#' Note that the returned matrix only contains *m/z* values that were actually fitted.
#' If a variance filtering step was applied this will not include **all** *m/z* values.
#' If you wish to get a matrix of **all** *m/z* values use ```MALDIquant::intensityMatrix(getSinglePeaks(object))```.
#' For average spectra intensity matrix with **all** *m/z* values use ```MALDIquant::intensityMatrix(getAvgPeaks(object), getAvgSpectra(object))```.
#'
#' @return
#' A matrix with columns as *m/z* values and rows as concentrations/spectra
#'
#' @export
#' @examples
#' # see example for `fitCurve()` to see how this data was generated
#' data(Blank2022res)
#' head(getIntensityMatrix(Blank2022res, avg = TRUE, excludeNormMz = TRUE) )
getIntensityMatrix <- function(object, avg = FALSE, excludeNormMz =FALSE) {

  if(avg) {
    intmat <- intensityMatrix(getAvgPeaks(object), getAvgSpectra(object))
  } else {
    intmat <- intensityMatrix(getSinglePeaks(object))
  }

  all_mz <- as.numeric(colnames(intmat))

  # filter fitted mz values
  mz <- getAllMz(object, excludeNormMz = excludeNormMz)
  idx <- match.closest(mz, all_mz, tolerance = 0.1)
  intmat <- intmat[,idx]

  return(intmat)
}

#' Extract m/z used for normalization
#'
#' @param object Object of class MALDIassay
#'
#' @return
#' Numeric, m/z used for normalization
#' @export
#' @examples
#' # see example for `fitCurve()` to see how this data was generated
#' data(Blank2022res)
#' getNormMz(Blank2022res) 

getNormMz <- function(object) {
  stopIfNotIsMALDIassay(object)
  return(object@settings$normMz)
}

#' Extract tolerance used for normalization
#'
#' @param object Object of class MALDIassay
#'
#' @return
#' Numeric, tolerance used for normalization
#' @export
#' @examples
#' # see example for `fitCurve()` to see how this data was generated
#' data(Blank2022res)
#' getNormMzTol(Blank2022res)
getNormMzTol <- function(object) {
  stopIfNotIsMALDIassay(object)
  return(object@settings$normTol)
}

#' Extract SNR used for peak detection
#'
#' @param object Object of class MALDIassay
#'
#' @return
#' Numeric, SNR used for peak detection
#' @export
#' @examples
#' # see example for `fitCurve()` to see how this data was generated
#' data(Blank2022res)
#' getSNR(Blank2022res)
getSNR <- function(object) {
  stopIfNotIsMALDIassay(object)
  return(object@settings$SNR)
}

#' Extract average spectra
#'
#' @param object Object of class MALDIassay
#'
#' @return
#' List of MALDIquantMassSpectrum
#' @export
#' @examples
#' 
#' # see example for `fitCurve()` to see how this data was generated
#' data(Blank2022res)
#' getAvgSpectra(Blank2022res)[[1]]
getAvgSpectra <- function(object) {
  stopIfNotIsMALDIassay(object)
  return(object@avgSpectra)
}

#' Extract peaks of average spectra
#'
#' @param object Object of class MALDIassay
#'
#' @return
#' List of MALDIquantMassPeaks
#' @export
#' @examples
#' # see example for `fitCurve()` to see how this data was generated
#' data(Blank2022res)
#' getAvgPeaks(Blank2022res)[[1]]
getAvgPeaks <- function(object) {
  stopIfNotIsMALDIassay(object)
  return(object@avgPeaks)
}

#' Extract peaks of single spectra spectra (before average calculation)
#'
#' @param object Object of class MALDIassay
#'
#' @return
#' List of MALDIquantMassPeaks
#' @export
#' @examples
#' # see example for `fitCurve()` to see how this data was generated
#' data(Blank2022res)
#' getSinglePeaks(Blank2022res)[[1]]
getSinglePeaks <- function(object) {
  stopIfNotIsMALDIassay(object)
  return(object@singlePeaks)
}

#' Extract normalization method
#'
#' @param object Object of class MALDIassay
#'
#' @return
#' Character, normalization method used.
#' @export
#' @examples
#' # see example for `fitCurve()` to see how this data was generated
#' data(Blank2022res)
#' getNormMethod(Blank2022res)
getNormMethod <- function(object) {
  stopIfNotIsMALDIassay(object)
  return(object@settings$normMeth)
}

#' Extract variance filtering method
#'
#' @param object Object of class MALDIassay
#'
#' @return
#' Character of variance filtering method used
#' @export
#' @examples
#' # see example for `fitCurve()` to see how this data was generated
#' data(Blank2022res)
#' getVarFilterMethod(Blank2022res)
getVarFilterMethod <- function(object) {
  stopIfNotIsMALDIassay(object)
  return(object@settings$varFilterMethod)
}

#' Extract applied mz-shift
#'
#' @param object Object of class MALDIassay
#'
#' @return
#' Numeric vector of mz-shits applied to spectra
#' @export
#' @examples
#' # see example for `fitCurve()` to see how this data was generated
#' data(Blank2022res)
#' head(getAppliedMzShift(Blank2022res))
getAppliedMzShift <- function(object) {
  stopIfNotIsMALDIassay(object)
  return(object@mzShifts)
}

#' Extract applied normalization factors
#'
#' @param object Object of class MALDIassay
#'
#' @return
#' Numeric vector of normalization factors applied to spectra
#' @export
#' @examples
#' # see example for `fitCurve()` to see how this data was generated
#' data(Blank2022res)
#' head(getAppliedNormFactors(Blank2022res))
getAppliedNormFactors <- function(object) {
  stopIfNotIsMALDIassay(object)
  return(object@normFactors)
}

#' Extract curve fits
#'
#' @param object Object of class MALDIassay
#'
#' @return
#' List, containing the data used to do the fits as well as the nlpr curve fit .
#' @export
#' @examples
#' # see example for `fitCurve()` to see how this data was generated
#' data(Blank2022res)
#' fits <- getCurveFits(Blank2022res)
getCurveFits <- function(object) {
  stopIfNotIsMALDIassay(object)
  return(object@fits)
}

#' Extract directory path
#'
#' @param object Object of class MALDIassay
#'
#' @return
#' List, containing the data used to do the fits as well as the nlpr curve fit .
#' @export
#' @examples
#' # see example for `fitCurve()` to see how this data was generated
#' data(Blank2022res)
#' getDirectory(Blank2022res)
getDirectory <- function(object) {
  stopIfNotIsMALDIassay(object)
  return(object@settings$dir)
}

#' Extract peak statistics
#'
#' @param object Object of class MALDIassay
#' @param summarise Logical, return summarized results (one result per mz and not per mz and spectra)
#'
#' @return
#' A tibble with peak statistics (R², fold-change, CV%, etc.)
#' @export
#'
#' @importFrom dplyr select ungroup arrange
#' @examples
#' # see example for `fitCurve()` to see how this data was generated
#' data(Blank2022res)
#' head(getPeakStatistics(Blank2022res, summarise = TRUE))
getPeakStatistics <- function(object, summarise = FALSE) {
  stopIfNotIsMALDIassay(object)
  stats <- object@stats

  if (summarise) {
    stats <- stats %>%
      mutate(mz = as.numeric(.data$mz)) %>%
      group_by(.data$mz, .data$mzIdx) %>%
      summarise(
        pIC50 = first(.data$pIC50),
        R2 = first(.data$R2),
        wgof = first(.data$wgof),
        min = mean(.data$min),
        max = mean(.data$max),
        FC = first(.data$fc)
      ) %>%
      ungroup() %>%
      arrange(.data$mzIdx) %>%
      left_join(getFittingParameters(object, summarise = TRUE),
                by = join_by("mz")) %>%
      mutate(symetric = ifelse(.data$npar < 5, TRUE, FALSE)) %>%
      select(-.data$npar)
  }

  return(stats)
}

#' Calculate remaining calibration error of a MALDIassay object
#'
#' @param object Object of class MALDIassay
#'
#' @return
#' A tibble containing statistics about remaining calibration error
#' @export
#'
#' @importFrom tibble tibble
#' @examples
#' # see example for `fitCurve()` to see how this data was generated
#' data(Blank2022res)
#' getRecalibrationError(Blank2022res)
getRecalibrationError <- function(object) {
  peaks <- peaks2df(getAvgPeaks(object))
  mzdev <- .getMzShift(
    peaksdf = peaks,
    tol = getNormMzTol(object),
    targetMz = getNormMz(object),
    tolppm = FALSE,
    allowNoMatch = TRUE
  )

  res_df <- tibble(
    meanAbs = mean(abs(mzdev$mzshift)),
    sdAbs = sd(abs(mzdev$mzshift)),
    maxAbs = max(abs(mzdev$mzshift)),
    mean = mean(mzdev$mzshift),
    sd = sd(mzdev$mzshift)
  )
  return(res_df)
}

#' Get the mz value associated with a mzIdx
#'
#' @param object Object of class MALDIassay
#' @param mzIdx  numeric, index of mass of interest (see \code{getPeakStatistics()})
#'
#' @return
#' numeric, mz value
#' @export
#' @examples
#' # see example for `fitCurve()` to see how this data was generated
#' data(Blank2022res)
#' getMzFromMzIdx(Blank2022res, mzIdx = 2)
getMzFromMzIdx <- function(object, mzIdx) {
  stopIfNotIsMALDIassay(object)
  mz <- as.numeric(getPeakStatistics(object, TRUE)[mzIdx, "mz"])

  if(length(mz)>1) {
    warning("Something is wrong. There are multiple mz-values with this index!\n")
  }
  return(mz)
}

#' Get all mz value of an MALDIassay-object
#'
#' @param object        Object of class MALDIassay
#' @param excludeNormMz Logical, remove normMz from list of mz values.
#'
#' @return
#' numeric vector of mz values
#' @export
#' @examples
#' # see example for `fitCurve()` to see how this data was generated
#' data(Blank2022res)
#' head(getAllMz(Blank2022res))
getAllMz <- function(object, excludeNormMz = FALSE) {
  stopIfNotIsMALDIassay(object)
  if(!excludeNormMz) {
    mz <-
      getPeakStatistics(object, TRUE) %>%
      pull(mz)
  } else {
    mz <-
      getPeakStatistics(object, TRUE) %>%
      filter(
        is.na(
          match.closest(
            table = getNormMz(object),
            x = mz,
            tolerance = getNormMzTol(object)
          )
        )
      ) %>%
      pull(mz)
  }

  return(mz)
}

#' Get the spot coordinates of spectra
#'
#' @param object      Object of class MALDIassay
#' @param singleSpec  Logical, extract the spot coordinates of single spectra (default) or from average spectra.
#'
#' @return
#' character vector of spot coordinates. In case of average spectra multiple spots are concatenated.
#' @export
#' @examples
#' # see example for `fitCurve()` to see how this data was generated
#' data(Blank2022res)
#' # spots per spectrum
#' getSpots(Blank2022res, singleSpec = TRUE)
#' 
#' #spots per concentration
#' getSpots(Blank2022res, singleSpec = FALSE)
getSpots <- function(object, singleSpec = TRUE) {
  stopIfNotIsMALDIassay(object)
  if(singleSpec) {
    spots <- object@singleSpecSpots
    return(spots)
  } else {
    spots <- extractSpots(getAvgSpectra(object))
    return(spots)
  }
}

#' Get fitting parameters
#'
#' @param object      Object of class MALDIassay
#' @param summarise   Logical, remove everything other then npar and mz from result.
#'
#' @return
#' tibble of fitting parameters for each fitted m/z-value
#' @export
#' @importFrom nplr getPar
#' @examples
#' # see example for `fitCurve()` to see how this data was generated
#' data(Blank2022res)
#' head(getFittingParameters(Blank2022res, summarise = FALSE))
getFittingParameters <- function(object, summarise = FALSE) {
  stopIfNotIsMALDIassay(object)

  fits <- getCurveFits(object)

  res_list <- lapply(seq_along(fits),
                     function(i) {
                       par <- getPar(fits[[i]]$model)

                       return(tibble(mz = names(fits)[i],
                              npar = par$npar[[1]],
                              bottom = par$params[["bottom"]],
                              top = par$params[["top"]],
                              xmid = par$params[["xmid"]],
                              scal = par$params[["scal"]],
                              s = par$params[["s"]]))
                     })

  df <- bind_rows(res_list, .id = "mzIdx") %>%
    mutate(mzIdx = as.numeric(.data$mzIdx))

  if(summarise) {
    df <- df %>%
      mutate(mz = as.numeric(.data$mz)) %>%
      select(.data$mz, .data$npar)
  }

  return(df)
}

#' Get binning tolerance
#'
#' @param object Object of class MALDIassay
#'
#' @return
#' Numeric, tolerance used for binning in Dalton.
#' @export
#' @examples
#' # see example for `fitCurve()` to see how this data was generated
#' data(Blank2022res)
#' getBinTol(Blank2022res)
getBinTol <- function(object) {
  return(object@settings$binTol)
}


