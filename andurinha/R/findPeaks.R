#' @title findPeaks
#'
#' @description This function finds peaks and allows to the most relevant based
#'   on the second derivative/absorbance sum spectrum.
#'
#' @param data A data frame object, which contains in the first column the wave
#'   numbers and in the following columns the samples absorbances.
#' @param resolution The equipment measurement resolution; by default 4 cm-1.
#' @param minAbs The cut off value to check spectra quality; by default 0.1.
#' @param cutOff The second derivative/absorbance sum spectrum cut off to reduce
#'   the raw peaks table; by default NULL.
#' @param scale By default (TRUE) the data is scaled by Z-scores. Use FALSE in
#'   case you do not want to scale it.
#' @param ndd By default (TRUE) the peaks are searched based on the second
#'   derivative sum spectrum. Use FALSE in case you want to search them based on
#'   the absorbance sum spectrum.
#'
#' @return A list with a collection of data frames which contains:
#' \enumerate{
#'   \item \emph{dataZ}: the standardised data by Z-scores.
#'   \item \emph{secondDerivative}: the second derivative values of the data.
#'   \item \emph{sumSpectrum_peaksTable}: the peaks wave numbers and their
#' second derivative/absorbance sum spectrum values.
#'   \item \emph{peaksTable}: the selected peaks wave numbers and their
#'   absorbance for each spectrum.
#' }
#'
#' @seealso  \code{\link{importSpectra}}, \code{\link{gOverview}} and
#'   \code{\link{plotPeaks}}
#'
#' @examples
#' # Find Peaks based on the absorbance sum spectrum
#' fp.abs <- findPeaks(andurinhaData, ndd = FALSE)
#'
#' # See the peaks table of the absorbance sum spectrum
#' fp.abs$sumSpectrum_peaksTable
#'
#' # Find Peaks based on the second derivative sum spectrum
#' fp.ndd <- findPeaks(andurinhaData)
#'
#' # See the peaks table of the second derivative sum spectrum
#' fp.ndd$sumSpectrum_peaksTable
#'
#' # Select a cutOff to reduce the number of peaks in the table
#' # (i.e. select the most relevant)
#' # fp.ndd$sumSpectrum_peaksTable %>%
#' #   arrange(desc(sumSpectrum))
#' # Run findPeaks() with the new cutOff
#' fp.ndd2 <- findPeaks(andurinhaData, cutOff = 0.25)
#'
#' @export

findPeaks <- function(data, resolution = 4, minAbs = 0.1, cutOff = NULL, scale = TRUE, ndd = TRUE) {

  if (!is.data.frame(data)) {
    abort(paste0(
      "`data` must be a data frame; not ", typeof(data), "."
    ))
  }

  if (!is.numeric(resolution)) {
    abort(paste0(
      "`resolution` must be numeric; not ", typeof(resolution), "."
    ))
  }

  if (!is.numeric(minAbs)) {
    abort(paste0(
      "`minAbs` must be numeric; not ", typeof(minAbs), "."
    ))
  }

  if (!is.null(cutOff) && !is.numeric(cutOff)) {
    abort(paste0(
      "`cutOff` must be NULL or numeric; not ", typeof(cutOff), "."
    ))
  }

  if (!is.logical(scale)) {
    abort(paste0(
      "`scale` must be logical; not ", typeof(scale), "."
    ))
  }

  if (!is.logical(ndd)) {
    abort(paste0(
      "`ndd` must be logical; not ", typeof(ndd), "."
    ))
  }

  if (length(resolution) > 1) {
    abort(paste0(
      "`resolution` must has length = 1; not ", length(resolution), "."
    ))
  }

  if (length(minAbs) > 1) {
    abort(paste0(
      "`minAbs` must has length = 1; not ", length(minAbs), "."
    ))
  }

  if (length(cutOff) > 1) {
    abort(paste0(
      "`cutOff` must has length = 1; not ", length(cutOff), "."
    ))
  }

  # ------------------------------------------------------------------
  # TESTING DATA:
  spectrumMaxima <- data.frame("sample" = names(data)[-1],
                               "absMax" = unname(apply(data[, -1], 2, max)))

  # check abs max value > 0.1; if < 0.1 notify
  if (min(spectrumMaxima$absMax) < minAbs) {

    # Warning message
    message(paste0(
      "In your data are samples with absorbance max < ", minAbs, ": ",
      "if you want to continue,
      change the default value of `minAbs` to the minimum absorbance of your data set."
    ))

    print(spectrumMaxima)

  } else {

    searchPeaks <- function(spectrum, d_WN) {

      r <- l <- vector(mode = "numeric", length = 0)

      # Search peaks in the spectrum:
      for (i in 1:(length(spectrum) - 1)) {
        if (spectrum[i] - spectrum[i+1] > 0) {
          r <- c(r, i)
        }
      }

      for (i in 2:length(r)) {
        if (spectrum[r[i]] - spectrum[r[i-1]] > 0) {
          l <- c(l, i)
        }
      }

      # Final positions:
      f <- r[l]

      # Peaks WN:
      p_WN <- d_WN[c(f)]

      return(p_WN)
    }

    if (scale == TRUE) {

      # STANDARISE by Z-scores:
      data <- data.frame("WN" = data[, 1],
                         as.data.frame(scale(data[, -1])))
    }

    if (ndd == TRUE) {

      # ------------------------------------------------------------------
      # SECOND DERIVATIVE:
      sg39 <- data.frame("WN" = data[, 1],
                         -100 * (data.frame(lapply(data[, -1], sgolayfilt, p = 3, n = 9, m = 2))))

      # ------------------------------------------------------------------
      # SUM SPECTRUM - Second Derivative:
      sg39Sum <- data.frame("WN" = data[, 1],
                            "sumSpectrum" = apply(sg39[, -1], 1, sum))

      # Second Derivative sum spectrum peaks WN:
      peaksWN <- searchPeaks(sg39Sum[, 2], sg39Sum[, 1])

      # Second Derivative sum spectrum peaks df
      # (WN + second derivative sum spectrum values):
      peaksSS <- sg39Sum[sg39Sum[, 1] %in% peaksWN, ]

    } else {

      # ------------------------------------------------------------------
      # SUM SPECTRUM - Absorbance:
      absSum <- data.frame("WN" = data[, 1],
                           "sumSpectrum" = apply(data[, -1], 1, sum))

      # Second Derivative sum spectrum peaks WN:
      peaksWN <- searchPeaks(absSum[, 2], absSum[, 1])

      # Second Derivative sum spectrum peaks df
      # (WN + absorbance sum spectrum values):
      peaksSS <- absSum[absSum[, 1] %in% peaksWN, ]

    }

    if (!is.null(cutOff)) {
      peaksSS <- peaksSS[peaksSS[, 2] > cutOff, ]
    }

    # PEAKS: WN + abs
    dataPeaks <- data[data[, 1] %in% peaksSS[, 1], ]

    # Group WN that are  equivalents:
    resolution <- resolution/2

    for (i in 1:(dim(dataPeaks)[1] - 1)) {
      if (dataPeaks[, 1][i+1] %in% seq(from = dataPeaks[, 1][i],
                                       to = dataPeaks[, 1][i] + resolution,
                                       by = 1)) {
        dataPeaks[, 1][i+1] <- dataPeaks[, 1][i]
      }
    }

    # Merge equivalent WN abs -> for each spectrum,
    # for each equal WN -> choose abs.max:
    ePAbs <- aggregate(dataPeaks, list(dataPeaks[, 1]), max)[, -1]

    # ------------------------------------------------------------------
    # OUTPUT:
    output <- compact(list(dataZ = if (scale == TRUE) { data },
                   secondDerivative = if (ndd == TRUE) { sg39 },
                   sumSpectrum_peaksTable = peaksSS,
                   peaksTable = ePAbs))

    return(output)
  }
}
