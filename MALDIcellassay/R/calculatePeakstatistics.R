#' Calculate peak statistics
#'
#' @param curveFits       list of curve fits as returned by `MALDIcellassay::calculateCurveFit()`.
#' @param singlePeaks     list of MALDIquant::MassPeaks.
#' @param spec            list of MALDIquant::MassSpectrum.
#'
#' @return
#' A tibble with peak statistics.
#' @export
#' @importFrom dplyr join_by
#' @importFrom MALDIquant intensityMatrix isMassPeaksList
#' 
#' @examples
#' data(Blank2022intmat)
#' fits <- calculateCurveFit(Blank2022intmat, idx = 1:5)
#' 
#' peakstats <- calculatePeakStatistics(curveFits = fits, 
#'                                      singlePeaks = Blank2022peaks, 
#'                                      spec = Blank2022spec)
#' head(peakstats)

calculatePeakStatistics <- function(curveFits, singlePeaks, spec) {
  if (!is.list(curveFits)) {
    stop("curveFits must be a list of curve fits. See MALDIcellassay::calculateCurveFit().\n")
  }
  if (!isMassPeaksList(singlePeaks)) {
    stop("singlePeaks must be a list of MALDIquant::MassPeaks.\n")
  }
  if (is.null(names(singlePeaks))) {
    stop("singlePeaks must have concentrations as names!\n")
  }

  intensityMatrix <- intensityMatrix(singlePeaks, spec)
  rownames(intensityMatrix) <- names(singlePeaks)

  fit_df <- lapply(curveFits, function(x) {
    model <- x$model

    res_df <-
      suppressMessages(
      suppressWarnings(
        as_tibble(
          nplr::getGoodness(model)
        )
      )
    ) %>%
      mutate(
        fc = calculateFC(model),
        pIC50 = .getEstimates(model, 0.5)
      )
    return(res_df)
  }) %>%
    bind_rows(.id = "mz") %>%
    rename("R2" = "gof")

  stat_df <-
    intensityMatrix %>%
    as_tibble() %>%
    mutate(sample = rownames(intensityMatrix)) %>%
    gather("mz", "int", -"sample") %>%
    arrange(as.numeric(.data[["mz"]])) %>%
    group_by(.data[["sample"]], .data[["mz"]]) %>%
    summarise(
      min = min(.data$int, na.rm = TRUE),
      mean = mean(.data$int, na.rm = TRUE),
      max = max(.data$int, na.rm = TRUE),
      stdev = sd(.data$int, na.rm = TRUE),
      "cv%" = .data$stdev / .data$mean * 100
    ) %>%
    ungroup() %>%
    left_join(fit_df, by = join_by("mz")) %>%
    filter(!is.na(.data$R2)) %>%
    mutate(mzIdx = as.numeric(as.factor(as.numeric(.data$mz))))
  return(stat_df)
}
