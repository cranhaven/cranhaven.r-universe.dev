#' Class MALDIassay
#'
#' A class for holding MALDI assay related information.
#'
#' @param object MALDIassay.
#'
#' @importFrom dplyr arrange group_by first summarise desc
#' @importFrom MALDIquant mass
#' @importFrom utils head
setClass("MALDIassay",
  representation = representation(
    avgSpectra = "list",
    avgPeaks = "list",
    singlePeaks = "list",
    singleSpecSpots = "character",
    normFactors = "numeric",
    mzShifts = "numeric",
    fits = "list",
    stats = "data.frame",
    included_specIdx = "numeric",
    settings = "list"
  )
)

show_MALDIassay <- function(object) {
  mz <- round(getNormMz(object), digits = 2)
  varFilterMethod <- getVarFilterMethod(object)
  tol <- getNormMzTol(object)
  numPeaksTotal <- length(mass(getSinglePeaks(object)[[1]]))
  hiVarPeaks <- length(unique(getPeakStatistics(object)$mz))
  mzdev <- getRecalibrationError(object)
  meanMzShift <- round(mean(getAppliedMzShift(object)), 4)
  absMaxMzShift <- round(max(abs(getAppliedMzShift(object))), 4)
  conc <- getConc(object)

  # Compose normalization information
  if (getNormMethod(object) == "mz") {
    normStr <- paste("Normalization on m/z", mz, "\u00B1", tol, "Da.\n")
  } else {
    normStr <- paste("Normalization using", getNormMethod(object), "method.\n")
  }


  cat("------MALDIassay object------\n")
  cat("\n")
  cat("Including", length(unique(conc)), "concentrations,\n")
  cat("ranging from", min(conc), "to", max(conc), ".")
  cat("\n")
  cat(normStr)
  cat("\n")
  if (object@settings$SinglePointRecal) {
    cat("Single point recalibation on", mz, "with", tol, "Da tolerance.\n")
    cat("Avg. mass shift before recal.:", meanMzShift, "Da. Max abs. shift:", absMaxMzShift, "Da.\n")
    cat("Avg. mass shift after recal. :", round(mzdev$mean, 4), "Da. Max abs. shift:", round(mzdev$meanAbs, 4), "Da.\n")
    cat("\n")
  }

  cat(paste0("Found ", numPeaksTotal, " peaks (SNR ", getSNR(object), ") and ", hiVarPeaks, " high variance peaks\n"))
  cat("using variance filtering method:", paste0(varFilterMethod, ".\n"))
  cat("\n")

  cat("Top5-features based on Fold-Change and R\u00B2:\n")
  print(getPeakStatistics(object, summarise = TRUE) %>%
    arrange(desc(.data$FC), desc(.data$R2)) %>%
    as.data.frame() %>%
    head(n = 5))
}

setMethod(
  "show", signature(object = "MALDIassay"),
  show_MALDIassay
)
