#' Creates a pyramid plot of the pedigree provided.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' The pedigree provided must have the following columns: \code{sex} and
#' \code{age}. This needs to be augmented to allow pedigrees structures that
#' are provided by the nprcgenekeepr package.
#'
#' @return The return value of par("mar") when the function was called.
#'
#' @param ped dataframe with pedigree data.
#' @importFrom lubridate now
#' @importFrom plotrix color.gradient
#' @importFrom stringi stri_c
#' @importFrom graphics par
#' @export
#' @examples
#' library(nprcgenekeepr)
#' data(qcPed)
#' getPyramidPlot(qcPed)
getPyramidPlot <- function(ped = NULL) {
  if (is.null(ped)) {
    ped <- getPyramidAgeDist()
  }
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  par(bg = "#FFF8DC")
  binWidth <- 2L
  axModulas <- 5L
  upperAges <- seq(
    binWidth,
    makeRoundUp(getPedMaxAge(ped), binWidth), binWidth
  )
  lowerAges <- upperAges - binWidth

  bins <- fillBins(ped, lowerAges, upperAges)
  maxAx <- max(getMaxAx(bins, axModulas))
  ageLabels <- stri_c(lowerAges, " - ", upperAges - 1L)
  mcol <- color.gradient(0L, 0L, 0.5)
  fcol <- color.gradient(1L, 0.5, 0.5)
  currentDate <- now()
  axBy <- maxAx / axModulas
  axGap <- axBy * 0.6
  ## The following values have worked well for chimpanzees:
  ## gap=40, laxlab = seq(0, 100, by = 10), and raxlab = seq(0, 100, by = 10)
  gap <- axGap
  laxlab <- seq(0L, maxAx, by = axBy)
  raxlab <- seq(0L, maxAx, by = axBy)
  agePyramidPlot(
    bins$males, bins$females, ageLabels, mcol, fcol,
    laxlab, raxlab, gap, currentDate
  )
}
