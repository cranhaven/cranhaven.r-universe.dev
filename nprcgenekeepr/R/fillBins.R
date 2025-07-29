#' fillBins Fill bins represented by list of two lists \code{males} and
#' \code{females}.
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr

#' @return  A list with two TODO: RMS provide description
#' @param ageDist dataframe with \code{sex} and \code{age} columns
#' @param lowerAges integer vector of lower age boundaries; must be the same
#' length as \code{upperAges}
#' @param upperAges integer vector of upper age boundaries; must be the same
#' length as \code{lowerAges}
#' @noRd
fillBins <- function(ageDist, lowerAges, upperAges = NULL) {
  if (any(names(ageDist) == "exit")) {
    ageDist <- ageDist[is.na(ageDist$exit), ]
  }
  if (is.null(upperAges)) {
    upperAges <- c(lowerAges[-1L], 100L)
  }
  maleBins <- integer(0L)
  femaleBins <- integer(0L)
  for (bin in seq_along(lowerAges)) {
    maleBins <- c(maleBins, nrow(ageDist[ageDist$sex == "M" &
      ageDist$age >= lowerAges[bin] &
      ageDist$age < upperAges[bin] &
      !is.na(ageDist$age), ]))
    femaleBins <- c(femaleBins, nrow(ageDist[ageDist$sex == "F" &
      ageDist$age >= lowerAges[bin] &
      ageDist$age < upperAges[bin] &
      !is.na(ageDist$age), ]))
  }
  list(males = maleBins, females = femaleBins)
}
