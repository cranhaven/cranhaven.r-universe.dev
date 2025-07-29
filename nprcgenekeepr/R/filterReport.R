#' Filters a genetic value report down to only the specified animals
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return A copy of report specific to the specified animals.
#'
#' @param ids character vector of animal IDs
#' @param rpt a dataframe with required colnames \code{id}, \code{gu},
#' \code{zScores}, \code{import}, \code{totalOffspring}, which is
#' a data.frame of results from a genetic value analysis.
#' @export
#' @examples
#' library(nprcgenekeepr)
#' rpt <- nprcgenekeepr::pedWithGenotypeReport$report
#' rpt1 <- filterReport(c("GHH9LB", "BD41WW"), rpt)
filterReport <- function(ids, rpt) {
  rpt[rpt$id %in% ids, ]
}
