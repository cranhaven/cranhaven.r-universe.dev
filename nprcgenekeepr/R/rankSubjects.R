#' Ranks animals based on genetic value.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Part of Genetic Value Analysis
#' Adds a column to \code{rpt} containing integers from 1 to nrow, and provides
#' a value designation for each animal of "high value" or "low value"
#'
#' @return A list of dataframes with value and ranking information added.
#'
#' @param rpt a list of data.frame (req. colnames: value) containing genetic
#' value data for the population. Dataframes separate out those animals that
#' are imports, those that have high genome uniqueness (gu > 10%), those that
#' have low mean kinship (mk < 0.25), and the remainder.
#'
#' @export
#' @examples
#' library(nprcgenekeepr)
#' finalRpt <- nprcgenekeepr::finalRpt
#' rpt <- rankSubjects(nprcgenekeepr::finalRpt)
#' rpt[["highGu"]][1, "value"]
#' rpt[["highGu"]][1, "rank"]
#' rpt[["lowMk"]][1, "value"]
#' rpt[["lowMk"]][1, "rank"]
#' rpt[["lowVal"]][1, "value"]
#' rpt[["lowVal"]][1, "rank"]
rankSubjects <- function(rpt) {
  rnk <- 1L

  for (i in seq_len(length(rpt))) {
    if (nrow(rpt[[i]]) == 0L) {
      next
    }

    if (names(rpt[i]) == "lowVal") {
      rpt[[i]][, "value"] <- "Low Value"
    } else if (names(rpt[i]) == "noParentage") {
      rpt[[i]][, "value"] <- "Undetermined"
    } else { # everything else
      rpt[[i]][, "value"] <- "High Value"
    }

    if (names(rpt[i]) == "noParentage") {
      rpt[[i]][, "rank"] <- NA
    } else {
      rpt[[i]][, "rank"] <- rnk:(rnk + nrow(rpt[[i]]) - 1L)
      rnk <- rnk + nrow(rpt[[i]])
    }
  }
  rpt
}
