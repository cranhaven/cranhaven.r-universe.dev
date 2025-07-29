#' Order the results of the genetic value analysis for use in a report.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Part of Genetic Value Analysis
#'
#' Takes in the results from a genetic value analysis and orders the report
#' according to the ranking scheme we have developed.
#'
#' @return A dataframe, which is \code{rpt} sorted according to the ranking
#' scheme:
#' \itemize{
#'  \item imported animals with no offspring
#'  \item animals with genome uniqueness above 10%, ranked by descending gu
#'  \item animals with mean kinship less than 0.25, ranked by ascending mk
#'  \item all remaining animals, ranked by ascending mk
#' }
#'
#' @param rpt a dataframe with required colnames \code{id}, \code{gu},
#' \code{zScores}, \code{import}, \code{totalOffspring}, which is
#' a data.frame of results from a genetic value analysis.
#' @param ped the pedigree information in datatable format with required
#' colnames \code{id}, \code{sire}, \code{dam}, \code{gen}, \code{population}).
#' This requires complete pedigree information..
#' @noRd
orderReport <- function(rpt, ped) {
  finalRpt <- list()

  founders <- ped$id[is.na(ped$sire) & is.na(ped$dam)]

  if ("origin" %in% names(rpt)) {
    # imports with no offspring
    i <- (!is.na(rpt$origin) & (rpt$totalOffspring == 0L) &
      (rpt$id %in% founders))

    imports <- rpt[i, ]
    rpt <- rpt[!i, ]
    if ("age" %in% names(rpt)) {
      finalRpt$imports <- imports[with(imports, order(age)), ]
    } else {
      finalRpt$imports <- imports[with(imports, order(id)), ]
    }

    # ONPRC-born animals with no parentage
    i <- (is.na(rpt$origin) & (rpt$totalOffspring == 0L) &
      (rpt$id %in% founders))

    noParentage <- rpt[i, ]
    rpt <- rpt[!i, ]
    if ("age" %in% names(rpt)) {
      finalRpt$noParentage <- noParentage[with(noParentage, order(age)), ]
    } else {
      finalRpt$noParentage <- noParentage[with(noParentage, order(id)), ]
    }
  }

  # subjects with > 10% genome uniqueness
  highGu <- rpt[(rpt$gu > 10L), ]
  finalRpt$highGu <- highGu[with(highGu, order(-trunc(gu), zScores)), ]
  rpt <- rpt[(rpt$gu <= 10L), ]

  # subjects with <= 10% genome uniqueness and <= 0.25 z-score
  lowMk <- rpt[(rpt$zScores <= 0.25), ]
  finalRpt$lowMk <- lowMk[with(lowMk, order(zScores)), ]

  rpt <- rpt[(rpt$zScores > 0.25), ]

  # subjects with <= 10% genome uniqueness and > 0.25 z-score
  finalRpt$lowVal <- rpt[with(rpt, order(zScores)), ]

  includeCols <- intersect(
    c(
      "imports", "highGu", "lowMk",
      "lowVal", "noParentage"
    ),
    names(finalRpt)
  )

  finalRpt <- finalRpt[includeCols]
  finalRpt <- rankSubjects(finalRpt)
  finalRpt <- do.call("rbind", finalRpt)
  rownames(finalRpt) <- seq_len(nrow(finalRpt))
  finalRpt
}
