#' @title \code{co_accessibility}
#'
#' @description \code{co_accessibility} allows you to determine whether 2 tiles
#'              are co-accessible using a zero-inflated Spearman correlation.
#'
#'
#' @param subMat sample-tile matrix with regions to analyze
#' @param filterPairs list of passed tile-pairs that have been tested. Used to remove already tested pairs.
#' @param numCores integer to determine # of parallel cores
#' @param ZI boolean that determines whether to use zero-inflated or normal Spearman correlations
#' @param verbose Set TRUE to display additional messages. Default is FALSE.
#'
#' @return a 3-column data.frame containing
#'         - Correlation = Zero-inflated Spearman Correlation
#'         - Tile1 = location of co-accessible region 1
#'         - Tile2 = location of co-accessible region 2
#'
#' @details The technical details of the zero-inflated correlation can be
#'          found here:
#'
#'               Pimentel, Ronald Silva, "Kendall's Tau and Spearman's Rho
#'               for Zero-Inflated Data" (2009). Dissertations.
#'
#'          while the implementation (scHOT R package), can be found here:
#'               http://www.bioconductor.org/packages/release/bioc/html/scHOT.html
#'
#'
#' @noRd
#'
#'

co_accessibility <- function(subMat, filterPairs, index, numCores = 40, ZI = TRUE, verbose = FALSE) {
  regionOfInterest <- rownames(subMat)[index]
  allOtherRegions <- rownames(subMat)[-index]

  # Var1 will always be our region of interest
  keyNeighborPairs <- as.matrix(data.frame(
    "Key" = regionOfInterest,
    "Neighbor" = allOtherRegions
  ))

  if (!is.null(filterPairs) & nrow(keyNeighborPairs) > 1) {
    # Filter out any of our neighboring regions (Var2) that were previous "regions of interest".
    # These will be the "Var1/Tile1"s in previous results.
    keyNeighborPairs <- keyNeighborPairs[!(keyNeighborPairs[, "Neighbor"] %in% filterPairs$Tile1), ]
  }

  # If only one pair of tiles left, then nrows will hit an error.
  # length()==2 is equivalent to one row (pair)
  if (length(keyNeighborPairs) == 0 & verbose) {
    if (verbose) {
      warning("Warning: No tiles found in neighborhood of region of interest")
    }
  } else if (length(keyNeighborPairs) == 0) {
    return(NULL)
  } else if (length(keyNeighborPairs) == 2) {
    # If only one pair of tiles to test, then it's no longer a data.frame, but a vector.
    zero_inflated_spearman <- weightedZISpearman(
      x = subMat[keyNeighborPairs[1], ],
      y = subMat[keyNeighborPairs[2], ],
      verbose = verbose,
      ZI = ZI
    )

    zi_spear_mat <- data.table(
      Correlation = zero_inflated_spearman,
      Tile1 = keyNeighborPairs[1],
      Tile2 = keyNeighborPairs[2]
    )
  } else {
    # General case for >1 pair
    zero_inflated_spearman <- unlist(parallel::mclapply(1:nrow(keyNeighborPairs),
      function(x) {
        weightedZISpearman(
          x = subMat[keyNeighborPairs[x, "Key"], ],
          y = subMat[keyNeighborPairs[x, "Neighbor"], ],
          verbose = verbose,
          ZI = ZI
        )
      },
      mc.cores = numCores
    ))

    # Create zero-inflated correlation matrix from correlation values
    zi_spear_mat <- data.table(
      Correlation = zero_inflated_spearman,
      Tile1 = keyNeighborPairs[, "Key"],
      Tile2 = keyNeighborPairs[, "Neighbor"]
    )
  }

  return(zi_spear_mat)
}
