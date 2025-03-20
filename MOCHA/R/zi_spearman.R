#######################################################################################
#' scHOT implemented Pimentel's zero-inflated (ZI) correlation
#' in their R package, providing implementations
#' of the ZI spearman and tau rank correlations.
#' MOCHA implements a slight modification of
#' scHOT's zero-inflated correlation measure, by
#' returning NAs in the cases where the correlation
#' is undefined, and modifying it to use a C-backed correlation program.
#' Both references are provided below and are
#' referenced in all documentations to indicate
#' their work in implementing these methods.
#'
#'
#' Description:
#' the weightedZISpearman function calculates weighted rho\*,
#' where rho\* is described in Pimentel et al (2009).
#' This association measure is defined for zero-inflated,
#' non-negative random variables.
#'
#' @title weightedZISpearman
#' @param w weight vector, values should be between 0 and 1
#' @param x,y x and y are non-negative data vectors
#' @param verbose Set TRUE to display additional messages. Default is FALSE.
#' @param ZI boolean flag that enables zero-inflated (ZI) Spearman correlations to be used. Default is TRUE. If FALSE, skip zero-inflation and calculate the normal Spearman.
#' @return \code{numeric} weighted rho* association value between x and y
#'
#'
#' @references scHOT
#' Ghazanfar, S., Lin, Y., Su, X. et al. Investigating higher-order interactions in
#'   single-cell data with scHOT. Nat Methods 17, 799–806 (2020).
#'   https://doi.org/10.1038/s41592-020-0885-x
#'
#' Zero-Inflated Correlation
#' Pimentel, Ronald Silva, "Kendall's Tau and Spearman's Rho for
#'   Zero-Inflated Data" (2009). Dissertations. 721.
#'   https://scholarworks.wmich.edu/dissertations/721
#'
#' @noRd
weightedZISpearman <- function(x, y, w = 1, verbose = FALSE, ZI = TRUE) {

  # needs the original values, not the ranks

  if (any(x < 0 | y < 0)) {
    stop("x and/or y values have negative values")
  }
  if (length(x) != length(y)) {
    stop("x and y should have the same length")
  }
  if (length(w) == 1) {
    w <- rep(w, length(x))
  }

  if (!ZI) {
    spearmanCorr <- wCorr::weightedCorr(x = x, y = y, weights = w, method = "Spearman")
    return(spearmanCorr)
  }

  posx <- x > 0
  posy <- y > 0
  pospos <- posx & posy

  p_11 <- sum(w * pospos) / sum(w)
  p_00 <- sum(w * (!posx & !posy)) / sum(w)
  p_01 <- sum(w * (!posx & posy)) / sum(w)
  p_10 <- sum(w * (posx & !posy)) / sum(w)


  if (any(pospos) & p_11 > 0) {
    rho_11 <- wCorr::weightedCorr(x = x[pospos], y = y[pospos], weights = w[pospos], method = "Spearman")
  } else {
    rho <- NA
    if (verbose) {
      message("Zero inflated Spearman correlation is undefined, returning NA")
    }
    return(rho)
  }

  rho_star <- p_11 * (p_01 + p_11) * (p_10 + p_11) * rho_11 +
    3 * (p_00 * p_11 - p_10 * p_01)

  if (is.na(rho_star)) {
    rho <- NA
    if (verbose) {
      message("Zero inflated Spearman correlation is undefined, returning NA")
    }
    return(rho)
  }



  return(rho_star)
}


#' @title Spearman
#'
#' @param compMat indices to correlate. Will split in half, and correlate the first half with the second half.
#' @return \code{numeric} rho* association value between x and y
#'
#'
#' @references scHOT
#' Ghazanfar, S., Lin, Y., Su, X. et al. Investigating higher-order interactions in
#'   single-cell data with scHOT. Nat Methods 17, 799–806 (2020).
#'   https://doi.org/10.1038/s41592-020-0885-x
#'
#' Zero-Inflated Correlation
#' Pimentel, Ronald Silva, "Kendall's Tau and Spearman's Rho for
#'   Zero-Inflated Data" (2009). Dissertations. 721.
#'   https://scholarworks.wmich.edu/dissertations/721
#'
#' @noRd

Spearman <- function(compMat) {
  verbose <- FALSE
  ZI <- FALSE
  # needs the original values, not the ranks
  numLen <- length(compMat) / 2
  x <- compMat[1:numLen]
  y <- compMat[(numLen + 1):(2 * numLen)]
  w <- rep(1, length(x))

  spearmanCorr <- wCorr::weightedCorr(x = x, y = y, weights = w, method = "Spearman")
  return(spearmanCorr)
}

#' @title ZISpearman
#'
#' @param compMat indices to correlate. Will split in half, and correlate the first half with the second half.
#' @return \code{numeric} rho* association value between x and y
#' @references scHOT
#' Ghazanfar, S., Lin, Y., Su, X. et al. Investigating higher-order interactions in
#'   single-cell data with scHOT. Nat Methods 17, 799–806 (2020).
#'   https://doi.org/10.1038/s41592-020-0885-x
#'
#' Zero-Inflated Correlation
#' Pimentel, Ronald Silva, "Kendall's Tau and Spearman's Rho for
#'   Zero-Inflated Data" (2009). Dissertations. 721.
#'   https://scholarworks.wmich.edu/dissertations/721
#'
#' @noRd

ZISpearman <- function(compMat) {
  verbose <- FALSE
  ZI <- TRUE
  # needs the original values, not the ranks
  numLen <- length(compMat) / 2
  x <- compMat[1:numLen]
  y <- compMat[(numLen + 1):(2 * numLen)]

  w <- rep(1, length(x))

  posx <- x > 0
  posy <- y > 0
  pospos <- posx & posy

  p_11 <- sum(w * pospos) / sum(w)
  p_00 <- sum(w * (!posx & !posy)) / sum(w)
  p_01 <- sum(w * (!posx & posy)) / sum(w)
  p_10 <- sum(w * (posx & !posy)) / sum(w)


  if (any(pospos) & p_11 > 0) {
    rho_11 <- wCorr::weightedCorr(x = x[pospos], y = y[pospos], weights = w[pospos], method = "Spearman")
  } else {
    rho <- NA
    if (verbose) {
      message("Zero inflated Spearman correlation is undefined, returning NA")
    }
    return(rho)
  }

  rho_star <- p_11 * (p_01 + p_11) * (p_10 + p_11) * rho_11 + 3 * (p_00 * p_11 - p_10 * p_01)

  if (is.na(rho_star)) {
    rho <- NA
    if (verbose) {
      message("Zero inflated Spearman correlation is undefined, returning NA")
    }
    return(rho)
  }

  return(rho_star)
}
