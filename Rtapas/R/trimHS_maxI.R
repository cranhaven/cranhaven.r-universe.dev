#' Trims the H-S association matrix maximizing the incongruence
#'
#' For N runs, it randomly chooses \code{n} associations and trims the H-S
#' association matrix to include them, allowing both single and multiple
#' associations.
#'
#' @param N Number of runs.
#'
#' @param HS Host-Symbiont association matrix.
#'
#' @param n Number of associations.
#'
#' @param check.unique if \code{TRUE} discards duplicated trimmed matrices. This
#'        alternative is recommended if \code{n} is small, because the
#'        probability of obtaining the same trimmed matrix in different runs
#'        increases as \code{n} decreases.
#'
#' @return A list of the N trimmed matrices.
#'
#' @export
#'
#' @examples
#' data(nuc_cp)
#' N = 10  #for the example, we recommend 1e+4 value
#' n = 15
#' TNC <- trimHS_maxI(N, np_matrix, n, check.unique = TRUE)
#'
trimHS_maxI <- function (N, HS, n, check.unique = TRUE) {

  trim.intI <- function (HS, n) {
    HS.LUT <- which(HS == 1, arr.ind = TRUE)
    HS.LUT <- cbind(HS.LUT, 1:nrow(HS.LUT))
    LH <- LS <- 2
    while(LH <= 2 | LS <= 2) {  # skip configurations with less than
      HS.trim <- HS.LUT[sample(nrow(HS.LUT), n), ] # three H or three S
      LH <- length(unique(HS.trim[, 1]))
      LS <- length(unique(HS.trim[, 2]))
    }
    hs <- HS
    hs[ , ] <- 0
    for (j in 1:nrow(HS.trim)) {
      hs[HS.trim[j,1], HS.trim[j,2]] <- 1
    }
    hs <- hs[-which(rowSums(hs) == 0), -which(colSums(hs) == 0)]
    return(hs)
  }

  trimI.HS <- replicate(N, trim.intI(HS = HS, n = n), simplify = FALSE)
  if (check.unique == TRUE) trimI.HS <- unique(trimI.HS)
  trimI.HS[sapply(trimI.HS, is.null)] <- NULL
  return(trimI.HS)
}
