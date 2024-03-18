#' Trims the H-S association matrix maximizing the congruence
#'
#' For N runs, it randomly chooses \code{n} unique one-to-one associations and
#' trims the H-S association matrix to include only the n associations.
#'
#' @param N Number of runs.
#'
#' @param HS Host-Symbiont association matrix.
#'
#' @param n Number of unique associations.
#'
#' @param check.unique if \code{TRUE} discards duplicated trimmed matrices. This
#'        alternative is recommended if \code{n} is small, because the
#'        probability of obtaining the same trimmed matrix in different runs
#'        increases as \code{n} decreases.
#'
#' @param strat Flag indicating whether execution is to be  \code{"sequential"}
#'        or \code{"parallel"}. Default is \code{"sequential"},
#'        resolves \R expressions sequentially in the current \R
#'        process. If \code{"parallel"} resolves \R expressions in parallel in
#'        separate \R sessions running in the background.
#'
#' @param cl Number of cluster to be used for parallel computing.
#'        \code{\link[parallelly:availableCores]{parallelly::availableCores()}}
#'        returns the number of clusters available.
#'        Default is \code{cl = 1} resulting in \code{"sequential"} execution.
#'
#'
#' @return A list of the N trimmed matrices.
#'
#' @examples
#' data(nuc_cp)
#' N = 10  #for the example, we recommend 1e+4 value
#' n = 15
#' TNC <- trimHS_maxC(N, np_matrix, n, check.unique = TRUE)
#'
#'
#' @import parallel
#'
#' @export
#'
trimHS_maxC <- function (N, HS, n, check.unique = TRUE,
                           strat = "sequential", cl = 1) {

  trim.int <- function (x, HS, n) {
    HS.LUT <- which(HS == 1, arr.ind = TRUE)
    HS.LUT <- cbind(HS.LUT, 1:nrow(HS.LUT))
    df <- as.data.frame(HS.LUT)
    hs.lut <- subset(df[sample(nrow(df)), ],
                     !duplicated(row) & !duplicated(col))
    if (nrow(hs.lut) < n) hs <- NULL else {
      hs.lut <- hs.lut[sample(nrow(hs.lut), n), ]
      hs <- diag(nrow(hs.lut))
      rownames(hs) <- rownames(HS[hs.lut[ ,1], ])
      colnames(hs) <- colnames(HS[ ,hs.lut[ ,2]])
      return(hs)
    }
  }

  strat.choice <- c("sequential", "parallel")
  if (strat %in% strat.choice == FALSE)
    stop(writeLines("Invalid strategy parameter"))

  if (strat == "sequential") {
    trim.HS <- lapply(1:N, trim.int, HS = HS, n = n)
    if (check.unique == TRUE) trim.HS <- unique(trim.HS)
    if (length(trim.HS) < N)
      warning("No. of trimmed H-S assoc. matrices < No. of runs")
    trim.HS[sapply(trim.HS, is.null)] <- NULL
    return(trim.HS)
  } else {
    cores <- parallelly::makeClusterPSOCK(workers = cl)
    trim.HS <- parallel::parLapply(cores, 1:N, trim.int, HS = HS, n = n)
    parallel::stopCluster(cores)
    if (check.unique == TRUE) trim.HS <- unique(trim.HS)
    if (length(trim.HS) < N)
      warning("No. of trimmed H-S assoc. matrices < No. of runs")
    trim.HS[sapply(trim.HS, is.null)] <- NULL
    return(trim.HS)
  }
}


