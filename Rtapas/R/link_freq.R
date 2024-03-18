#' Frequency of host-symbiont association
#'
#' Determines the frequency (or residual/corrected frequency) of each
#' host-symbiont association in a given percentile of cases that maximize
#' phylogenetic (in)congruence.
#'
#' @param x List of trimmed matrices produced by
#' \code{\link[=trimHS_maxC]{trimHS_maxC()}} or
#' \code{\link[=trimHS_maxI]{trimHS_maxI()}}.
#'
#' @param fx Vector of statistics produced with \code{\link[=geo_D]{geo_D()}},
#'        \code{\link[=paco_ss]{paco_ss()}} or \code{\link[=paraF]{paraF()}}
#'
#' @param HS Host-symbiont association matrix.
#'
#' @param percentile Percentile to evaluate (\emph{p}). Default is
#'        \code{0.01} (1\%).
#'
#' @param sep Character that separates host and symbiont labels.
#'
#' @param below.p Determines whether frequencies are to be computed below or
#'        above the percentile set. Default is \code{TRUE}.
#'
#' @param res.fq Determines whether a correction to avoid one-to-one
#'        associations being overrepresented in the percentile evaluated.
#'        If \code{TRUE} (default) a residual frequency value (observed -
#'        expected frequency) is computed for each host-symbiont association.
#'
#' @section NOTE:
#'        The \code{res.fq = TRUE} correction is recommended in tanglegrams with
#'        large portion of multiple (as opposed to one-to-one) host-symbiont
#'        associations. For future usage, frequencies of host-symbiont
#'        associations above a given percentile values can also be computed
#'        setting \code{below.p = FALSE}.
#'
#'
#' @return A dataframe with host-symbiont associations in rows. The first and
#'         second columns display the names of the host and symbiont terminals,
#'         respectively. The third column designates the host-symbiont
#'         association by pasting the names of the terminals, and the fourth
#'         column displays the frequency of occurrence of each host-symbiont
#'         association. If \code{res.fq = TRUE}, column 5 displays the
#'         corrected frequencies as a residual.
#'
#' @examples
#' data(amph_trem)
#' N = 10 #for the example, we recommend 1e+4 value
#' n = 8
#'
#' TAM <- trimHS_maxC(N, am_matrix, n, check.unique = TRUE)
#' PACO <- paco_ss(TAM, amphipod, trematode, symmetric = TRUE,
#'                 ei.correct = "sqrt.D", strat = "parallel", cl = 8)
#' LFPACO <- link_freq(TAM, PACO, am_matrix, percentile = 0.01,
#'                   below.p = TRUE, res.fq = TRUE)
#'
#' @export
link_freq <- function (x, fx, HS, percentile = 0.01,
                       sep= "-", below.p = TRUE, res.fq = TRUE) {
  if (below.p == TRUE)
    percent <- which(fx <= quantile(fx, percentile, na.rm = TRUE)) else
      percent <- which(fx >= quantile(fx, percentile, na.rm = TRUE))
    trim.HS <- x[percent]
    paste.link.names <- function(X, sep) {
      X.bin <- which(X > 0, arr.ind = TRUE)
      Y <- diag(nrow(X.bin))
      Y <- diag(nrow(X.bin))
      rownames(Y) <- rownames(X)[X.bin[, 1]]
      colnames(Y) <- colnames(X)[X.bin[, 2]]
      pln <- paste(rownames(Y), colnames(Y), sep = sep)
      return(pln)
    }
    link.names <- t(sapply(trim.HS, paste.link.names, sep = sep))
    lf <- as.data.frame(table(link.names))
    HS.LUT <- which(HS == 1, arr.ind = TRUE)
    linkf <- as.data.frame(cbind(rownames(HS)[HS.LUT[, 1]],
                                 colnames(HS)[HS.LUT[, 2]]))
    colnames(linkf) <- c('H', 'S')
    linkf$HS <- paste(linkf[, 1], linkf[, 2], sep = sep)
    linkf$Freq <- rep(0, nrow(linkf))
    linkf[match(lf[, 1],linkf[, 3]), 4] <- lf[, 2]
    linkf2 <- linkf
    #
    if (res.fq == TRUE) {
      link.names.all <- t(sapply(x, paste.link.names, sep = sep))
      lf.all <- as.data.frame(table(link.names.all))
      linkf.all <- as.data.frame(cbind(rownames(HS)[HS.LUT[, 1]],
                                       colnames(HS)[HS.LUT[, 2]]))
      colnames(linkf.all) <- c('H', 'S')
      linkf.all$HS <- paste(linkf.all[, 1], linkf.all[, 2], sep = sep)
      linkf.all$Freq <- rep(0, nrow(linkf.all))
      linkf.all[match(lf.all[,1], linkf.all[, 3]), 4] <- lf.all[, 2]
      w <- linkf.all[, 4]
      w <- as.matrix(w*0.01)
      wFq <- linkf$Freq - w
      linkf$wFq <- wFq
    } else linkf <- linkf2
    return(linkf)
}
