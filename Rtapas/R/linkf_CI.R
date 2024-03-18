#' Confidence intervals for the frequency of host-symbiont association
#'
#' From the matrix obtained in \code{\link[=prob_statistic]{prob_statistic()}},
#' compute the confidence intervals for the frequencies (or residual/corrected
#' frequencies) of the host-symbiont associations using a set of pairs of
#' posterior probability trees of host and symbiont.
#'
#' @param freqfun Global-fit method. Options are \code{"geoD"}
#'        (Geodesic Distances), \code{"paco"} (PACo) or \code{"paraF"}
#'        (ParaFit). It should be the same method used to obtain \code{"fx"}.
#'
#' @param x Matrix produced with \code{\link[=prob_statistic]{prob_statistic()}}
#'        for\code{"geoD"} (Geodesic Distances),
#'        \code{"paco"} (PACo) or \code{"paraF"} (ParaFit).
#'
#' @param fx Vector of statistics produced with
#'        \code{\link[=max_cong]{max_cong()}} or
#'        \code{\link[=max_incong]{max_incong}} for\code{"geoD"}
#'        (Geodesic Distances), \code{"paco"} (PACo) or \code{"paraF"}
#'        (ParaFit).
#'
#' @param c.level Confidence interval level. Default is \code{95} (95\%).
#'
#' @param barplot Default is \code{"TRUE"}, plots the distribution and
#'        confidence intervals of the frequencies.
#'
#' @param col.bar A vector of colors for the bars or bar components.
#'        By default, \code{"lightblue"} is used.
#'
#' @param col.ci A vector of colors for the confidence intervals arrows.
#'        By default, \code{"darkblue"} is used.
#'
#' @param ... Any graphical option admissible in
#'        \code{\link[=barplot]{barplot()}}
#'
#' @param y.lim Limits for the y axis.
#'
#' @return A dataframe with associations information (columns 1 and 2), the
#'         observed value of the frequencies for these associations (column 3),
#'         the mean, the minimum and the maximum value of the frequencies
#'         (columns 4, 5 and 6) obtained with the sets of posterior
#'         probability trees.
#'
#' @importFrom graphics arrows axis
#'
#' @export
#'
#' @examples
#' data(nuc_cp)
#' N = 10 #for the example, we recommend 1e+4 value
#' n = 8
#' # Maximizing incongruence
#' NPi <- max_incong(np_matrix, NUCtr, CPtr, n, N, method = "paco",
#'                   symmetric = FALSE, ei.correct = "sqrt.D",
#'                   percentile = 0.99, diff.fq = TRUE,
#'                   strat = "parallel", cl = 8)
#' # Loaded directly from dataset
#' # THSi <- trimHS_maxI(N, np_matrix, n)
#' # pp_treesPACo_incong <- prob_statistic(ths = THSi, np_matrix,
#' #                        NUC_500tr[1:5], CP_500tr[1:5], freqfun = "paco",
#' #                        NPi, symmetric = FALSE, ei.correct = "sqrt.D",
#' #                        percentile = 0.99, diff.fq = TRUE, res.fq = FALSE,
#' #                        below.p = FALSE, strat = "parallel", cl = 8)
#'
#' LFci <- linkf_CI (freqfun = "paco", x = pp_treesPACo_incong, fx = NPi,
#'                   c.level = 95, ylab = "Observed - Expected frequency")
#'
#'
linkf_CI <- function (freqfun = "paco", x, fx, c.level = 95, barplot = TRUE,
                      col.bar = "lightblue", col.ci = "darkblue", y.lim = NULL,
                      ...) {

  freqfun.choice <- c("geoD", "paco", "paraF")
  if (freqfun %in% freqfun.choice == FALSE)
    stop(writeLines("Invalid freqfun parameter.\r Correct choices are 'geoD',\n
                    'paco' or 'paraF'"))
  if (freqfun == "geoD") {
    GD01 <- x
    LFGD01 <- fx
    a <- 1 - (c.level/100)
    GD.LO <- apply(GD01, 2, quantile, a/2)
    GD.HI <- apply(GD01, 2, quantile, 1 - (a/2))
    GD.AV <- apply(GD01, 2, mean)
    df <- data.frame(LFGD01[, 1], LFGD01[, 2], LFGD01[, ncol(LFGD01)],
                     GD.LO, GD.HI, GD.AV)
    colnames(df) <- c("Taxa1", "Taxa2", "GD.Fq",
                      "GD.LO", "GD.HI", "GD.AV")
    if (barplot == TRUE) {
      link.fq <- barplot(GD.AV, xaxt = "n", horiz = FALSE,
                         cex.names = 0.6, las = 2, cex.axis = 0.8,
                         ylim = y.lim, col = col.bar,
                         ...)
      suppressWarnings(arrows(link.fq, GD.HI, link.fq,
                              GD.LO, length = 0, angle = 90, code = 3, col = col.ci))
      axis(side = 1, at = link.fq[1:length(GD.AV)], labels = LFGD01$HS,
           las = 2, tick = FALSE, line = 0.1, cex.axis = 0.5)
      return(df)
    }
    else {
      return(df)
    }
  }
  if (freqfun == "paco") {
    PACO01 <- x
    LFPACO01 <- fx
    a <- 1 - (c.level/100)
    PACO.LO <- apply(PACO01, 2, quantile, a/2)
    PACO.HI <- apply(PACO01, 2, quantile, 1 - (a/2))
    PACO.AV <- apply(PACO01, 2, mean)
    df <- data.frame(LFPACO01[, 1], LFPACO01[, 2], LFPACO01[, ncol(LFPACO01)], PACO.LO, PACO.HI, PACO.AV)
    colnames(df) <- c("Taxa1", "Taxa2", "PACO.Fq",
                      "PACO.LO", "PACO.HI", "PACO.AV")
    if (is.null(y.lim)){
      y.lim = c(min(PACO.LO), max(PACO.HI))
    } else {
      y <- y.lim
    }

    if (barplot == TRUE) {
      link.fq <- barplot(PACO.AV, xaxt = "n", horiz = FALSE,
                         cex.names = 0.6, las = 2, cex.axis = 0.8,
                         ylim = y.lim, col = col.bar,
                         ...)
      suppressWarnings(arrows(link.fq, PACO.HI, link.fq,
                              PACO.LO, length = 0, angle = 90, code = 3, col = col.ci))
      axis(side = 1, at = link.fq[1:length(PACO.AV)], labels = LFPACO01$HS,
           las = 2, tick = FALSE, line = 0.1, cex.axis = 0.5)
      return(df)
    }
    else {
      return(df)
    }
  }
  if (freqfun == "paraF") {
    PF01 <- x
    LFPF01 <- fx
    a <- 1 - (c.level/100)
    PF.LO <- apply(PF01, 2, quantile, a/2)
    PF.HI <- apply(PF01, 2, quantile, 1 - (a/2))
    PF.AV <- apply(PF01, 2, mean)
    df <- data.frame(LFPF01[, 1], LFPF01[, 2], LFPF01[, ncol(LFPF01)],
                     PF.LO, PF.HI, PF.AV)
    colnames(df) <- c("Taxa1", "Taxa2", "PF.Fq",
                      "PF.LO", "PF.HI", "PF.AV")
    if (barplot == TRUE) {
      link.fq <- barplot(PF.AV, xaxt = "n", horiz = FALSE,
                         cex.names = 0.6, las = 2, cex.axis = 0.8,
                         ylim = y.lim, col = col.bar,
                         ...)
      suppressWarnings(arrows(link.fq, PF.HI, link.fq,
                              PF.LO, length = 0, angle = 90, code = 3, col = col.ci))
      axis(side = 1, at = link.fq[1:length(PF.AV)], labels = LFPF01$HS,
           las = 2, tick = FALSE, line = 0.1, cex.axis = 0.5)
      return(df)
    }
    else {
      return(df)
    }
  }
}
