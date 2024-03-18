#' Plot the confidence intervals of Gini coefficient
#'
#' Computes and displays in a boxplot the Gini coefficient and their
#' confidence intervals of the frequency (or residual/corrected frequencies)
#' distributions of the estimated (in)congruence metric (with any of the three
#' global-fit methods) of the individual host-symbiont associations.
#'
#' @param LF_1 Vector of statistics produced with
#'        \code{\link[=max_cong]{max_cong()}} or
#'        \code{\link[=max_incong]{max_incong()}} for \code{"geoD"},
#'        \code{"paco"} or \code{"paraF"}.
#'
#' @param M01 Matrix produced with
#'        \code{\link[=prob_statistic]{prob_statistic()}} for \code{"geoD"},
#'        \code{"paco"} or \code{"paraF"} using \code{LF_1}.
#'
#' @param ... Any optional argument admissible in
#'        \code{\link[graphics:boxplot]{boxplot()}}
#'
#' @param ylab Title of the y label.
#'
#' @param plot Default is \code{"TRUE"}, plots the Gini coefficient and its
#'        confidence intervals in a boxplot.
#'
#' @return The Gini values obtained and their representation in a boxplot, with
#'         their confidence intervals.
#'
#' @section NOTE:
#'          It produces a conventional Gini coefficient (G)
#'          (Ultsch and Lötsch 2017) if all output values are positive, or
#'          a normalized Gini coefficient (G*) (Raffinetti et al. 2015) if
#'          negative values are produced due to corrected frequencies
#'          (if \code{res.fq = TRUE} or
#'          \code{diff.fq = TRUE}). For more details see
#'          \code{\link[GiniWegNeg:Gini_RSV]{Gini_RSV()}}.
#'
#' @references
#' Ultsch A., Lötsch J. (2017). A data science based standardized Gini index
#' as a Lorenz dominance preserving measure of the inequality of distributions.
#' PLOS ONE. 12:e0181572. \doi{10.1371/journal.pone.0181572}
#'
#' Raffinetti E., Siletti E., Vernizzi A. (2015). On the Gini coefficient
#' normalization when attributes with negative values are considered.
#' Stat Methods Appl. 24:507–521. \doi{10.1007/s10260-014-0293-4}
#'
#'
#'
#'
#' @importFrom GiniWegNeg Gini_RSV
#' @importFrom graphics boxplot text
#' @export
#'
#' @examples
#' data(nuc_cp)
#' N = 10 #for the example, we recommend 1e+4 value
#' n = 15
#' # Maximizing congruence
#' NPc_PACo <- max_cong(np_matrix, NUCtr, CPtr, n, N, method = "paco",
#'                 symmetric = FALSE, ei.correct = "sqrt.D",
#'                 percentile = 0.01, res.fq = FALSE,
#'                 strat = "parallel", cl = 8)
#'
#' # Loaded directly from dataset
#' # THSC <- trimHS_maxC(N, np_matrix, n)
#' # pp_treesPACo_cong <- prob_statistic(ths = THSc, np_matrix, NUC_500tr[1:10],
#' #                         CP_500tr[1:10], freqfun = "paco", NPc_PACo,
#' #                         symmetric = FALSE, ei.correct = "sqrt.D",
#' #                         percentile = 0.01, correction = "none",
#' #                         strat = "parallel", cl = 8)
#'
#' gini_ci(LF_1 = NPc_PACo, M01 = pp_treesPACo_cong,
#'          ylab = "Gini Coefficient (G)",
#'          plot = TRUE, ylim = c(0.3, 0.8))
#' abline(h = 1/3) # because res.fq = TRUE
#'
gini_ci <- function (LF_1, M01,
                     ylab = "Gini coefficient", plot = TRUE, ...){

  Gini01 <- unlist(Gini_RSV( LF_1[, ncol(LF_1)]))
  GiniM01 <- unlist(apply(M01, 1, Gini_RSV))

  if(plot == TRUE){
    box <- boxplot(GiniM01, show.names = TRUE, ylab = ylab, ...)
    text(1, Gini01, "*", cex = 2, col = "red")
  }

  box <- boxplot(GiniM01, plot = FALSE)
  lower_CI <- box$conf[1,]
  upper_CI <- box$conf[2,]
  Mean <- mean(GiniM01)
  Observed <- Gini01

  values <- data.frame(Observed, Mean, lower_CI, upper_CI)

  return(values)
}

