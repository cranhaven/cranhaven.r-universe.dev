#' Kharrati-Kopaei and Sadooghi-Alvandi's (2007) test for interaction
#'
#' This function calculates Kharrati-Kopaei and Sadooghi-Alvandi's test statistic and corresponding p-value for testing interaction.
#'
#' @param x numeric matrix, \eqn{a \times b} data matrix where the number of row and column is corresponding to the number of factor levels.
#' @param nsim a numeric value, the number of Monte Carlo samples for computing an exact Monte Carlo p-value. The default value is 10000.
#' @param Elapsed_time logical: if \code{TRUE} the progress will be printed in the console.
#' @param alpha a numeric value, the level of the test. The default value is 0.05.
#' @param plot logical: if \code{TRUE} an interaction plot will be plotted.
#' @param vecolor character vector with length two, for visualizing the colors of lines in interaction plot. The default colors are blue and red.
#' @param linetype numeric vector with length two, for visualizing the line types in interaction plot. The default line types are 1 and 2.
#' @param report logical: if \code{TRUE} the result of the test is reported at the \code{alpha} level.
#'
#' @details  Suppose that \eqn{a \ge b} and \eqn{b \ge 4}. Consider the \eqn{l}-th division of the data table into two sub-tables,
#'  obtained by putting \eqn{a_1} (\eqn{2 \le a_1 \le a-2}) rows in the first sub-table and the remaining \eqn{a_2} rows in the second sub-table (\eqn{a_1+a_2=a}).
#'  Let RSS1 and RSS2 denote the residual sum of squares for these two sub-tables, respectively. For a particular division \eqn{l}, let \eqn{F_l=max\{F_l,1/F_l\}}
#'  where \eqn{F_l=(a_2-1)RSS1/((a_1-1)RSS2)} and let \eqn{P_l} denote the corresponding p-value.
#'  Kharrati-Kopaei and Sadooghi-Alvandi (2007) proposed their test statistic as the minimum value of \eqn{P_l} over \eqn{l=1,…,2^{(a-1)}-a-1} all possible divisions of the table.
#'  If \code{plot} is \code{TRUE} an interaction plot will be plotted by displaying levels of column factor on the horizontal axis,
#'  levels of row factor using lines that are visually distinguished by line type and color, and the
#'  observed values on the vertical axis. Color and line type are used to display which levels of row factor are assigned to which
#'  sub-tables based on the minimum p-values among all possible configurations. Note
#'  that the grouping colors and line types appear whether or not the KKSA.test detects
#'  a significant non-additivity. The default colors are blue and red, and the default line types are one and two for the two sub-tables. They can be customized by supplying arguments called \code{vecolor} and \code{linetype}.
#'  Note that this method of testing requires that the data matrix has more than three
#'  rows. This test procedure is powerful for detecting interaction when the magnitude of interaction effects is heteroscedastic across the sub-tables of observations.
#'
#' @return An object of the class \code{ITtest}, which is a list inducing following components:
#' \item{pvalue_exact}{The calculated exact Monte Carlo p-value.}
#' \item{pvalue_appro}{The Bonferroni-adjusted p-value is calculated.}
#' \item{statistic}{The value of the test statistic.}
#' \item{Nsim}{The number of Monte Carlo samples that are used to estimate p-value.}
#' \item{data_name}{The name of the input dataset.}
#' \item{test}{The name of the test.}
#' \item{Level}{The level of test.}
#' \item{Result}{The result of the test at the alpha level with some descriptions on the type of significant interaction.}
#'
#'
#' @references Kharrati-Kopaei, M., Sadooghi-Alvandi, S.M. (2007). A New Method for
#'  Testing Interaction in Unreplicated Two-Way Analysis of Variance. Communications
#'  in Statistics-Theory and Methods 36:2787–2803.
#'
#'  Shenavari, Z., Kharrati-Kopaei, M. (2018). A Method for Testing Additivity in
#'  Unreplicated Two-Way Layouts Based on Combining Multiple Interaction Tests. International Statistical Review
#'  86(3): 469-487.
#'
#' @examples
#' data(IDCP)
#' KKSA_test(IDCP, nsim = 1000, Elapsed_time = FALSE)
#'
#' @export
KKSA_test <- function(x, nsim = 10000, alpha = 0.05, report = TRUE, plot = FALSE, vecolor = c("blue", "red"), linetype = c(1, 2), Elapsed_time = TRUE) {
  if (!is.matrix(x)) {
    stop("The input should be a matrix")
  } else {
    DNAME <- deparse1(substitute(x))
    bl <- nrow(x)
    tr <- ncol(x)
    n <- tr * bl
    if (bl < 4) {
      warning("KKSA_test needs at least four levels for the row factor.")
      str <- Result_KKSA(x, nsim = nsim, alpha = alpha, simu = NULL)$string
      out <- list(
        pvalue_exact = NA,
        pvalue_appro = NA,
        nsim = nsim,
        statistic = NA,
        data_name = DNAME,
        test = "KKSA Test",
        Level = alpha,
        Result = str
      )
    } else {
      cck <- 2^(bl - 1) - 1 - bl
      statistics <- kk_f(x)
      simu <- rep(0, 0)
      if (Elapsed_time) {
        pb <- completed(nsim)
        for (i in 1:nsim) {
          simu[i] <- kk_f(matrix(rnorm(n), nrow = bl))
          if (i == pb$pr[pb$j]) pb <- nextc(pb, i)
        }
      } else {
        for (i in 1:nsim) {
          simu[i] <- kk_f(matrix(rnorm(n), nrow = bl))
        }
      }
      KKSA_p <- mean(statistics > simu)
      KKSA_p_apr <- statistics * cck
      KKSA_p_apr <- min(1, KKSA_p_apr)
      qKKSA <- quantile(simu, prob = alpha, names = FALSE)
      if (plot) {
        index <- Result_KKSA(x, nsim = nsim, alpha = alpha, simu = simu)$index
        color <- 1:bl
        color[index] <- vecolor[1]
        color[-index] <- vecolor[2]
        ltype <- 1:bl
        ltype[index] <- linetype[1]
        ltype[-index] <- linetype[2]
        oldpar <- par(mfcol = c(1, 1))
        on.exit(par(oldpar))
        par(mfcol = c(1, 1), mai = c(0.45, 0.38, 0.10, 0.55), tck = 0.01, mgp = c(1, 0, 0), xpd = TRUE)
        matplot(t(x), type = "b", xaxt = "n", ylab = "Observed values", xlab = "Column", col = color, lwd = 2, lty = ltype)
        matpoints(t(x), type = "p", pch = as.character(1:bl), col = "black")
        axis(1, at = 1:tr, labels = 1:tr, cex.axis = 1)
        legend(tr + 0.03, max(x), rep(paste0("row", 1:bl)), lty = ltype, bty = "n", cex = 0.60, col = color, lwd = 2)
      }
      if (report) {
        if (KKSA_p < alpha) {
          str <- Result_KKSA(x, nsim = nsim, alpha = alpha, simu = simu)$string
        } else {
          str <- paste0("The KKSA_test could not detect any significant interaction at the ", paste0(100 * (alpha), "%"), " level.", " The estimated critical value of the KKSA_test at the ", paste0(100 * (alpha), "%"), " level with ", nsim, " Monte Carlo samples is ", round(qKKSA, 4), ".")
        }
      } else {
        str <- paste0("A report has not been wanted! To have a report, change argument 'report' to TRUE.")
      }
      out <- list(
        pvalue_exact = KKSA_p,
        pvalue_appro = KKSA_p_apr,
        nsim = nsim,
        statistic = statistics,
        data_name = DNAME,
        test = "KKSA Test",
        Level = alpha,
        Result = str
      )
    }
    structure(out, class = "ITtest")
  }
}
