#' Franck's (2013) et al. test for Interaction
#'
#' This function calculates Franck's (2013) et al. test statistic, ACMIF, and corresponding p-value.
#'
#' @param x numeric matrix, \eqn{a \times b} data matrix where the number of row and column is corresponding to the number of factor levels.
#' @param nsim a numeric value, the number of Monte Carlo samples for computing an exact Monte Carlo p-value. The default value is 10000.
#' @param Elapsed_time logical: if \code{TRUE} the progress will be printed in the console.
#' @param alpha a numeric value, the level of the test. The default value is 0.05.
#' @param plot logical: if \code{TRUE} an interaction plot will be plotted.
#' @param vecolor character vector of length two, for visualizing the colors of lines in interaction plot. The default colors are blue and red.
#' @param linetype numeric vector of length two, for visualizing the line types in interaction plot. The default line types are 1 and 2.
#' @param report logical: if \code{TRUE} the result of the test is reported at the \code{alpha} level.
#'
#' @details Franck et al. (2013) derived a test statistic based on the “hidden additivity” structure.
#'  They defined this structure as “the levels of one factor belong in two or more groups such that within each group the effects of the two factors are additive but the groups may interact with the ungrouped factor”.
#'  To detect hidden additivity, Franck et al. (2013) divided the table of data into two sub-tables (based on the rows of the data matrix) and an interaction F-test was developed.
#'  Then, they performed a search over all possible configures of data and used the maximum of the interaction F-test as a test statistic. The hypothesis of no interaction is rejected when the maximum interaction F-test is large.
#'  If \code{plot} is \code{TRUE} an interaction plot will be plotted by displaying levels of column factor on the horizontal axis,
#'  levels of row factor using lines that are visually distinguished by line type and color, and the
#'  observed values on the vertical axis. Color and line type are used to display which levels of row factor are assigned to which
#'  groups based on the maximum F-values among all possible configurations. Note
#'  that the grouping colors and line types appear whether or not the Franck.test detects
#'  a significant non-additivity. The default colors are blue and red, and the default line types are one and two for the two groups. They can be customized by supplying arguments called \code{vecolor} and \code{linetype}.
#'  Note that the number of rows should be greater than two to perform the Franck.test. This test is powerful when there is a hidden additivity structure in the data set.
#'
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
#' @references
#'  Franck, C., Nielsen, D., Osborne, J.A. (2013). A method for detecting hidden additivity in two-factor unreplicated experiments.
#'  Computational Statistics and Data Analysis 67:95-104.
#'
#'  Franck, C., Osborne, J.A. (2016).  Exploring Interaction Effects in Two-Factor Studies using the hidden Package in R.
#'  R Journal 8 (1):159-172.
#'
#'  Shenavari, Z., Kharrati-Kopaei, M. (2018). A Method for Testing Additivity in
#'  Unreplicated Two-Way Layouts Based on Combining Multiple Interaction Tests. International Statistical Review
#'  86(3): 469-487.
#'
#' @examples
#' data(CNV)
#' Franck_test(CNV, nsim = 1000, Elapsed_time = FALSE)
#'
#' @importFrom stats pchisq pf qnorm var
#' @export
Franck_test <- function(x, nsim = 10000, alpha = 0.05, report = TRUE, plot = FALSE, vecolor = c("blue", "red"), linetype = c(1, 2), Elapsed_time = TRUE) {
  DNAME <- deparse1(substitute(x))
  if (!is.matrix(x)) {
    stop("The input should be a matrix")
  } else {
    bl <- nrow(x)
    tr <- ncol(x)
    n <- tr * bl
    if (bl < 3) {
      warning("Frank_test needs at least three levels for the row factor.")
      str <- Result_Franck(x, nsim = nsim, alpha = alpha, simu = NULL)$string
      out <- list(
        pvalue_exact = NA,
        pvalue_appro = NA,
        nsim = nsim,
        statistic = NA,
        data_name = DNAME,
        test = "Franck Test",
        Level = alpha,
        Result = str
      )
    } else {
      cch <- 2^(bl - 1) - 1
      statistics <- hh_f(x)
      simu <- rep(0, 0)
      if (Elapsed_time) {
        pb <- completed(nsim)
        for (i in 1:nsim) {
          simu[i] <- hh_f(matrix(rnorm(n), nrow = bl, ncol = tr))
          if (i == pb$pr[pb$j]) pb <- nextc(pb, i)
        }
      } else {
        for (i in 1:nsim) {
          simu[i] <- hh_f(matrix(rnorm(n), nrow = bl, ncol = tr))
        }
      }
      hidden <- mean(statistics < simu)
      adjpvalue <- (1 - pf(statistics, (tr - 1), (tr - 1) * (bl - 2))) * cch
      hidden_apr <- min(1, adjpvalue)
      qFranck <- quantile(simu, prob = 1 - alpha, names = FALSE)
      if (plot) {
        index <- Result_Franck(x, nsim = nsim, alpha = alpha, simu = simu)$index
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
        if (hidden < alpha) {
          str <- Result_Franck(x, nsim = nsim, alpha = alpha, simu = simu)$string
        } else {
          str <- paste0("The Franck_test could not detect any significant interaction at the ", paste0(100 * (alpha), "%"), " level.", " The estimated critical value of the Franck_test at the ", paste0(100 * (alpha), "%"), " level with ", nsim, " Monte Carlo samples is ", round(qFranck, 4), ".")
        }
      } else {
        str <- paste("A report has not been wanted! To have a report, change argument 'report' to TRUE.")
      }
      out <- list(
        pvalue_exact = hidden,
        pvalue_appro = hidden_apr,
        nsim = nsim,
        statistic = statistics,
        data_name = DNAME,
        test = "Franck Test",
        Level = alpha,
        Result = str
      )
    }
    structure(out, class = "ITtest")
  }
}
