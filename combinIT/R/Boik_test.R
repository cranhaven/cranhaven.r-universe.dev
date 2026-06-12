#' Boik's (1993) Locally Best Invariant (LBI) Test
#'
#' This function calculates the LBI test statistic for testing the null hypothesis \eqn{H_0:} There is no interaction.
#' It returns an exact p-value when \eqn{p=2} where \eqn{p=min\{a-1,b-1\}}. It returns an exact Monte Carlo p-value when \eqn{p>2}. It also provides an asymptotic chi-squared p-value. Note that the p-value of the Boik.test is always one when \eqn{p=1}.
#'
#' @param x a numeric matrix, \eqn{a \times b} data matrix where the number of row and column is corresponding to the number of factor levels.
#' @param nsim a numeric value, the number of Monte Carlo samples for calculating an exact Monte Carlo p-value. The default value is 10000.
#' @param alpha a numeric value, the level of the test. The default value is 0.05.
#' @param report logical: if \code{TRUE} the result of the test is reported at the \code{alpha} level.
#'
#' @return An object of the class \code{ITtest}, which is a list inducing following components:
#' \item{pvalue_exact}{An exact Monte Carlo p-value when \eqn{p>2}. For \eqn{p=2}, an exact p-value is calculated.}
#' \item{pvalue_appro}{An chi-squared asymptotic p-value.}
#' \item{statistic}{The value of test statistic.}
#' \item{Nsim}{The number of Monte Carlo samples that are used to estimate p-value.}
#' \item{data_name}{The name of the input dataset.}
#' \item{test}{The name of the test.}
#' \item{Level}{The level of test.}
#' \item{Result}{The result of the test at the alpha level with some descriptions on the type of significant interaction.}
#'
#'
#' @details The LBI test statistic is \eqn{T_{B93}=(tr(R'R))^2/(p tr((R'R)^2))} where \eqn{p=min\{a-1,b-1\}} and \eqn{R} is the residual
#'   matrix of the input data matrix, \eqn{x}, under the null hypothesis \eqn{H_0:} There is no interaction. This test rejects the null hypothesis of no interaction when \eqn{T_{B93}} is small.
#'   Boik (1993) provided the exact distribution of \eqn{T_{B93}} when \eqn{p=2} under \eqn{H_0}. In addition, he provided an asymptotic distribution of \eqn{T_{B93}} under \eqn{H_0} when \eqn{q} tends to infinity where \eqn{q=max\{a-1,b-1\}}.
#'   Note that the LBI test is powerful when the \eqn{a \times b} matrix of interaction terms has small rank and one singular value dominates the remaining singular values or
#'   in practice, if the largest eigenvalue of \eqn{RR'} is expected to dominate the remaining eigenvalues.
#'
#'
#' @references Boik, R.J. (1993). Testing additivity in two-way classifications
#'  with no replications: the locally best invariant test. Journal of Applied
#'  Statistics 20(1): 41-55.
#'
#'  Shenavari, Z., Kharrati-Kopaei, M. (2018). A Method for Testing Additivity in
#'  Unreplicated Two-Way Layouts Based on Combining Multiple Interaction Tests. International Statistical Review
#'  86(3): 469-487.
#'
#' @examples
#' data(MVGH)
#' Boik_test(MVGH, nsim = 1000)
#' @importFrom stats median pbeta rnorm qbeta
#' @export
Boik_test <- function(x, nsim = 10000, alpha = 0.05, report = TRUE) {
  if (!is.matrix(x)) {
    stop("The input should be a matrix")
  } else {
    DNAME <- deparse1(substitute(x))
    tr <- ncol(x)
    bl <- nrow(x)
    n <- tr * bl
    p <- min(tr - 1, bl - 1)
    q <- max(tr - 1, bl - 1)
    statistics <- Bfc(x, bl, tr, p)
    Tb <- (1 / statistics - 1)
    T0 <- p * q * Tb / 2
    df <- (p + 2) * (p - 1) / 2
    if (p == 1) {
      asyboik_p <- 1
      simu <- Bfsim(nsim, bl, tr, p)
      boik_p <- mean(statistics >= simu)
    }
    if (p > 2) {
      simu <- Bfsim(nsim, bl, tr, p)
      boik_p <- mean(statistics >= simu)
      asyboik_p <- 1 - pchisq(T0, df)
      qBoik <- quantile(simu, prob = alpha, names = FALSE)
    }
    if (p == 2) {
      boik_p <- 1 - pbeta(Tb, 1, (q - 1) / 2)
      asyboik_p <- 1 - pchisq(T0, df)
      qBoik <- qbeta(1 - alpha, 1, (q - 1) / 2)
      qBoik <- 1 / (qBoik + 1)
    }
    if (report) {
      if (boik_p < alpha) {
        str <- Result_Boik(x, nsim = nsim, alpha = alpha, simu = simu)
      } else {
        if (p == 2) {
          str <- paste0("The Boik_test could not detect any significant interaction at the ", paste0(100 * (alpha), "%"), " level.", " The exact critical value of the Boik_test at the ", paste0(100 * (alpha), "%"), " level is ", round(qBoik, 4), ".")
        }
        if (p > 2) {
          str <- paste0("The Boik_test could not detect any significant interaction at the ", paste0(100 * (alpha), "%"), " level.", " The exact critical value of the Boik_test at the ", paste0(100 * (alpha), "%"), " level is ", round(qBoik, 4), ".")
        }
        if (p == 1) {
          str <- paste0("The Boik_test could not detect any significant interaction at the ", paste0(100 * (alpha), "%"), " level.", " The exact critical value of the Boik_test is ", 1, ".")
        }
      }
    } else {
      str <- paste("A report has not been wanted! To have a report, change argument 'report' to TRUE.")
    }
    out <- list(
      pvalue_exact = boik_p,
      pvalue_appro = asyboik_p,
      nsim = nsim,
      statistic = statistics,
      data_name = DNAME,
      test = "Boik Test",
      Level = alpha,
      Result = str
    )
  }
  structure(out, class = "ITtest")
}
