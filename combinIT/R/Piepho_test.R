#' Piepho's (1994) Test for Interaction
#'
#' This function tests the interaction based on a statistic proposed by Piepho (1994).
#' This function reports Piepho's test statistic, an asymptotic p-value, and a Monte Carlo p-value.
#'
#' @param x numeric matrix, \eqn{a \times b} data matrix where the number of row and column is corresponding to the number of factor levels.
#' @param nsim a numeric value, the number of Monte Carlo samples for computing an exact Monte Carlo p-value. The default value is 10000.
#' @param alpha a numeric value, the level of the test. The default value is 0.05.
#' @param report logical: if \code{TRUE} the result of the test is reported at the \code{alpha} level.
#'
#' @return An object of the class \code{ITtest}, which is a list inducing following components:
#' \item{pvalue_exact}{The calculated exact Monte Carlo p-value.}
#' \item{pvalue_appro}{The asymptotic p-value.}
#' \item{statistic}{The value of the test statistic.}
#' \item{Nsim}{The number of Monte Carlo samples that are used to estimate p-value.}
#' \item{data_name}{The name of the input dataset.}
#' \item{test}{The name of the test.}
#' \item{Level}{The level of test.}
#' \item{Result}{The result of the test at the alpha level with some descriptions on the type of significant interaction.}
#'
#' @details Piepho (1994) proposed three test statistics. The third one is
#'  based on Grubbs’ (1948) type estimator of variance for the level of the row factor.
#'  This type of estimator is used in this function. Piepho (1994) proposed an asymptotic distribution of test statistic; however, a Monte Carlo method is used to calculate the p-value.
#'  The Piepho test is not applicable when the row number of the data matrix is less than three. Note that Piepho’s test is powerful for detecting interactions when the Grubbs’ type estimators of variances are heterogeneous across the levels of one factor.
#'
#' @references Piepho, H. P. (1994). On Tests for Interaction in a Nonreplicated Two-Way Layout. Australian
#' Journal of Statistics 36:363-369.
#'
#' Shenavari, Z., Kharrati-Kopaei, M. (2018). A Method for Testing Additivity in
#' Unreplicated Two-Way Layouts Based on Combining Multiple Interaction Tests. International Statistical Review
#' 86(3): 469-487.
#'
#' Grubbs, F.E. (1948). On Estimating Precision of Measuring Instruments and Product Variability. Journal of the American Statistical Association 43(242): 243-264.
#'
#' @examples
#' data(MVGH)
#' Piepho_test(MVGH, nsim = 1000)
#'
#' @export
Piepho_test <- function(x, nsim = 10000, alpha = 0.05, report = TRUE) {
  if (!is.matrix(x)) {
    stop("The input should be a matrix")
  } else {
    DNAME <- deparse1(substitute(x))
    tr <- ncol(x)
    bl <- nrow(x)
    n <- tr * bl
    if (bl < 3) {
      warning("Piepho_test needs at least 3 levels for the row factor")
      str <- Result_Piepho(x, nsim = nsim, alpha = alpha, simu = NULL)
      out <- list(
        pvalue_exact = NA,
        pvalue_appro = NA,
        nsim = nsim,
        statistic = NA,
        data_name = DNAME,
        test = "Piepho Test",
        Level = alpha,
        Result = str
      )
    } else {
      statistics <- piephoC(x, bl, tr)
      simu <- Piephosim(nsim, bl, tr)
      pieph <- mean(statistics < simu, na.rm = TRUE)
      qPiepho <- quantile(simu, prob = 1 - alpha, names = FALSE, na.rm = TRUE)
      df <- bl - 1
      asypieph <- 1 - pchisq(statistics, df = df)
      R <- x - matrix(rowMeans(x), bl, tr) - matrix(colMeans(x), bl, tr, byrow = TRUE) + mean(x)
      W <- rowSums(R^2)
      sigmahat <- (bl * (bl - 1) * W - sum(W)) / ((bl - 1) * (bl - 2) * (tr - 1))
      if (report) {
        if (pieph < alpha) {
          str <- Result_Piepho(x, nsim = nsim, alpha = alpha, simu = simu)
        } else {
          str <- paste0("The Piepho_test could not detect any significant interaction at the ", paste0(100 * (alpha), "%"), " level.", " The estimated critical value of the Piepho_test at the ", paste0(100 * (alpha), "%"), " level with ", nsim, " Monte Carlo samples is ", round(qPiepho, 4), ".")
        }
      } else {
        str <- paste("A report has not been wanted! To have a report, change argument 'report' to TRUE.")
      }
      out <- list(
        pvalue_exact = pieph,
        pvalue_appro = asypieph,
        nsim = nsim,
        statistic = statistics,
        data_name = DNAME,
        test = "Piepho Test",
        Level = alpha,
        Result = str
      )
    }
  }
  structure(out, class = "ITtest")
}
