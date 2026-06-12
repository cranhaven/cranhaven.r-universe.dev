#' Kharrati-Kopaei and Miller's (2016) Test for Interaction
#'
#' This function calculates the test statistic for testing \eqn{H_0:} There is no interaction, and corresponding Monte Carlo p-value
#' proposed by Kharrati-Kopaei and Miller (2016).
#'
#' @param x a numeric matrix, \eqn{a \times b} data matrix where the number of row and column is corresponding to the number of factor levels.
#' @param nsim a numeric value, the number of Monte Carlo samples for computing an exact Monte Carlo p-value. The default value is 10000.
#' @param nc0 a numeric value, the number of Monte Carlo samples for computing the unbiasing constant \eqn{c_0}. The default value is 10000.
#' @param alpha a numeric value, the level of the test. The default value is 0.05.
#' @param report logical: if \code{TRUE} the result of the test is reported at the \code{alpha} level.
#'
#' @return An object of the class \code{ITtest}, which is a list inducing following components:
#' \item{pvalue_exact}{The calculated exact Monte Carlo p-value.}
#' \item{pvalue_appro}{is not available for \code{KKM_test}.}
#' \item{Nsim}{The number of Monte Carlo samples that are used to estimate p-value.}
#' \item{statistic}{The value of the test statistic.}
#' \item{data_name}{The name of the input dataset.}
#' \item{test}{The name of the test.}
#' \item{Level}{The level of test.}
#' \item{Result}{The result of the test at the alpha level with some descriptions on the type of significant interaction.}
#'
#' @details
#' Kharrati-Kopaei and Miller (2016) proposed a test statistic for testing interaction
#' based on inspecting all pairwise interaction contrasts (PIC).
#' This test depends on an unbiasing constant \eqn{c_0} that is calculated by a Monte Carlo simulation.
#' In addition, the null distribution of the test statistic is calculated by a Monte Carlo simulation. This test is not applicable when both \eqn{a} and \eqn{b} are less than three.
#' Note that this test procedure is powerful when significant interactions are caused by some data cells.
#'
#' @references Kharrati-Kopaei, M., Miller, A. (2016). A method for testing interaction in
#'  unreplicated two-way tables: using all pairwise interaction contrasts. Statistical
#'  Computation and Simulation 86(6):1203-1215.
#'
#'  Shenavari, Z., Kharrati-Kopaei, M. (2018). A Method for Testing Additivity in
#'  Unreplicated Two-Way Layouts Based on Combining Multiple Interaction Tests. International Statistical Review
#'  86(3): 469-487.

#' @examples
#' data(RDWW)
#' KKM_test(RDWW, nsim = 1000, nc0 = 1000)
#'
#' @export
KKM_test <- function(x, nsim = 1000, alpha = 0.05, report = TRUE, nc0 = 10000) {
  if (!is.matrix(x)) {
    stop("The input should be a matrix")
  } else {
    DNAME <- deparse1(substitute(x))
    y <- c(t(x))
    tr <- ncol(x)
    bl <- nrow(x)
    n <- tr * bl
    kp <- kpr(bl, tr)
    c0 <- C0(kp, n, nc0)
    statistics <- picf(y, kp, c0)
    simu <- PICfsim(nsim, kp, c0, n)
    PIC <- mean(statistics < simu)
    qKKM <- quantile(simu, prob = 1 - alpha, names = FALSE)
    if (report) {
      if (PIC < alpha) {
        str <- Result_KKM(x, simu = simu, nsim = nsim, alpha = alpha, nc0 = nc0)
      } else {
        str <- paste0("The KKM_test could not detect any significant interaction at the ", paste0(100 * (alpha), "%"), " level.", " The estimated critical value of the KKM_test at the ", paste0(100 * (alpha), "%"), " level with ", nsim, " Monte Carlo samples is ", round(qKKM, 4), ".")
      }
    } else {
      str <- paste("A report has not been wanted! To have a report, change argument 'report' to TRUE.")
    }
  }
  structure(
    list(
      pvalue_exact = PIC,
      pvalue_appro = "NULL",
      nsim = nsim,
      statistic = statistics,
      data_name = DNAME,
      test = "KKM Test",
      Level = alpha,
      Result = str
    ),
    class = "ITtest"
  )
}
