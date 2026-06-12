#' Malik's (2016) et al. Test for Interaction
#'
#' The Malik's (2016) et al. test statistic is calculated and the corresponding exact p-value is calculated by a Monte Carlo simulation.
#'
#' @param x numeric matrix, \eqn{a \times b} data matrix where the number of row and column is corresponding to the number of factor levels.
#' @param nsim a numeric value, the number of Monte Carlo samples for computing an exact Monte Carlo p-value. The default value is 10000.
#' @param Elapsed_time logical: if \code{TRUE} the progress will be printed in the console.
#' @param alpha a numeric value, the level of the test. The default value is 0.05.
#' @param report logical: if \code{TRUE} the result of the test is reported at the \code{alpha} level.
#'
#' @return An object of the class \code{ITtest}, which is a list inducing following components:
#' \item{pvalue_exact}{The calculated exact Monte Carlo p-value.}
#' \item{pvalue_appro}{is not available for \code{Malik_test}.}
#' \item{statistic}{The value of the test statistic.}
#' \item{Nsim}{The number of Monte Carlo samples that are used to estimate p-value.}
#' \item{data_name}{The name of the input dataset.}
#' \item{test}{The name of the test.}
#' \item{Level}{The level of test.}
#' \item{Result}{The result of the test at the alpha level with some descriptions on the type of significant interaction.}
#'
#' @details
#'  Malik (2016) et al. proposed to partition
#'  the residuals into three clusters using a suitable clustering method like “k-means clustering”.
#'  The hypothesis of no interaction can be interpreted as the effect of the three
#'  clusters are equal. Therefore, the result of the test may depend on the method of clustering. In this package, clustering is done by \code{kmeans} function in \code{RcppArmadillo}. The \code{speed_mode} parameter on the kmeans clustering was set as \code{static_subset}.
#'  Note that the Malik's et al. test performs well when there are some outliers in the residuals; i.e. some cells produce large negative or positive residuals due to the significant interaction.
#'  Further, the distribution of the Malik's et al. test statistic is not known under additivity and the corresponding p-value is calculated by a Monte Carlo simulation.

#' @references Malik, W.A., Mohring, J., Piepho, H.P. (2016). A
#' clustering-based test for non-additivity in an unreplicated two-way layout.
#' Communications in Statistics-Simulation and Computation 45(2):660-670.
#'
#' Shenavari, Z., Kharrati-Kopaei, M. (2018). A Method for Testing Additivity in
#' Unreplicated Two-Way Layouts Based on Combining Multiple Interaction Tests. International Statistical Review
#' 86(3): 469-487.
#'
#' @examples
#' data(IDCP)
#' Malik_test(IDCP, nsim = 1000, Elapsed_time = FALSE)
#'
#' @export
Malik_test <- function(x, nsim = 10000, alpha = 0.05, report = TRUE, Elapsed_time = TRUE) {
  if (!is.matrix(x)) {
    stop("The input should be a matrix")
  } else {
    DNAME <- deparse1(substitute(x))
    tr <- ncol(x)
    bl <- nrow(x)
    n <- tr * bl
    block <- gl(bl, tr)
    treatment <- gl(tr, 1, bl * tr)
    y <- c(t(x))
    statistic <- M_f(x)
    simu <- rep(0, 0)
    if (Elapsed_time) {
      pb <- completed(nsim)
      for (i in 1:nsim) {
        x0 <- matrix(rnorm(n), nrow = bl)
        y0 <- c(t(x0))
        simu[i] <- M_f(x = x0)
        if (i == pb$pr[pb$j]) pb <- nextc(pb, i)
      }
    } else {
      for (i in 1:nsim) {
        x0 <- matrix(rnorm(n), nrow = bl)
        y0 <- c(t(x0))
        simu[i] <- M_f(x = x0)
      }
    }
    malik <- mean(statistic < simu)
    qMalik <- quantile(simu, prob = 1 - alpha, names = FALSE)
    if (report) {
      if (malik < alpha) {
        str <- Result_Malik(x, simu = simu, alpha = alpha, nsim = nsim)
      } else {
        str <- paste0("The Malik_test could not detect any significant interaction at the ", paste0(100 * (alpha), "%"), " level.", " The estimated critical value of the Malik_test at the ", paste0(100 * (alpha), "%"), " level with ", nsim, " Monte Carlo samples is ", round(qMalik, 4), ".")
      }
    } else {
      str <- paste("A report has not been wanted! To have a report, change argument 'report' to TRUE.")
    }
    structure(
      list(
        pvalue_exact = malik,
        pvalue_appro = "NULL",
        nsim = nsim,
        statistic = statistic,
        data_name = DNAME,
        test = "Malik Test",
        Level = alpha,
        Result = str
      ),
      class = "ITtest"
    )
  }
}
