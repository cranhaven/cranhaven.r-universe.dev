#' Sample Size for Comparing Two Proportions
#'
#' Calculates the required sample size for a two-group comparison of
#' proportions based on the method of Casagrande, Pike, and Smith (1978).
#'
#' @param p1 A scalar. The anticipated proportion in Arm 1 (e.g., control).
#' @param p2 A scalar. The anticipated proportion in Arm 2 (e.g., experimental).
#' @param alpha A scalar. The one-sided Type I error rate.
#' @param beta A scalar. The Type II error rate (1 - power).
#' @param r A scalar. The randomization ratio of Arm 2 to Arm 1 ($n_2/n_1$).
#'
#' @return A numeric vector of length two:
#' \itemize{
#'   \item The required sample size for Arm 1 ($n_1$).
#'   \item The total required sample size ($n_1 + n_2$).
#' }
#' @seealso \url{https://www2.ccrb.cuhk.edu.hk/stat/proportion/Casagrande.htm}
#'
#' @examples
#' Two.Prop.Test.Sample.Size(p1=0.27, p2=0.47, alpha = 0.10, beta=0.2, r=2)
#'
#' @export
#'
Two.Prop.Test.Sample.Size <- function(p1, p2, alpha, beta, r) {

  delta <- abs(p1 - p2)
  Pbar <- (p1 + r * p2) / (r + 1)
  Qbar <- 1 - Pbar

  mp <- (qnorm(alpha, lower.tail = FALSE) * sqrt((r + 1) * Pbar * Qbar) +
           qnorm(beta, lower.tail = FALSE) * sqrt(r * p1 * (1 - p1) + p2 * (1 - p2)))^2 / (r * delta^2)
  m <- mp * (1 + sqrt(1 + 2 * (r + 1) / (r * mp * delta)))^2 / 4

  return(c(ceiling(m), (r + 1) * ceiling(m)))
}

