#' Bayesian Hybrid Design
#'
#' This function calculates the expected effective sample size of the DPP.
#'
#' @param pc A scalar. Response rate for control arm in current study.
#' @param nc A scalar. Number of patients in control arm in current study.
#' @param pch A scalar. Response rate for control treatment in historical study.
#' @param nche A scalar. Equivalent number of patients borrowed from historical
#'   study.
#' @param nch A scalar. Total number of patients in historical control.
#' @param a0c A scalar. Hyperprior for control response rate beta(a0c, b0c).
#' @param b0c A scalar. Hyperprior for control response rate beta(a0c, b0c).
#' @param delta_threshold A scalar. Borrow when abs(pc_hat (current study) -
#'   pch) <= delta_threshold.
#' @param method A string characters. Method for dynamic borrowing, "Empirical
#'   Bayes", "Bayesian p", "Generalized BC", "JSD". Default "Empirical Bayes".
#' @param theta A scalar parameter with a range of (0, 1), and applicable to
#'   method: "Generalized BC". Default 0.5.
#' @param eta A scalar parameter with a range of (0, infty), and applicable to
#'   method: "Bayesian p", "Generalized BC", "JSD". "Generalized BC" method
#'   requires two parameters theta and eta. Default 1.
#'
#' @return The expected effective sample size.
#'
#' @examples
#' EESS(pc=0.3,nc=40,pch=0.3,nche=40,nch=180, a0c=0.001,b0c=0.001,
#' delta_threshold=0.1,method="Empirical Bayes", theta=0.5, eta=1)
#'
#' @export
#'
EESS <- function(pc,
                 nc, pch, nche, nch,
                 a0c = 0.001, b0c = 0.001,
                 delta_threshold = 0.1,
                 method = "Empirical Bayes", theta = 0.5, eta = 1) {

  Yc <- 0:nc
  pYc <- dbinom(Yc, size = nc, prob = pc)
  w <- rep(NA, nc + 1)

  for (i in seq_along(Yc)) {
    w[i] <- borrow.wt(Yc = i - 1, nc = nc, Ych = round(nch * pch), nch = nch, nche = nche,
                      a0c = a0c, b0c = b0c, delta_threshold = delta_threshold,
                      method = method, theta = theta, eta = eta)$w
  }

  return(nch * sum(w * pYc))
}
