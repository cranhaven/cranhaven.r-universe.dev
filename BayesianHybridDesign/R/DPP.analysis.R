#' Analysis of a Study Using Bayesian Hybrid Design using Dynamic Power Prior
#' Framework
#'
#' This function Perform Analysis for a Study Using Bayesian Hybrid Design using
#' Dynamic Power Prior Framework.
#'
#' @param Yt A scalar. Response rate for experimental arm in current study.
#' @param Yc A scalar. Response rate for control arm in current study.
#' @param nt A scalar. sample size for experimental arm.
#' @param nc A scalar. sample size for control arm.
#' @param Ych A scalar. Number of responders in historical control.
#' @param nch A scalar. Total number of subjects in historical control.
#' @param nche A scalar. maximum amount of borrowing in terms of equivalent
#'   number of subjects.
#' @param a0c A scalar. Hyperprior for control response rate beta(a0c, b0c).
#'   Default 0.001.
#' @param b0c A scalar. Hyperprior for control response rate beta(a0c, b0c).
#'   Default 0.001.
#' @param a0t A scalar. Hyperprior for experimental response rate beta(a0t, b0t).
#'   Default 0.001.
#' @param b0t A scalar. Hyperprior for experimental response rate beta(a0t, b0t).
#'   Default 0.001.
#' @param delta_threshold Borrow when abs(pc_hat (current study) - pch) <=
#'   delta_threshold#'. Default 0.1.
#' @param method A string characters. Method for dynamic borrowing, "Empirical
#'   Bayes", "Bayesian p", "Generalized BC", "JSD". Default "Empirical Bayes".
#' @param theta A scalar parameter with a range of (0, 1), and applicable to
#'   method: "Generalized BC". Default 0.5.
#' @param eta A scalar parameter with a range of (0, infty), and applicable to
#'   method: "Bayesian p", "Generalized BC", "JSD". "Generalized BC" method
#'   requires two parameters theta and eta. Default 1.
#'
#' @return An object of class `list` with values:
#' \itemize{
#'   \item w: Borrowing weight.
#'   \item phat_pt_larger_pc: Posterior probability P(ORR_trt > ORR_ctrl | data).
#'   \item apost_c_trial, bpost_c_trial: Parameters for the posterior Beta
#'     distribution of the concurrent control arm response rate.
#'   \item apost_c_hca, bpost_c_hca: Parameters for the posterior Beta
#'     distribution of the hybrid control arm response rate.
#'   \item apost_t, bpost_t: Parameters for the posterior Beta
#'     distribution of the experimental arm response rate.
#'   \item m.t: Posterior median response rate for the experimental arm.
#'   \item m.c: Posterior median response rate for the concurrent control arm.
#'   \item m.hca: Posterior median response rate for the hybrid control arm.
#' }
#'
#' @examples
#' \donttest{
#' o <- DPP.analysis(Yt=39, nt=60, Yc=13, nc=30, Ych=90, nch=200, nche = 30,
#' a0c= 0.001, b0c= 0.001, a0t= 0.001, b0t= 0.001,
#' delta_threshold = 0.1, method = "Empirical Bayes",
#' theta = 0.5, eta = 1)
#' print(o)
#' }
#'
#' @export
#'
DPP.analysis <- function(Yt = 39, nt = 60, Yc = 13, nc = 30,
                         Ych = 90, nch = 200, nche = 30,
                         a0c = 0.001, b0c = 0.001,
                         a0t = 0.001, b0t = 0.001,
                         delta_threshold = 0.1, method = "Empirical Bayes",
                         theta = 0.5, eta = 1) {
  w <- borrow.wt(Yc = Yc, nc = nc, Ych = Ych,
                 nch = nch, nche = nche, a0c = a0c, b0c = b0c,
                 delta_threshold = delta_threshold, method = method,
                 theta = theta, eta = eta)$w

  apost_c_trial <- a0c + Yc
  bpost_c_trial <- b0c + (nc - Yc)
  apost_c_hca <- apost_c_trial + w * Ych
  bpost_c_hca <- bpost_c_trial + w * (nch - Ych)
  apost_t <- a0t + Yt
  bpost_t <- b0t + (nt - Yt)
  
  P_ORRt_upper_Times_p_ORRc <- function(y, ac, bc, at, bt) {
    pbeta(y, at, bt, lower.tail = FALSE) * dbeta(y, ac, bc)
  }
  
  phat_pt_larger_pc <- integrate(P_ORRt_upper_Times_p_ORRc,
                                 lower = 1e-04, upper = 0.9999, ac = apost_c_hca,
                                 bc = bpost_c_hca, at = apost_t, bt = bpost_t)$value

  o <- list()
  o$w <- w
  o$phat_pt_larger_pc <- phat_pt_larger_pc
  o$apost_c_trial <- apost_c_trial
  o$bpost_c_trial <- bpost_c_trial
  o$apost_c_hca <- apost_c_hca
  o$bpost_c_hca <- bpost_c_hca
  o$apost_t <- apost_t
  o$bpost_t <- bpost_t
  o$m.t <- qbeta(0.5, apost_t, bpost_t)
  o$m.c <- qbeta(0.5, apost_c_trial, bpost_c_trial)
  o$m.hca <- qbeta(0.5, apost_c_hca, bpost_c_hca)

  return(o)
}


