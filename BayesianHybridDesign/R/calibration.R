#' Calibration for Bayesian Hybrid Design
#'
#' This function calculates the threshold tau for calibration of Bayesian Hybrid
#' Design.
#' P(pt>pc|hybrid data)>tau is used to determine statistical significance.
#'
#' @param nt A scalar number of patients in experimental arm in current study
#' @param nc A scalar number of patients in control arm in current study
#' @param nche A scalar representing the equivalent number of patients borrowed
#'   from historical study
#' @param nch A scalar for total number of patients in historical control
#' @param alpha A scalar. One sided type I error rate.
#' @param pc.calib A scalar. Response rate for control arm in current study for
#'   calibration. Usually, pc.calib = pch.
#' @param pch A scalar. Response rate for control treatment in historical study
#' @param a0c A scalar. Hyperprior for control response rate beta(a0c, b0c)
#' @param b0c A scalar. Hyperprior for control response rate beta(a0c, b0c)
#' @param a0t A scalar. Hyperprior for experimental response rate beta(a0t, b0t)
#' @param b0t A scalar. Hyperprior for experimental response rate beta(a0t, b0t)
#' @param delta_threshold A scale threshold parameter. Only if abs(pc (current
#'   study) - pch) <= delta_threshold, we borrow from historical control. Default
#'   0.1.
#' @param method A string characters. Method for dynamic borrowing, "Empirical
#'   Bayes", "Bayesian p", "Generalized BC", "JSD"
#' @param theta A scalar parameter with a range of (0, 1), and applicable to
#'   "Generalized BC". Default 0.5.
#' @param eta A scalar parameter with a range of (0, infty), and applicable to
#'   methods "Bayesian p", "Generalized BC", "JSD". Default 1.
#' @param datamat A matrix with dimension nsim * 2 containing pre-simulated
#'   data for the experimental arm (column 1) and control arm (column 2),
#'   respectively. Default is NULL, and binomial random Monte Carlo samples will
#'   be generated in the function.
#' @param w0 A scale prior power parameters w. If not specified (default), w_d
#'   is calculated by the specified method for dynamic borrowing.
#' @param nsim A scalar. Number of replications to calculate power
#' @param seed A scalar. seed for simulations
#'
#' @return The scalar threshold for statistical significance that can control the
#'   type I error (1-sided)
#'
#' @examples
#' \donttest{
#'   tau <- calibration(nt=40, pc.calib=0.3, nc=40,
#'                      pch=0.3, nche=40, nch=200,
#'                      alpha = 0.10,
#'                      a0c=0.001, b0c=0.001, a0t=0.001, b0t=0.001,
#'                      delta_threshold=0.1,
#'                      method="Empirical Bayes", theta=0.5, eta=1,
#'                      nsim = 1000, seed=2000) # nsim reduced for quick example
#' }
#'
#' @export
#'
calibration <- function(nt, pc.calib, nc,
                        pch, nche, nch,
                        alpha = 0.10,
                        a0c = 0.001, b0c = 0.001, a0t = 0.001, b0t = 0.001,
                        delta_threshold = 0.1,
                        method = "Empirical Bayes", theta = 0.5, eta = 1,
                        datamat = NULL, w0 = NULL,
                        nsim = 10000, seed = NULL) {

  # under H0 for calibration
  pt <- pc <- pc.calib

  # Generate P(pt>pc|hybrid data) for each simulated trial
  if(!is.null(seed)){
    set.seed(seed)
  }
  pt_larger_pc <- rep(NA, nsim)

  Ych <- round(nch * pch)

  for (i in 1:nsim) {
    if (is.null(datamat)) {
      Yt.s <- rbinom(1, size = nt, prob = pt) # simulated Yt
      Yc.s <- rbinom(1, size = nc, prob = pc) # simulated Yc
    } else {
      Yt.s <- datamat[i, 1]
      Yc.s <- datamat[i, 2]
    }

    # Dynamic borrowing parameter
    if (is.null(w0)) {
      wt <- borrow.wt(Yc = Yc.s, nc = nc, Ych = Ych, nch = nch, nche = nche,
                      a0c = a0c, b0c = b0c, delta_threshold = delta_threshold,
                      method = method, theta = theta, eta = eta)
      w <- wt$w
    } else {
      w <- w0
    }

    # Posterior distributions
    apost_c_trial <- a0c + Yc.s
    bpost_c_trial <- b0c + (nc - Yc.s)

    apost_c_hca <- apost_c_trial + w * Ych
    bpost_c_hca <- bpost_c_trial + w * (nch - Ych)

    apost_t <- a0t + Yt.s
    bpost_t <- b0t + (nt - Yt.s)

    # calculate P(pt>pc|hybrid data)
    P_ORRt_upper_Times_p_ORRc <- function(y, ac, bc, at, bt) {
      pbeta(y, at, bt, lower.tail = FALSE) * dbeta(y, ac, bc)
    }

    pt_larger_pc[i] <- integrate(P_ORRt_upper_Times_p_ORRc,
                                 lower = 0.0001, upper = 0.9999,
                                 ac = apost_c_hca,
                                 bc = bpost_c_hca,
                                 at = apost_t,
                                 bt = bpost_t)$value
  }

  tau <- quantile(pt_larger_pc, 1 - alpha, type = 3)

  return(as.numeric(tau))
}
