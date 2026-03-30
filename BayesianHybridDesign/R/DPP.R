#' Bayesian Hybrid Design using Dynamic Power Prior Framework
#'
#' This function calculates the sample size and Bayesian hybrid design parameters
#' using dynamic power prior framework.
#'
#' @param pt A scalar. Response rate for experimental arm in current study.
#' @param pc A scalar. Response rate for control arm in current study.
#' @param pch A scalar. Response rate for control treatment in historical study.
#' @param pc.calib A scalar. Response rate for control arm in current study for
#'   calibration. Usually, pc.calib = pch.
#' @param nch A scalar. Total number of patients in historical control.
#' @param nc.range A vector with length = 2. Search range for nc. Default is
#'   NULL, and the range will be automatically determined.
#' @param r A scalar. Randomization ratio for current study. r=1 means 1:1 and
#'   r=2 means 2:1.
#' @param q A scalar. Specification of n_che in terms of times of nc; i.e.
#'   n_che = q*nc. Usually, q >= 1 and q <= nch/n_che.
#' @param alpha A scalar. One sided type I error rate.
#' @param power A scalar. Power. Default 0.8.
#' @param delta_threshold Borrow when abs(pc_hat (current study) - pch) <=
#'   delta_threshold. Default 0.1.
#' @param method A string characters. Method for dynamic borrowing, "Empirical
#'   Bayes", "Bayesian p", "Generalized BC", "JSD". Default "Empirical Bayes".
#' @param theta A scalar parameter with a range of (0, 1), and applicable to
#'   method: "Generalized BC". Default 0.5.
#' @param eta A scalar parameter with a range of (0, infty), and applicable to
#'   method: "Bayesian p", "Generalized BC", "JSD". "Generalized BC" method
#'   requires two parameters theta and eta. Default 1.
#' @param a0c A scalar. Hyperprior for control response rate beta(a0c, b0c).
#'   Default 0.001.
#' @param b0c A scalar. Hyperprior for control response rate beta(a0c, b0c).
#'   Default 0.001.
#' @param a0t A scalar. Hyperprior for experimental response rate beta(a0t, b0t).
#'   Default 0.001.
#' @param b0t A scalar. Hyperprior for experimental response rate beta(a0t, b0t).
#'   Default 0.001.
#' @param nsim A scalar. Number of replications to calculate power. Default
#'   100,000.
#' @param seed A scalar. seed for simulations. Default NULL.
#'
#' @return An object with values
#' \itemize{
#'   \item alpha: nominal type I error rate
#'   \item power: The calculated power by simulation.
#'   \item type1err: empirical type I error rate.
#'   \item tau: The calibrated threshold for statistical significance.
#'   \item nt:  sample size for experimental arm
#'   \item nc:  sample size for control arm
#'   \item nche: maximum amount of borrowing in terms of number of subjects
#'   \item delta.bound:  significance boundary of delta between the study
#'     experimental group and study control group
#'   \item mean.PMD: mean of posterior mean difference over nsim estimates
#'   \item sd.PMD: standard deviation of posterior mean difference over nsim
#'     estimates
#'   \item mean_pc_hca: a vector of nsim length. Storing the posterior means of
#'     pc based on hybrid control for nsim replications.
#'   \item mean_pc_c: a vector of nsim length. Storing the posterior means of pc
#'     based on study control for nsim replications.
#' }
#'
#' @examples
#' \donttest{
#' o <- DPP(pt = 0.5, pc = 0.3, pch = 0.3, pc.calib = 0.3, nch = 200, nc.range = NULL,
#'          r = 2, q = 1, alpha = 0.1, power = 0.8,
#'          delta_threshold = 0.1,
#'          method = "Empirical Bayes", theta = 0.5, eta = 1,
#'          a0c = 0.001, b0c = 0.001, a0t = 0.001, b0t = 0.001,
#'          nsim = 1000, seed = 2000)
#' print(o)
#' }
#' @export
#'
DPP <- function(pt, pc, pch, pc.calib, nch, nc.range = NULL,
                r, q, alpha = 0.1, power = 0.8,
                delta_threshold = 0.1,
                method = "Empirical Bayes", theta = 0.5, eta = 1,
                a0c = 0.001, b0c = 0.001, a0t = 0.001, b0t = 0.001,
                nsim = 100000, seed = NULL) {

  # Range of search
  if (is.null(nc.range)) {
    # Use two-sample proportion test's with 50% power sample size for nc.min
    # because the typical power for a hybrid design is 80%.
    min.nc <- Two.Prop.Test.Sample.Size(p1 = pc, p2 = pt, alpha = alpha, beta = 0.5, r = r)[1]
    # Use two-sample proportion test's with the required power sample size for nc.max
    # because after borrowing, the sample size for a hybrid design cannot exceed
    # the sample size without borrowing.
    max.nc <- Two.Prop.Test.Sample.Size(p1 = pc, p2 = pt, alpha = alpha, beta = 1 - power, r = r)[1]
  } else {
    min.nc <- nc.range[1]
    max.nc <- nc.range[2]
  }
  seq.nc <- min.nc:max.nc
  L <- length(min.nc:max.nc)

  for (i in 1:L) {
    # (1)Sample size
    nc <- seq.nc[i]
    nt <- round(nc * r)
    nche <- round(nc * q)

    # (2)Calibration and Power calculation
    o <- power.DPP(pt = pt, nt = nt, pc = pc, nc = nc, pc.calib = pc.calib,
                   pch = pch, nche = nche, nch = nch,
                   alpha = alpha, tau = NULL,
                   a0c = a0c, b0c = b0c, a0t = a0t, b0t = b0t,
                   delta_threshold = delta_threshold, method = method,
                   theta = theta, eta = eta, nsim = nsim, seed = seed)
    tau <- o$tau
    pow <- o$power
    delta.bound <- o$delta.bound
    mean.PMD <- o$pc.PMD
    sd.PMD <- o$pc.sd.PMD

    mean_pc_hca <- o$mean_hca
    mean_pc_c <- o$mean_c

    # (3) EXIT
    if (pow >= power) {
      # after found sample size, calculate type I error
      o <- power.DPP(pt = pc, nt = nt, pc = pc, nc = nc, pc.calib = pc.calib,
                     pch = pch, nche = nche, nch = nch,
                     alpha = alpha, tau = tau,
                     a0c = a0c, b0c = b0c, a0t = a0t, b0t = b0t,
                     delta_threshold = delta_threshold, method = method,
                     theta = theta, eta = eta, nsim = nsim, seed = seed)
      type1err <- o$power
      ans <- list(alpha = alpha, power = pow, type1err = type1err,
                  tau = tau, nt = nt, nc = nc, nche = nche,
                  delta.bound = delta.bound, mean.PMD = mean.PMD,
                  sd.PMD = sd.PMD, mean_pc_hca = mean_pc_hca,
                  mean_pc_c = mean_pc_c)
      break
    } else {
      ## GEMINI'S NOTE: Returning a string on failure makes the function
      ## "type-unstable", which is not a good practice. The function should
      ## return a consistent data type (e.g., a list or NULL).
      ## See revision instruction #2 below.
      if (i == L) {
        ans <- "No solution found in the provided range of nc"
      }
    }
  }
  return(ans)
}

