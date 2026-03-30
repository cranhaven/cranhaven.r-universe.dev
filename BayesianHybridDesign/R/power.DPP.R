#' Power Calculation for Bayesian Hybrid Design
#'
#' Calculates statistical power and other design parameters for a Bayesian
#' Hybrid Design using a dynamic power prior approach, based on simulations.
#'
#' @param pt,pc Numerics. The response rates for the experimental and control
#'   arms, respectively.
#' @param nt,nc Integers. The sample sizes for the experimental and control
#'   arms, respectively.
#' @param pc.calib A scalar numeric. The control response rate assumed for
#'   calibrating the type I error threshold, `tau`. Often `pc.calib = pch`.
#' @param pch,nch Numeric and integer. The response rate and sample size of the
#'   historical control arm.
#' @param nche An integer. The effective number of subjects to be borrowed,
#'   used for calculating the global borrowing weight.
#' @param alpha A scalar numeric. The one-sided Type I error rate, used for
#'   calibration if `tau` is not provided.
#' @param tau (Optional) A scalar numeric. The pre-calibrated threshold for
#'   statistical significance. If `NULL`, it will be calculated internally.
#' @param a0c,b0c,a0t,b0t Numerics. Hyperparameters for the Beta priors on the
#'   response rates.
#' @param delta_threshold A scalar numeric. The similarity threshold for borrowing.
#' @param method A string specifying the dynamic borrowing method.
#' @param theta,eta Numerics. Additional parameters for certain borrowing methods.
#' @param datamat (Optional) A matrix with `nsim` rows and 2 columns
#'   (experimental, control) of pre-simulated response counts.
#' @param w0 (Optional) A scalar numeric. A fixed borrowing weight to override
#'   the dynamic calculation.
#' @param nsim An integer. The number of simulations to run.
#' @param seed An integer. A seed for the random number generator. Default NULL.
#'
#' @return A large list containing the power, the calibrated `tau`, all input
#'   parameters, and detailed simulation results such as:
#' \describe{
#'   \item{power}{The calculated statistical power.}
#'   \item{tau}{The calibrated significance threshold.}
#'   \item{pc.PMD, pc.sd.PMD}{The mean and standard deviation of the posterior
#'     mean difference between the hybrid and concurrent controls.}
#'   \item{delta.bound}{The minimum detectable difference in response rates.}
#'   \item{phat_pt_larger_pc_all}{A vector of posterior probabilities
#'     `P(pt > pc | data)` for each of the `nsim` simulations.}
#'   \item{mean_hca, mean_c}{Vectors of the posterior means for the hybrid
#'     and concurrent control arms for each simulation.}
#'   \item{simulated.data}{A matrix of the simulated response counts used.}
#'   \item{w}{A vector of the final borrowing weights used in each simulation.}
#'   \item{...}{and all input parameters.}
#' }
#'
#' @examples
#' \donttest{
#' o <- power.DPP(pt=0.5, nt=40, pc=0.3, nc=40, pc.calib = 0.3, pch=0.3,
#'                nche=40, nch=180, alpha=0.1, nsim = 1000, seed=2000) # nsim is reduced
#' }
#' @export
#'
power.DPP <- function(pt, nt, pc, nc, pc.calib,
                      pch, nche, nch,
                      alpha = 0.1, tau = NULL,
                      a0c = 0.001, b0c = 0.001, a0t = 0.001, b0t = 0.001,
                      delta_threshold = 0.1,
                      method = "Empirical Bayes", theta = 0.5, eta = 1,
                      datamat = NULL, w0 = NULL,
                      nsim = 100000, seed = NULL) {
  # Threshold of significance tau for P(pt>pc|hybrid data) > tau
  if (is.null(tau)) {
    tau <- calibration(nt = nt, pc.calib = pc.calib, nc = nc, pch = pch,
                       nche = nche, nch = nch,
                       alpha = alpha, a0c = a0c, b0c = b0c, a0t = a0t, b0t = b0t,
                       delta_threshold = delta_threshold, method = method,
                       theta = theta, eta = eta, nsim = nsim, seed = seed)
  }

  # Define helper function once, outside of loops
  P_ORRt_upper_Times_p_ORRc <- function(y, ac, bc, at, bt) {
    pbeta(y, at, bt, lower.tail = FALSE) * dbeta(y, ac, bc)
  }

  Yc <- round(nc * pc)
  Ych <- round(nch * pch)

  # Dynamic borrowing parameter
  if (is.null(w0)) {
    wt <- borrow.wt(Yc = Yc, nc = nc, Ych = Ych, nch = nch, nche = nche,
                    a0c = a0c, b0c = b0c, delta_threshold = delta_threshold,
                    method = method, theta = theta, eta = eta)
    w <- wt$w
  } else {
    w <- w0
  }

  Yt <- 0:nt # All scenarios for number of responders in exp arm

  # Posterior distributions
  apost_c_trial <- a0c + Yc
  bpost_c_trial <- b0c + (nc - Yc)
  apost_c_hca <- apost_c_trial + w * Ych
  bpost_c_hca <- bpost_c_trial + w * (nch - Ych)
  apost_t <- a0t + Yt
  bpost_t <- b0t + (nt - Yt)

  # Boundary number of responders for significance
  phat_pt_larger_pc <- rep(NA, nt + 1)

  for (i in 1:length(Yt)) {
    phat_pt_larger_pc[i] <- integrate(P_ORRt_upper_Times_p_ORRc,
                                      lower = 0.0001, upper = 0.9999,
                                      ac = apost_c_hca, bc = bpost_c_hca,
                                      at = apost_t[i], bt = bpost_t[i])$value
  }

  # min detectable response difference: delta.bound
  delta.bound <- NA
  BoundaryIdx <- which((phat_pt_larger_pc >= tau))
  if (length(BoundaryIdx)) {
    delta.bound <- (BoundaryIdx[1] - 1) / nt - pc
  }

  ######################
  # Power calculation
  ######################
  if (!is.null(seed)) {
    set.seed(seed)
  }

  success <- 0
  mean_hca <- mean_c <- rep(NA, nsim)
  phat_pt_larger_pc_all <- rep(NA, nsim)
  datamat2 <- matrix(NA, nsim, 2)
  wvec <- numeric(nsim)

  # for each simulated trial, determine whether significant
  for (i in 1:nsim) {
    if (is.null(datamat)) {
      Yt.s <- rbinom(1, size = nt, prob = pt)
      Yc.s <- rbinom(1, size = nc, prob = pc)
    } else {
      Yt.s <- datamat[i, 1]
      Yc.s <- datamat[i, 2]
    }
    datamat2[i,] <- c(Yt.s, Yc.s)

    # Dynamic borrowing parameter
    if (is.null(w0)) {
      # Use round() here as well for robustness
      wt <- borrow.wt(Yc = Yc.s, nc = nc, Ych = round(nch * pch), nch = nch, nche = nche,
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

    # calculate the probability
    phat_pt_larger_pc <- integrate(P_ORRt_upper_Times_p_ORRc,
                                   lower = 0.0001, upper = 0.9999,
                                   ac = apost_c_hca,
                                   bc = bpost_c_hca,
                                   at = apost_t,
                                   bt = bpost_t)$value

    phat_pt_larger_pc_all[i] <- phat_pt_larger_pc
    success <- success + (phat_pt_larger_pc > tau)
    mean_hca[i] <- apost_c_hca / (apost_c_hca + bpost_c_hca)
    mean_c[i] <- apost_c_trial / (apost_c_trial + bpost_c_trial)
    wvec[i] <- w
  }

  power <- as.numeric(success) / nsim

  pc.PMD <- mean(mean_hca - mean_c)
  pc.sd.PMD <- sd(mean_hca - mean_c)

  return(list(power = power, tau = tau, alpha = alpha, pc.calib = pc.calib,
              nt = nt, nc = nc, nche = nche, pt = pt, pc = pc, nch = nch, pch = pch,
              method = method, theta = theta, eta = eta,
              delta_threshold = delta_threshold,
              pc.PMD = pc.PMD, pc.sd.PMD = pc.sd.PMD,
              delta.bound = delta.bound,
              phat_pt_larger_pc_all = phat_pt_larger_pc_all,
              mean_hca = mean_hca, mean_c = mean_c,
              simulated.data = datamat2,
              nsim = nsim, seed = seed,
              w = wvec))
}
