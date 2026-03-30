#' Explore Power Across Multiple Scenarios for a Bayesian Hybrid Design
#'
#' Evaluates statistical power and other design parameters across a grid of
#' specified settings, using parallel computing for efficiency.
#'
#' @details
#' This function serves as a wrapper for `power.DPP()`. It uses `expand.grid()`
#' to create a full factorial design from the vector-based input parameters
#' (`method`, `pc`, `nc`, `pc.calib`, `q`). It then iterates through each
#' scenario to calculate the power, type I error, and other metrics.
#'
#' @param method A character vector. One or more dynamic borrowing methods to
#'   explore (e.g., `"Empirical Bayes"`, `"Bayesian p"`).
#' @param pc A numeric vector. Response rates for the current control arm to
#'   explore.
#' @param nc A numeric vector of integers. Sample sizes for the current control
#'   arm to explore.
#' @param pc.calib A numeric vector. Control arm response rates used for
#'   calibrating the type I error threshold, `tau`.
#' @param q A numeric vector. Ratios of `nche / nc` to explore.
#' @param delta A scalar numeric. The true difference to be added to `pc` to
#'   determine the experimental arm response rate (`pt`).
#' @param r A scalar numeric. The randomization ratio of the experimental arm to
#'   the control arm (`nt / nc`).
#' @param pch A scalar numeric. The response rate of the historical control arm.
#' @param nch A scalar integer. The total number of subjects in the historical
#'   control arm.
#' @param alpha A scalar numeric. The one-sided Type I error rate.
#' @param a0c,b0c Numerics. Hyperparameters for the Beta(a0c, b0c) prior on
#'   the control response rate. Defaults to 0.001.
#' @param a0t,b0t Numerics. Hyperparameters for the Beta(a0t, b0t) prior on
#'   the experimental response rate. Defaults to 0.001.
#' @param delta_threshold A scalar numeric. The similarity threshold; borrowing is
#'   only triggered if `abs(pc_hat - pch_hat) <= delta_threshold`. Default is 0.1.
#' @param theta A scalar numeric in (0, 1), applicable to the "Generalized BC"
#'   method. Default is 0.5.
#' @param eta A scalar numeric, applicable to the "Bayesian p", "Generalized
#'   BC", and "JSD" methods. Default is 1.
#' @param nsim A scalar integer. The number of Monte Carlo simulations for each
#'   scenario.
#' @param seed A scalar integer. A seed for the random number generator to ensure
#'   reproducibility. Default is NULL.
#' @param ncore An integer. The number of CPU cores for parallel processing. If
#'   `NULL` (the default), it uses one less than the number of detected cores.
#'
#' @return A `data.frame` where each row corresponds to one scenario from the
#'   input grid. The columns include the input parameters and the following
#'   results:
#' \describe{
#'   \item{method}{The dynamic borrowing method used.}
#'   \item{nt, nc, nche}{Sample sizes for the experimental, current control, and
#'     effective historical control arms.}
#'   \item{pt, pc, pc.calib, pch}{Response rates used in the scenario.}
#'   \item{q}{The ratio of `nche / nc`.}
#'   \item{tau}{The calibrated threshold for statistical significance.}
#'   \item{type1err}{The simulated Type I error rate for the scenario.}
#'   \item{power}{The simulated statistical power for the scenario.}
#'   \item{delta.boundary}{The significance boundary for the difference between
#'     groups.}
#'   \item{mean.PMD}{The mean of the posterior mean difference over simulations.}
#'   \item{sd.PMD}{The standard deviation of the posterior mean difference.}
#' }
#'
#' @seealso \code{\link{power.DPP}} for the underlying single-scenario calculation.
#'
#'
#' @examples
#' \donttest{
#' # Run with 2 cores as an example; on CRAN, examples should use <= 2 cores.
#' Res1 <- explore.power.DPP(method=c("Empirical Bayes"),
#'                           pc=c(0.27, 0.37),
#'                           nc=23:25,
#'                           pc.calib = 0.27,
#'                           q = c(1, 1.5),
#'                           delta = 0.2,
#'                           r = 1,
#'                           pch=0.27, nch=500,
#'                           alpha=0.1,
#'                           nsim = 200, # Reduced for a quick example
#'                           seed=1000,
#'                           ncore=2)
#' }
#'
#' @export
#'
#' @import parallel
#' @import doParallel
#' @import foreach
explore.power.DPP <- function(method,
                              pc,
                              nc,
                              pc.calib,
                              q,
                              delta,
                              r,
                              pch,
                              nch,
                              alpha = 0.1,
                              a0c = 0.001,
                              b0c = 0.001,
                              a0t = 0.001,
                              b0t = 0.001,
                              delta_threshold = 0.1,
                              theta = 0.5,
                              eta = 1,
                              nsim = 100000,
                              seed = NULL,
                              ncore = NULL) {

  if (is.null(ncore)) {
    ncore <- max(1, parallel::detectCores() - 1)
  }

  cl <- makeCluster(ncore)
  registerDoParallel(cl)
  on.exit(stopCluster(cl), add = TRUE) # This ensures cleanup

  # Generate the setting matrix, each row is one setting
  Settings <- expand.grid(method = method, pc = pc, nc = nc, pc.calib = pc.calib, q = q)

  Settings$nt <- Settings$nc * r
  Settings$nche <- round(Settings$nc * Settings$q)

  i <- NULL

  # Loop over all settgins with parallel computing
  NS <- nrow(Settings)
  Result <- foreach(i = 1:NS, .combine = rbind) %dopar% {

    # Getting setting parameters for each setting
    pci <- Settings$pc[i]
    nci <- Settings$nc[i]
    methodi <- Settings$method[i]
    nti <- Settings$nt[i]
    nchei <- Settings$nche[i]
    pc.calibi <- Settings$pc.calib[i]
    qi <- Settings$q[i]

    pti <- pci + delta

    # Get the calibrated threshold tau
    # Get power, delta boundary, mean PMD and sd(PMD)
    o <- power.DPP(pt = pti, nt = nti, pc = pci, nc = nci, pc.calib = pc.calibi,
                   pch = pch, nche = nchei, nch = nch,
                   alpha = alpha, tau = NULL,
                   a0c = a0c, b0c = b0c, a0t = a0t, b0t = b0t,
                   delta_threshold = delta_threshold, method = methodi,
                   theta = theta, eta = eta, nsim = nsim, seed = seed)
    tau <- o$tau
    pow <- o$power
    delta.bound <- o$delta.bound
    mean.PMD <- o$pc.PMD
    sd.PMD <- o$pc.sd.PMD

    mean_pc_hca <- mean(o$mean_hca)
    mean_pc_c <- mean(o$mean_c)

    # Based on the calibrated tau, calculate the type I error given pt = pc in the current setting
    o <- power.DPP(pt = pci, nt = nti, pc = pci, nc = nci, pc.calib = pc.calibi,
                   pch = pch, nche = nchei, nch = nch,
                   alpha = alpha, tau = tau,
                   a0c = a0c, b0c = b0c, a0t = a0t, b0t = b0t,
                   delta_threshold = delta_threshold, method = methodi,
                   theta = theta, eta = eta, nsim = nsim, seed = seed)
    type1err <- o$power

    # Return the setting parameters and type I error, power, delta boundary, mean PMD and sd(PMD)
    ans <- data.frame(method = methodi,
                      nt = nti,
                      nc = nci,
                      nche = nchei,
                      pt = pti,
                      pc = pci,
                      pc.calib = pc.calibi,
                      pch = pch,
                      tau = tau,
                      type1err = type1err,
                      power = pow,
                      delta.boundary = delta.bound,
                      mean.PMD = mean.PMD,
                      sd.PMD = sd.PMD,
                      q = qi)

    return(ans)
  }

  return(Result)

}

