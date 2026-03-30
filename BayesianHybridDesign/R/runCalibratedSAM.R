#' @title Run a Calibrated Simulation Using SAM Priors
#' @md
#'
#' @description
#' Performs a two-step simulation.
#' 1.  **Calibration Step:** It first runs a simulation under the null hypothesis
#'     (using `*.calib` parameters) to determine the posterior probability
#'     thresholds (quantiles) needed to control the Type I Error Rate at the
#'     specified `typeIER.cal` level.
#' 2.  **Main Simulation Step:** It then uses these calibrated thresholds to run
#'     the main simulation (using parameters `nc`, `nt`, `pc`, `pt`) to evaluate
#'     final operating characteristics (e.g., Type I Error or Power) and bias.
#'
#' @details
#' This function wraps the core [runSAM()] function by adding a calibration layer.
#' It derives historical data priors using [RBesT::gMAP()] and [RBesT::automixfit()]
#' based on `nche.c` and `pch`.
#'
#' The calibration step (`runSAM` with `...calib` parameters) finds the
#' `(1 - typeIER.cal)` quantiles for the posterior distributions (SAM, rMAP, Non-info)
#' under the null.
#'
#' The main simulation step (`runSAM` with main parameters) then calculates the
#' proportion of simulations where the posterior probability exceeds these
#' calibrated thresholds. This proportion represents the final Type I Error or Power.
#'
#' @param nsim The total number of simulation trials to run for both the
#'   calibration and main simulation steps.
#' @param pch Historical control response rate (used to generate `HistData`
#'   for the [RBesT::gMAP()] prior).
#' @param delta_threshold The CSD (Clinically Significant Difference) threshold
#'   used for the SAM prior in [runSAM()].
#' @param nche.c Sample size for the historical control data (`HistData`).
#' @param nc.calib Control group sample size for the **calibration** step.
#' @param nt.calib Treatment group sample size for the **calibration** step.
#' @param pc.calib Control response rate for the **calibration** step. This is
#'   also used as the treatment response rate in this step to simulate the null hypothesis.
#' @param xt.cal (Optional) A vector of pre-simulated treatment outcomes for the
#'   **calibration** step. If `NULL`, data is simulated internally.
#' @param xc.cal (Optional) A vector of pre-simulated control outcomes for the
#'   **calibration** step. If `NULL`, data is simulated internally.
#' @param typeIER.cal The target Type I Error rate to control for during the
#'   calibration step. Defaults to 0.1.
#' @param nche Historical control sample size. (Note: This parameter is defined
#'   in the function signature but not explicitly used in the function body;
#'   `nche.c` is used for calibration data generation.)
#' @param nc Control group sample size for the **main simulation** step.
#' @param nt Treatment group sample size for the **main simulation** step.
#' @param pc Control response rate for the **main simulation** step.
#' @param pt Treatment response rate for the **main simulation** step. (Note:
#'   Set `pt = pc` for Type I Error, or `pt > pc` for Power).
#' @param xt (Optional) A vector of pre-simulated treatment outcomes for the
#'   **main simulation**. If `NULL`, data is simulated internally.
#' @param xc (Optional) A vector of pre-simulated control outcomes for the
#'   **main simulation**. Examples use [RBesT::mixbeta()]. If `NULL`, data is simulated internally.
#' @param nf.prior The non-informative prior to use. Defaults to
#'   `RBesT::mixbeta(c(1, 0.001, 0.001))`.
#' @param seed.hist Seed for generating historical data.
#' @param seed.gMAP Seed for the [RBesT::gMAP()] function.
#' @param seed.SAM Seed for the main `runSAM` simulation call.
#' @param seed.cal Seed for the calibration `runSAM` call.
#'
#' @return A list with the following components:
#'   \describe{
#'     \item{Sim_Result}{A numeric vector (length 3) with the final simulation
#'       result (Type I Error or Power) for the "SAMprior", "MAP", and
#'       "Noninfo" methods.}
#'     \item{pc.PMD}{Posterior Mean Difference (Bias) for the three methods.}
#'     \item{pc.PSDD}{Posterior SD of the Difference (Bias) for the three methods.}
#'     \item{pc.PM}{A matrix (`3 x nsim`) of posterior means for the control
#'       rate for each simulation iteration.}
#'     \item{Calibration_Thresholds}{A matrix (1x3) containing the
#'       decision thresholds (quantiles) determined during the calibration step
#'       for the SAM, rMAP, and Non-info methods.}
#'   }
#'
#' @seealso \code{\link{runSAM}}, \code{\link[RBesT]{gMAP}},
#'   \code{\link[RBesT]{automixfit}}, \code{\link[RBesT]{mixbeta}},
#'   \code{\link[RBesT]{decision2S}}
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Example uses functions from the RBesT package
#' library(RBesT)
#'
#' sim_power_pc30 <- runCalibratedSAM(
#'    nsim = 100,
#'    pch = 0.30,
#'    delta_threshold = 0.1,
#'    nche.c = 180,
#'    nc.calib = 45,
#'    nt.calib = 45,
#'    pc.calib = 0.3,
#'    typeIER.cal = 0.10,
#'    nche = 180,
#'    nc = 45,
#'    nt = 45,
#'    pc = 0.30,
#'    pt = 0.50
#' )
#'
#' print(sim_power_pc30$Sim_Result)
#' }
#'
runCalibratedSAM <- function(
    nsim = 1000,
    pch,
    delta_threshold,
    nche.c,
    nc.calib,
    nt.calib,
    pc.calib,
    xt.cal = NULL,
    xc.cal = NULL,
    typeIER.cal = 0.1,
    nche,
    nc,
    nt,
    pc,
    pt,
    xt = NULL,
    xc = NULL,
    nf.prior = RBesT::mixbeta(c(1, 0.001, 0.001)),
    seed.hist = 1000,
    seed.gMAP = 2000,
    seed.SAM = 3000,
    seed.cal = 4000
) {

  message("Starting Step 1: Calibration...")

  set.seed(seed.hist)
  HistData <- data.frame(study = "TL01", r = round(nche.c * pch), n = nche.c)

  set.seed(seed.gMAP)
  # Ensure RBesT functions are called correctly
  map_HistData <- RBesT::gMAP(cbind(r, n - r) ~ 1 | study,
                              family = binomial,
                              data = HistData,
                              tau.dist = "HalfNormal",
                              tau.prior = 1,
                              beta.prior = 2)

  map_automix <- RBesT::automixfit(map_HistData)

  datamat_cal <- if (!is.null(xt.cal) && !is.null(xc.cal)) cbind(xt.cal, xc.cal) else NULL

  set.seed(seed.cal)
  CalRes <- runSAM(if.prior = map_automix,
                   nf.prior = nf.prior,
                   delta    = delta_threshold,
                   method.w = 'LRT',
                   n        = nc.calib,
                   n.t      = nt.calib,
                   decision = RBesT::decision2S(0.90, 0, lower.tail = FALSE),
                   ntrial   = nsim,
                   if.MAP   = TRUE,
                   weight   = 0.5,
                   theta    = pc.calib,
                   theta.t  = pc.calib,
                   datamat  = datamat_cal,
                   dist     = TRUE
  )

  sam.tau.q <- quantile(CalRes$res_SAM_dist, 1 - typeIER.cal, type = 3)
  rMAP.tau.q <- quantile(CalRes$res_rMAP_dist, 1 - typeIER.cal, type = 3)
  non.tau.q <- quantile(CalRes$res_Non_dist, 1 - typeIER.cal, type = 3)

  cal.tau <- matrix(c(sam.tau.q, rMAP.tau.q, non.tau.q), nrow = 1)
  sam.tau <- cal.tau[1, 1]
  rMAP.tau <- cal.tau[1, 2]
  non.tau <- cal.tau[1, 3]

  message("Calibration complete. Thresholds determined.")

  SAMMethodNames <- c("SAMprior", "MAP", "Noninfo")
  nm <- length(SAMMethodNames)

  Sim_Result <- rep(NA, nm)
  pc.PMD <- rep(NA, nm)
  pc.PSDD <- rep(NA, nm)
  pc.PM <- array(NA, dim = c(nm, nsim))

  message("Starting Step 3: Main Simulation...")

  datamat <- if (!is.null(xt) && !is.null(xc)) cbind(xt, xc) else NULL

  set.seed(seed.SAM)
  SAM.Res <- runSAM(if.prior = map_automix,
                    nf.prior = nf.prior,
                    delta    = delta_threshold,
                    method.w = 'LRT',
                    n        = nc,
                    n.t      = nt,
                    decision = RBesT::decision2S(0.90, 0, lower.tail = FALSE),
                    ntrial   = nsim,
                    if.MAP   = TRUE,
                    weight   = 0.5,
                    theta    = pc,
                    theta.t  = pt,
                    datamat  = datamat
  )

  Sim_Result[1] <- mean(SAM.Res$res_SAM_dist > sam.tau)
  Sim_Result[2] <- mean(SAM.Res$res_rMAP_dist > rMAP.tau)
  Sim_Result[3] <- mean(SAM.Res$res_Non_dist > non.tau)

  for (i in 1:nsim) {
    pc.PM[1, i] <- summary(SAM.Res$post_theta_c_SAM_list[[i]])["mean"]
    pc.PM[2, i] <- summary(SAM.Res$post_theta_c_MAP_list[[i]])["mean"]
    pc.PM[3, i] <- summary(SAM.Res$post_theta_c_list[[i]])["mean"]
  }

  pc.PMD[1] <- mean(pc.PM[1, ] - pc.PM[3, ])
  pc.PMD[2] <- mean(pc.PM[2, ] - pc.PM[3, ])
  pc.PMD[3] <- 0 # Reference bias is 0 by definition

  pc.PSDD[1] <- sd(pc.PM[1, ] - pc.PM[3, ])
  pc.PSDD[2] <- sd(pc.PM[2, ] - pc.PM[3, ])
  pc.PSDD[3] <- 0

  message("Main simulation complete.")
  message("Starting Step 4: Summarizing results...")

  results_list <- list(
    Sim_Result = Sim_Result,
    pc.PMD = pc.PMD,
    pc.PSDD = pc.PSDD,
    pc.PM = pc.PM,
    Calibration_Thresholds = cal.tau
  )

  return(results_list)
}
