#' Fuse svycoxph and cph objects for rms ecosystem compatibility
#'
#' Takes a fitted \code{cph} object and a fitted \code{svycoxph} object with
#' the same formula, and returns a modified \code{cph}-class object with
#' survey-correct coefficients and variance-covariance matrix while preserving
#' the \code{rms} \code{$Design} structure needed for \code{anova.rms()},
#' \code{Predict()}, \code{summary.rms()}, and related generics.
#'
#' @param fit_cph A \code{cph} object fitted with \code{x=TRUE, y=TRUE, surv=TRUE}.
#' @param fit_svy A \code{svycoxph} object fitted with the same formula and data.
#'
#' @return A modified \code{cph} object with survey-correct inference.
#'   The \code{$var} slot contains the sandwich variance-covariance matrix
#'   from \code{fit_svy}; \code{$coefficients} contains the weighted partial
#'   likelihood estimates. The \code{$Design} and all structural slots are
#'   preserved from \code{fit_cph}.
#'
#' @details
#' \code{anova.rms()} constructs Wald tests as
#' \eqn{(L\beta)^\top (LVL^\top)^{-1} (L\beta)} where \eqn{L} is a contrast
#' matrix derived from \code{$Design} and \eqn{V} is \code{$var}. Substituting
#' the survey-corrected \eqn{V} from \code{svycoxph} yields design-correct
#' Wald statistics while preserving the rms term-identification machinery.
#'
#' Degrees of freedom in \code{anova.rms()} are based on contrast matrix rank,
#' not survey PSU count. For fully correct F-test df, use
#' \code{survey::regTermTest()} directly on \code{fit_svy}.
#'
#' @references
#' Binder, D.A. (1992). Fitting Cox's proportional hazards models from survey
#' data. \emph{Biometrika}, 79(1), 139--147.
#'
#' Lin, D.Y. (2000). On fitting Cox's proportional hazards models to survey
#' data. \emph{Biometrika}, 87(1), 37--47.
#'
#' @seealso \code{\link{weighted_basehaz}}, \code{\link{svycph_set_basehaz}}
#' @importFrom stats coef vcov
#' @export
svycph_fuse <- function(fit_cph, fit_svy) {
  # Validate inputs
  if (!inherits(fit_cph, "cph"))
    stop("fit_cph must be a cph object")
  if (!inherits(fit_svy, "svycoxph"))
    stop("fit_svy must be a svycoxph object")

  n_cph <- length(coef(fit_cph))
  n_svy <- length(coef(fit_svy))
  if (n_cph != n_svy)
    stop("Number of coefficients differs (", n_cph, " vs ", n_svy,
         ") -- ensure both models use the same formula")

  fused <- fit_cph

  # Substitute survey-weighted values; preserve cph coefficient names.
  # rms shortens rcs(X,k)X -> X and log(X) -> X, while svycoxph keeps the
  # full term names -- positional correspondence is guaranteed by same formula.
  coef_svy        <- coef(fit_svy)
  names(coef_svy) <- names(coef(fit_cph))
  vcov_svy        <- vcov(fit_svy)
  dimnames(vcov_svy) <- dimnames(vcov(fit_cph))

  fused$coefficients <- coef_svy
  fused$var          <- vcov_svy

  # Survey df is already computed by svycoxph (n_PSU - n_strata)
  fused$svycph_fused   <- TRUE
  fused$svycph_vcov_df <- fit_svy$degf.resid

  class(fused) <- c("svycph_fused", class(fit_cph))
  fused
}


#' Compute survey-weighted cumulative baseline hazard with variance
#'
#' Implements the weighted Breslow estimator and its linearization-based
#' variance following Lin (2000). The point estimate weights each event's
#' contribution by its survey weight; the variance uses the influence function
#' of the weighted estimator combined with the survey design's stratified
#' cluster structure.
#'
#' @param fit_svy A fitted \code{svycoxph} object.
#' @param design The \code{svydesign} object used to fit \code{fit_svy}.
#' @param centered Logical. If \code{TRUE} (default), the baseline hazard
#'   corresponds to a person at the weighted mean of each covariate, matching
#'   the \code{centered=TRUE} default of \code{survival::basehaz()}.
#' @param se_type Character. Which variance estimator to use for \code{std.err}:
#'   \describe{
#'     \item{\code{"lin"}}{(default) Lin (2000) design-based linearization
#'       variance. Measures sensitivity to PSU selection; appropriate for
#'       population-level design inference. Produces very small SEs for
#'       large-population surveys like NHANES.}
#'     \item{\code{"greenwood"}}{Survey-weighted Greenwood formula:
#'       \eqn{\sum_{t_k \leq t} n^w(t_k) / [Y^w(t_k)]^2}. Measures
#'       statistical precision from the weighted event count; gives confidence
#'       bands of interpretable width for \code{survplot()}.}
#'   }
#'
#' @return A data frame with columns:
#'   \item{time}{Event times.}
#'   \item{hazard}{Weighted cumulative baseline hazard H_0(t).}
#'   \item{surv}{Baseline survival exp(-H_0(t)).}
#'   \item{se_H0}{Standard error of H_0(t) on the hazard scale.}
#'   \item{std.err}{Standard error of log(H_0(t)), for direct substitution
#'     into the \code{$std.err} slot of a fused \code{cph} object.
#'     Computed from \code{se_H0} via the delta method: SE(log H) = SE(H)/H.}
#'
#' @section Choosing \code{se_type}:
#' For NHANES-scale populations the Lin design variance is orders of magnitude
#' smaller than the Greenwood-weighted variance because the former captures only
#' PSU-selection uncertainty (very small for rare events), while the latter
#' captures statistical uncertainty from the weighted event count. The ratio
#' is approximately proportional to the square root of the mean survey weight.
#' Use \code{se_type = "greenwood"} when the goal is \code{survplot()} confidence
#' bands that convey statistical reliability; use \code{"lin"} when the goal is
#' design-consistent variance for formal population inference.
#'
#' @details
#' The weighted Breslow increment at each event time \eqn{t_k} is:
#' \deqn{d\hat{H}_0^w(t_k) = \frac{\sum_{i: t_i = t_k, \delta_i=1} w_i}
#'   {\sum_{j \in \mathcal{R}(t_k)} w_j \exp(\mathbf{X}_j^\top \hat{\beta})}}
#'
#' The influence function of \eqn{d\hat{H}_0^w(t_k)} for observation \eqn{i}
#' is (Lin 2000, eq. 2.3):
#' \deqn{\phi_i(t_k) = \frac{I(t_i = t_k,\, \delta_i = 1)}{Y^w(t_k)} -
#'   \frac{n^w(t_k)}{[Y^w(t_k)]^2}\, I(t_i \geq t_k)\, \exp(\mathbf{X}_i^\top\hat{\beta})}
#'
#' where \eqn{Y^w(t_k) = \sum_{j \in \mathcal{R}(t_k)} w_j \exp(\mathbf{X}_j^\top\hat{\beta})}
#' and \eqn{n^w(t_k) = \sum_{i: t_i=t_k, \delta_i=1} w_i}.
#'
#' The cumulative influence \eqn{\Phi_i(t) = \sum_{t_k \leq t} \phi_i(t_k)}
#' is used to construct the linearization variance estimate (Lin 2000, eq. 2.4):
#' \deqn{\widehat{\mathrm{Var}}(\hat{H}_0^w(t)) =
#'   \sum_h \frac{n_h}{n_h - 1}
#'   \sum_{\alpha \in h} \left(e_{h\alpha}(t) - \bar{e}_h(t)\right)^2}
#'
#' where \eqn{e_{h\alpha}(t) = \sum_{i \in \text{PSU}\,\alpha} \Phi_i(t)} is
#' the PSU-level total of influence functions within stratum \eqn{h}.
#'
#' Note: this variance conditions on \eqn{\hat{\beta}} and does not propagate
#' uncertainty from coefficient estimation. For large samples this contribution
#' is negligible relative to the design variance.
#'
#' @references
#' Lin, D.Y. (2000). On fitting Cox's proportional hazards models to survey
#' data. \emph{Biometrika}, 87(1), 37--47.
#'
#' @seealso \code{\link{svycph_fuse}}, \code{\link{svycph_set_basehaz}}
#' @importFrom stats weights weighted.mean
#' @export
weighted_basehaz <- function(fit_svy, design, centered = TRUE,
                             se_type = c("lin", "greenwood")) {
  se_type <- match.arg(se_type)

  sv      <- fit_svy$y
  times   <- sv[, "time"]
  status  <- sv[, "status"]
  w       <- weights(design)
  lp      <- fit_svy$linear.predictors
  exp_lp  <- exp(lp)
  n_obs   <- length(times)

  event_times <- sort(unique(times[status == 1L]))
  n_ev        <- length(event_times)

  dH      <- numeric(n_ev)
  Y_w     <- numeric(n_ev)
  n_w     <- numeric(n_ev)
  phi_run <- numeric(n_obs)

  if (se_type == "lin")
    Phi_cumul <- matrix(0, nrow = n_obs, ncol = n_ev)

  for (k in seq_len(n_ev)) {
    t_k      <- event_times[k]
    in_risk  <- times >= t_k
    is_event <- times == t_k & status == 1L

    Y_w[k]  <- sum(w[in_risk] * exp_lp[in_risk])
    n_w[k]  <- sum(w[is_event])
    dH[k]   <- n_w[k] / Y_w[k]

    if (se_type == "lin") {
      phi_k          <- as.numeric(is_event) / Y_w[k] -
                        (n_w[k] / Y_w[k]^2) * as.numeric(in_risk) * exp_lp
      phi_run        <- phi_run + phi_k
      Phi_cumul[, k] <- phi_run
    }
  }

  H0        <- cumsum(dH)
  centering <- if (centered) exp(-weighted.mean(lp, w)) else 1.0
  H0        <- H0 * centering

  if (se_type == "lin") {
    # Lin (2000) eq. 2.4: linearization variance via PSU-level influence totals.
    # Measures design (PSU-selection) uncertainty. Very small for NHANES-scale
    # populations because rare events are similar across PSUs.
    Phi_cumul <- Phi_cumul * centering
    strata    <- design$strata[[1L]]
    clusters  <- design$cluster[[1L]]
    var_H0    <- numeric(n_ev)

    for (h in unique(strata)) {
      in_h <- strata == h
      psus <- unique(clusters[in_h])
      n_h  <- length(psus)
      if (n_h < 2L) next

      psu_totals <- vapply(psus, function(alpha) {
        rows <- which(in_h & clusters == alpha)
        colSums(Phi_cumul[rows, , drop = FALSE])
      }, numeric(n_ev))
      psu_totals <- t(psu_totals)   # n_h x n_ev

      psu_means  <- colMeans(psu_totals)
      deviations <- sweep(psu_totals, 2L, psu_means, `-`)
      var_H0 <- var_H0 + (n_h / (n_h - 1L)) * colSums(deviations^2)
    }

  } else {
    # Greenwood-weighted: cumsum(n_w / Y_w^2).
    # Analogous to Nelson-Aalen variance on the population scale.
    # Gives confidence bands of interpretable width for survplot().
    var_H0 <- cumsum(n_w / Y_w^2) * centering^2
  }

  se_H0     <- sqrt(pmax(var_H0, 0))
  se_log_H0 <- se_H0 / pmax(H0, .Machine$double.eps)

  data.frame(
    time    = event_times,
    hazard  = H0,
    surv    = exp(-H0),
    se_H0   = se_H0,
    std.err = se_log_H0
  )
}


#' Substitute weighted baseline hazard into a fused cph object
#'
#' Replaces the baseline hazard estimate in a fused \code{cph} object with the
#' survey-weighted version from \code{weighted_basehaz()}, enabling
#' \code{survplot()} to produce design-correct survival curves.
#'
#' @param fit_fused A \code{svycph_fused} object from \code{svycph_fuse()}.
#' @param h0 A data frame returned by \code{weighted_basehaz()}.
#'
#' @return The modified fused object with weighted baseline hazard.
#'
#' @seealso \code{\link{svycph_fuse}}, \code{\link{weighted_basehaz}}
#' @export
svycph_set_basehaz <- function(fit_fused, h0) {
  # cph stores the baseline hazard as:
  #   $time    -- numeric vector of time points (from time=0)
  #   $surv    -- numeric vector of baseline survival S_0(t) = exp(-H_0(t))
  #   $std.err -- standard errors of baseline survival (set to NA here;
  #              variance of weighted baseline hazard requires Lin 2000 eq. 2.4)
  #
  # h0 is a data frame from weighted_basehaz() with columns time, hazard, surv.
  # We prepend time=0 / surv=1 to match cph convention.

  # Prepend t=0 anchor (S=1, SE=0) to match cph convention
  t_aug    <- c(0,           h0$time)
  surv_aug <- c(1,           h0$surv)
  se_aug   <- c(0,           if ("std.err" %in% names(h0)) h0$std.err
                             else rep(NA_real_, nrow(h0)))

  fit_fused$time    <- t_aug
  fit_fused$surv    <- surv_aug
  fit_fused$std.err <- se_aug
  fit_fused$maxtime <- max(h0$time)

  fit_fused
}


# svycoxph already stores degf.resid = n_PSU - n_strata; no need to recompute.
