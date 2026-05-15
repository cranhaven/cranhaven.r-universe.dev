#' @noRd
#' @keywords internal
# -- Utility: safe builder + small helpers for mgcv::gam ---------------------

.fit_gam_models <- function(data, formula, family, args = list()) {
  # Lock fields we control to avoid accidental overrides
  args$formula <- NULL; args$data <- NULL; args$family <- NULL
  do.call(mgcv::gam, c(list(formula = formula, data = data, family = family), args))
}


.predict_gam_cols <- function(dat, fit, prefixes, suffix, level = 0.95) {
  # Predict on LINK scale to get valid SEs, then transform to RESPONSE via linkinv
  pr <- stats::predict(fit, newdata = dat, type = "link", se.fit = TRUE)
  inv <- fit$family$linkinv
  z  <- stats::qnorm(1 - (1 - level)/2)
  mu_resp  <- inv(pr$fit)
  lwr_resp <- inv(pr$fit - z * pr$se.fit)
  upr_resp <- inv(pr$fit + z * pr$se.fit)
  out <- list(
    stats::setNames(list(as.numeric(mu_resp)),  paste0(prefixes[1], suffix)),
    stats::setNames(list(as.numeric(lwr_resp)), paste0(prefixes[2], suffix)),
    stats::setNames(list(as.numeric(upr_resp)), paste0(prefixes[3], suffix))
  )
  as.data.frame(out)
}

# -- Helpers used in empirical curves ---------------------------------------
.rescale_01 <- function(x) {
  r <- range(x, na.rm = TRUE, finite = TRUE)
  if (!is.finite(r[1]) || !is.finite(r[2]) || r[2] == r[1]) return(rep(0.5, length(x)))
  (x - r[1]) / (r[2] - r[1])
}

.sv01 <- function(y01) {
  n <- sum(!is.na(y01))
  if (n <= 1L) return(y01)
  ((y01 * (n - 1)) + 0.5) / n
}

.clamp01 <- function(p, eps = 1e-6) pmin(pmax(p, eps), 1 - eps)

.linkinv_fun <- function(fam) if (!is.null(fam$linkinv)) fam$linkinv else stop("Family lacks linkinv.")


.predict_ocat_ci_delta <- function(fit, newdata, level = 0.95) {
  stopifnot(inherits(fit, "gam"))

  pr <- stats::predict(fit, newdata = newdata, type = "link", se.fit = TRUE)
  eta_hat <- as.numeric(pr$fit)
  se_hat  <- pmax(0, as.numeric(pr$se.fit))
  n <- length(eta_hat)

  theta <- fit$family$getTheta(trans = TRUE)
  if (is.null(theta)) theta <- numeric(0)
  Rm1 <- length(theta)
  if (Rm1 < 1L) stop("ocat family returned empty theta; ensure R was specified when fitting.")
  R <- Rm1 + 1L

  Fcum <- function(et) {
    mat <- stats::plogis(outer(et, theta, function(e, th) th - e))
    dim(mat) <- c(length(et), Rm1)
    mat
  }
  flog <- function(et) {
    F <- Fcum(et)
    F * (1 - F)
  }
  make_probs <- function(Fc) {
    Fc <- as.matrix(Fc)
    out <- matrix(NA_real_, nrow = nrow(Fc), ncol = ncol(Fc) + 1L)
    out[, 1] <- Fc[, 1]
    if (ncol(Fc) > 1L) out[, 2:ncol(Fc)] <- Fc[, 2:ncol(Fc), drop = FALSE] - Fc[, 1:(ncol(Fc)-1L), drop = FALSE]
    out[, ncol(out)] <- 1 - Fc[, ncol(Fc)]
    rs <- rowSums(out)
    rs[!is.finite(rs) | rs <= 0] <- 1
    out / rs
  }

  P_est <- make_probs(Fcum(eta_hat))
  r_idx <- matrix(rep(seq_len(R), each = n), nrow = n)
  E_est <- rowSums(P_est * r_idx)

  z <- stats::qnorm(1 - (1 - level)/2)
  f_vals <- flog(eta_hat)
  dP1 <- -f_vals[, 1, drop = TRUE]
  dPR <-  f_vals[, ncol(f_vals), drop = TRUE]
  dPmid <- if (R > 2L) f_vals[, 1:(R-2), drop = FALSE] - f_vals[, 2:(R-1), drop = FALSE] else NULL
  dP <- matrix(0, nrow = n, ncol = R)
  dP[, 1] <- dP1
  if (!is.null(dPmid)) dP[, 2:(R-1)] <- dPmid
  dP[, R] <- dPR
  dE <- rowSums(dP * r_idx)
  se_E <- abs(dE) * se_hat
  E_lwr <- pmax(1, pmin(R, E_est - z * se_E))
  E_upr <- pmax(1, pmin(R, E_est + z * se_E))

  out <- data.frame(E_est = E_est, E_lwr = E_lwr, E_upr = E_upr)
  out
}
