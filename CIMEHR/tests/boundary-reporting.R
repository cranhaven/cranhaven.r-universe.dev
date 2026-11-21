library(CIMEHR)

# Fixtures with supplied covariance entries.
make_reporting_fit <- function(method, boundary = TRUE, missing_se = FALSE,
                               failed = FALSE) {
  value <- if (boundary) 0 else 0.3
  dg <- list(ok = !failed, converged = !failed, identified = !failed,
    regular = !boundary, conditional_inference_valid = boundary,
    frailty_boundary = boundary, observation_variance_boundary = boundary,
    variance_boundary = boundary, inference_scope = 'supplied fixture covariance')
  b <- c(beta.X = -0.5, theta.X = 0.2, gamma.X = 0.5, sigma2_zeta = value, alpha.X = 0.2,
         delta.X = -0.4, sigma2_q.X = value)
  supplied_se <- c(beta.X = 0.06, theta.X = 0.09, gamma.X = 0.1, sigma2_zeta = 0.2, alpha.X = 0.3,
                   delta.X = 0.4, sigma2_q.X = 0.5)
  if (missing_se) supplied_se[c('sigma2_zeta', 'sigma2_q.X')] <- NA_real_
  if (method %in% c('cimehr', 'cimehr_hl')) {
    return(structure(list(method_key = method, coefficients = b,
      se = supplied_se, vcov_all = structure(diag(supplied_se^2),
        dimnames = list(names(b), names(b))), diagnostics = dg), class = 'cimehr_fit'))
  }
  V <- diag(supplied_se^2)
  dimnames(V) <- list(names(b), names(b))
  nuisance <- list(visiting_process = list(gamma_V = c(X = b['gamma.X']),
    sigma2_zeta = value, convergence = 0L, frailty_boundary = boundary),
    observation_process = list(alpha_fixed = c(X = 0.2),
      delta_latent_random_link = c(X = -0.4), Sigma_q = c(X = value),
      boundary_variance = c(X = boundary)))
  nuisance$visiting_process$gamma_V <- c(X = 0.5)
  structure(list(method_key = method, coefficients = b[c('beta.X','theta.X')],
    se = supplied_se[c('beta.X','theta.X')], diagnostics = dg,
    fit = list(nuisance_summary = nuisance,
      stacked_sandwich = list(vcov_joint = V, regular = !boundary,
        scope = 'supplied fixture covariance'))), class = 'cimehr_fit')
}

one_row <- function(tab, key) {
  out <- tab[tab$parameter == key, , drop = FALSE]
  stopifnot(nrow(out) == 1L)
  out
}

for (method in c('cimehr', 'cimehr_hl', 'cimehr_spline', 'cimehr_spline_hl')) {
  fit <- make_reporting_fit(method)
  preserved <- fit
  for (stage in c('visiting', 'observation')) {
    summarize <- if (stage == 'visiting') summary_visiting else summary_observation
    key <- if (stage == 'visiting') 'sigma2_zeta' else 'sigma2_q.X'
    expected_se <- if (stage == 'visiting') 0.2 else 0.5
    row <- one_row(summarize(fit), key)
    expected <- stats::qnorm(0.975) * expected_se
    stopifnot(row$point_valid, row$se_available, !row$ci_available,
      !row$inference_valid, !row$regular_inference_valid,
      !row$conditional_inference_available,
      identical(row$se, expected_se),
      is.na(row$lower), is.na(row$upper),
      is.na(row$regular_lower), is.na(row$regular_upper),
      is.na(row$conditional_lower), is.na(row$conditional_upper),
      row$parameter_boundary, row$fit_boundary,
      row$se_status == 'available', row$ci_status == 'boundary_uncalibrated',
      !'numerical_lower' %in% names(row))
    warned <- FALSE
    explicit <- withCallingHandlers(summarize(fit, numerical_wald = TRUE),
      warning = function(w) { warned <<- TRUE; invokeRestart('muffleWarning') })
    explicit <- one_row(explicit, key)
    stopifnot(warned, explicit$numerical_wald_available,
      isTRUE(all.equal(c(explicit$numerical_lower, explicit$numerical_upper),
        c(-expected, expected))), is.na(explicit$lower), is.na(explicit$upper),
      !explicit$inference_valid,
      grepl('coverage is not established', explicit$numerical_wald_scope, fixed = TRUE))

    # Missing covariance.
    unavailable <- one_row(summarize(make_reporting_fit(method,
      missing_se = TRUE)), key)
    stopifnot(is.na(unavailable$se), !unavailable$se_available,
      !unavailable$ci_available, !unavailable$inference_valid,
      is.na(unavailable$lower), is.na(unavailable$upper),
      unavailable$se_status == 'boundary_not_computed',
      unavailable$ci_status == 'se_unavailable')

    # Failed point fit with a finite supplied SE.
    failure <- one_row(summarize(make_reporting_fit(method, failed = TRUE)), key)
    stopifnot(!failure$point_valid, failure$se_available,
      !failure$ci_available, !failure$inference_valid,
      is.na(failure$lower), is.na(failure$upper),
      failure$ci_status == 'fit_invalid')

    # Interior variance inference.
    interior <- one_row(summarize(make_reporting_fit(method,
      boundary = FALSE)), key)
    stopifnot(interior$regular_inference_valid, interior$inference_valid,
      interior$ci_available, interior$conditional_inference_available,
      isTRUE(all.equal(c(interior$lower, interior$upper),
        c(0.3 - expected, 0.3 + expected))),
      identical(interior$lower, interior$regular_lower),
      identical(interior$upper, interior$regular_upper),
      !interior$parameter_boundary, !interior$fit_boundary,
      interior$se_status == 'available', interior$ci_status == 'regular')
  }
  # Zero-frailty selected-face covariance.
  delta <- one_row(summary_observation(fit), 'delta.X')
  stopifnot(delta$ci_available, delta$inference_valid,
    delta$conditional_inference_available, !delta$regular_inference_valid,
    !delta$parameter_boundary, delta$fit_boundary, delta$ci_status == 'conditional',
    delta$inference_status == 'conditional_supported',
    identical(delta$se, 0.4),
    isTRUE(all.equal(c(delta$lower, delta$upper),
      c(-0.4 - stats::qnorm(0.975) * 0.4, -0.4 + stats::qnorm(0.975) * 0.4))))
  gamma <- one_row(summary_visiting(fit), 'gamma.X')
  stopifnot(gamma$ci_available, gamma$inference_valid,
    gamma$regular_inference_valid)
  outcome <- one_row(summary_outcome(fit), 'beta.X')
  stopifnot(outcome$ci_available, outcome$conditional_inference_available,
    !outcome$regular_inference_valid, !outcome$parameter_boundary,
    outcome$fit_boundary, outcome$ci_status == 'conditional',
    outcome$inference_valid, outcome$inference_status == 'conditional_supported',
    identical(outcome$se, 0.06),
    isTRUE(all.equal(c(outcome$lower, outcome$upper),
      c(-0.5 - stats::qnorm(0.975) * 0.06, -0.5 + stats::qnorm(0.975) * 0.06))))
  stopifnot(identical(fit, preserved))

  # Failed covariance with informational diagnostics.
  bad_cov <- fit
  bad_cov$diagnostics$covariance_failed <- TRUE
  bad_out <- one_row(summary_outcome(bad_cov), 'beta.X')
  bad_delta <- one_row(summary_observation(bad_cov), 'delta.X')
  good_gamma <- one_row(summary_visiting(bad_cov), 'gamma.X')
  stopifnot(!bad_out$inference_valid, bad_out$inference_status == 'covariance_failure',
    !bad_delta$inference_valid, bad_delta$inference_status == 'covariance_failure',
    identical(bad_out$se, outcome$se), identical(bad_out$lower, outcome$lower),
    identical(bad_out$upper, outcome$upper), good_gamma$inference_valid,
    good_gamma$regular_inference_valid, good_gamma$inference_status == 'regular')

  # Target-specific SE availability.
  one_missing <- fit
  one_missing$se['beta.X'] <- NA_real_
  missing <- one_row(summary_outcome(one_missing), 'beta.X')
  retained <- one_row(summary_outcome(one_missing), 'theta.X')
  stopifnot(!missing$se_available, !missing$ci_available, !missing$inference_valid,
    retained$se_available, retained$ci_available, retained$inference_valid)

  for (flag in c('identified', 'converged', 'ok')) {
    failed_fit <- fit
    failed_fit$diagnostics[[flag]] <- FALSE
    failed_out <- summary_outcome(failed_fit)
    stopifnot(!any(failed_out$inference_valid %in% TRUE),
      all(failed_out$inference_status == 'fit_invalid'))
  }
}

# Comparator frailty-variance reporting.
comparator <- structure(list(method_key = 'ehrjoint',
  nuisance = list(frailty_variance = 0, se_sigma2 = 0.25),
  diagnostics = list(ok = TRUE, converged = TRUE, identified = TRUE,
    regular = FALSE, frailty_boundary = TRUE)), class = 'cimehr_fit')
row <- one_row(summary_visiting(comparator), 'frailty_variance')
stopifnot(!row$ci_available, !row$inference_valid, row$se_available,
  row$parameter_boundary, is.na(row$lower), is.na(row$upper))


# Disabled, boundary, and unknown inference states.
for (method in c('cimehr', 'cimehr_hl', 'cimehr_spline', 'cimehr_spline_hl')) {
  fit <- make_reporting_fit(method, missing_se = TRUE)
  fit$fit$model_info$inference <- 'none'
  disabled <- one_row(summary_observation(fit), 'sigma2_q.X')
  stopifnot(disabled$se_status == 'disabled', disabled$parameter_boundary,
    disabled$ci_status == 'se_unavailable')
}
legacy <- structure(list(method_key = 'ehrjoint',
  nuisance = list(frailty_variance = 0),
  diagnostics = list(ok = TRUE, converged = TRUE)), class = 'cimehr_fit')
stopifnot(summary_visiting(legacy)$se_status == 'unknown')
legacy$diagnostics$identified <- FALSE
stopifnot(summary_visiting(legacy)$se_status == 'identifiability_failure')
legacy$diagnostics$identified <- TRUE
legacy$diagnostics$covariance_failed <- TRUE
stopifnot(summary_visiting(legacy)$se_status == 'numerical_failure')
legacy$diagnostics$covariance_failed <- NULL
legacy$diagnostics$inference_scope <- 'not computed'
stopifnot(summary_visiting(legacy)$se_status == 'not_computed')

# Negative covariance diagonals.
negative <- structure(list(method_key = 'lmm', coefficients = c(X = 0.5),
  vcov = matrix(-0.1, 1L, 1L, dimnames = list('X', 'X')),
  diagnostics = list(ok = TRUE, converged = TRUE, regular = TRUE)),
  class = 'cimehr_fit')
row <- summary_outcome(negative)
stopifnot(is.na(row$se), !row$se_available, !row$ci_available,
  row$se_status == 'numerical_failure')
negative$method_key <- 'ehrjoint'
negative$nuisance <- list(gamma = c(X = 0.5), gamma_vcov = negative$vcov)
row <- summary_visiting(negative)
stopifnot(is.na(row$se), row$se_status == 'numerical_failure', !row$ci_available)
for (method in c('cimehr_spline', 'cimehr_spline_hl')) {
  fit <- make_reporting_fit(method, boundary = FALSE)
  fit$fit$stacked_sandwich$vcov_joint['sigma2_q.X', 'sigma2_q.X'] <- -0.1
  row <- one_row(summary_observation(fit), 'sigma2_q.X')
  stopifnot(is.na(row$se), !row$se_available, !row$ci_available,
    row$se_status == 'numerical_failure')
}
for (bad in list(NA, 1, 'yes', c(TRUE, FALSE))) {
  stopifnot(inherits(try(summary_visiting(comparator, numerical_wald = bad),
    silent = TRUE), 'try-error'))
}


# Disabled inference in legacy fits.
none_native <- make_reporting_fit('cimehr', missing_se = TRUE)
none_native$fit <- list(call = quote(CIMEHR(data, inference = requested_mode)),
  diagnostics = list(inference_scope = 'not computed'), inference = NULL)
stopifnot(one_row(summary_observation(none_native), 'sigma2_q.X')$se_status == 'disabled')
# Missing inference metadata.
unknown <- structure(list(method_key = 'lmm', coefficients = c(X = 0.5),
  se = c(X = NA_real_), diagnostics = list(ok = TRUE, converged = TRUE)),
  class = 'cimehr_fit')
stopifnot(summary_outcome(unknown)$se_status == 'unknown')
# Numerical limits, point estimates, and reference df.
for (bad_df in c(NA_real_, 0, -1)) {
  fit <- make_reporting_fit('cimehr')
  fit$df <- bad_df
  tab <- summary_outcome(fit)
  stopifnot(all(tab$ci_status == 'reference_unavailable'), all(is.na(tab$lower)))
}


# Atomic and S4 native fits.
methods::setClass('CIMEHRReportingNativeFixture', slots = c(value = 'numeric'), where = .GlobalEnv)
for (native in list(3, 'external fit', methods::new('CIMEHRReportingNativeFixture', value = 1))) {
  fit <- unknown
  fit$fit <- native
  fit$method_key <- NULL
  stopifnot(summary_outcome(fit)$se_status == 'unknown')
}
for (bad_variance in c(-0.1, NaN, Inf, -Inf)) {
  fit <- negative
  fit$vcov[1, 1] <- bad_variance
  fit$se <- NULL
  row <- summary_outcome(fit)
  stopifnot(is.na(row$se), row$se_status == 'numerical_failure', !row$ci_available)
  fit$nuisance$gamma_vcov[1, 1] <- bad_variance
  row <- summary_visiting(fit)
  stopifnot(is.na(row$se), row$se_status == 'numerical_failure', !row$ci_available)
}
fit <- negative
fit$vcov[1, 1] <- NA_real_
stopifnot(summary_outcome(fit)$se_status == 'unknown')
for (bad_se in c(-0.1, NaN, Inf, -Inf)) {
  fit <- unknown
  fit$se <- c(X = bad_se)
  stopifnot(summary_outcome(fit)$se_status == 'numerical_failure')
}

# Positive frailty with failed covariance.
curve_report <- get('.report_curve_inference_metadata', asNamespace('CIMEHR'))
for (method in c('cimehr', 'cimehr_hl', 'cimehr_spline', 'cimehr_spline_hl')) {
  for (boundary in c(FALSE, TRUE)) {
    fit <- make_reporting_fit(method, boundary = boundary)
    before <- summary_outcome(fit)
    before_delta <- one_row(summary_observation(fit), 'delta.X')
    before_gamma <- one_row(summary_visiting(fit), 'gamma.X')
    fit$diagnostics$covariance_failed <- TRUE
    after <- summary_outcome(fit)
    after_delta <- one_row(summary_observation(fit), 'delta.X')
    after_gamma <- one_row(summary_visiting(fit), 'gamma.X')
    ci <- confint(fit)
    regular_ci <- confint(fit, type = 'regular')
    stopifnot(identical(after$estimate, before$estimate),
      identical(after$se, before$se), identical(after$lower, before$lower),
      identical(after$upper, before$upper), all(after$ci_available),
      !any(after$inference_valid), !any(after$regular_inference_valid),
      all(after$inference_status == 'covariance_failure'),
      identical(as.vector(ci[, 1]), after$lower),
      identical(as.vector(ci[, 2]), after$upper),
      all(is.na(regular_ci)),
      !after_delta$inference_valid,
      after_delta$inference_status == 'covariance_failure',
      identical(after_delta$lower, before_delta$lower),
      identical(after_delta$upper, before_delta$upper),
      identical(after_gamma, before_gamma))
    curve <- data.frame(time = c(.2, .7), fit = c(1, 1.1),
      lower = c(.8, .9), upper = c(1.2, 1.3))
    reported <- curve_report(curve, fit)
    stopifnot(identical(reported$lower, curve$lower),
      identical(reported$upper, curve$upper),
      all(attr(reported, 'ci_available')),
      !any(attr(reported, 'inference_valid')),
      all(attr(reported, 'inference_status') == 'covariance_failure'))
    # Bootstrap intervals with failed parent analytic covariance.
    boot <- curve
    boot$n_boot <- c(2L, 1L)
    boot_reported <- curve_report(boot, fit)
    stopifnot(identical(boot_reported$lower, boot$lower),
      identical(boot_reported$upper, boot$upper),
      identical(attr(boot_reported, 'inference_valid'), c(TRUE, FALSE)))
    analytic_unavailable <- fit
    analytic_unavailable$diagnostics$regular <- FALSE
    analytic_unavailable$diagnostics$conditional_inference_available <- FALSE
    analytic_unavailable$diagnostics$conditional_inference_valid <- FALSE
    boot_reported <- curve_report(boot, analytic_unavailable)
    stopifnot(identical(attr(boot_reported, 'inference_valid'), c(TRUE, FALSE)))

    for (flag in c('identified', 'converged', 'ok')) {
      failed <- make_reporting_fit(method, boundary = boundary)
      failed$diagnostics[[flag]] <- FALSE
      for (summarize in list(summary_outcome, summary_visiting, summary_observation)) {
        tab <- summarize(failed)
        stopifnot(!any(tab$point_valid), !any(tab$inference_valid),
          all(tab$inference_status == 'fit_invalid'))
      }
      reported <- curve_report(curve, failed)
      stopifnot(identical(reported$lower, curve$lower),
        identical(reported$upper, curve$upper),
        !any(attr(reported, 'inference_valid')),
        all(attr(reported, 'inference_status') == 'fit_invalid'))
      failed_boot <- curve_report(boot, failed)
      stopifnot(!any(attr(failed_boot, 'inference_valid')),
        all(attr(failed_boot, 'inference_status') == 'fit_invalid'),
        identical(failed_boot$lower, boot$lower),
        identical(failed_boot$upper, boot$upper))
    }

    # Target-specific bootstrap SE availability.
    missing <- make_reporting_fit(method, boundary = boundary)
    missing$se['beta.X'] <- NA_real_
    beta <- one_row(summary_outcome(missing), 'beta.X')
    theta <- one_row(summary_outcome(missing), 'theta.X')
    stopifnot(!beta$se_available, !beta$ci_available, !beta$inference_valid,
      theta$se_available, theta$ci_available, theta$inference_valid,
      one_row(summary_observation(missing), 'delta.X')$inference_valid,
      one_row(summary_visiting(missing), 'gamma.X')$inference_valid)
  }
}
