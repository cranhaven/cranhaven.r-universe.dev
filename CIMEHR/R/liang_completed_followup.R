.liang_completed_varying_followup <- function(d, id, censor_col) {
  if (is.null(censor_col)) return(FALSE)
  ids <- unique(d[[id]]); si <- match(d[[id]], ids)
  cc <- as.numeric(d[[censor_col]])
  first <- match(ids, d[[id]])
  if (any(!is.finite(cc))) stop("Every subject needs a finite censoring time.", call. = FALSE)
  if (any(abs(cc - cc[first][si]) > 1e-10))
    stop("Censoring time must be constant within each subject.", call. = FALSE)
  diff(range(cc[first])) > 1e-10
}

.liang_completed_visit_stage <- function(d, id, time, visit_covs, censor_col,
    sigma_floor) {
  ids <- unique(d[[id]]); si <- match(d[[id]], ids)
  first <- match(ids, d[[id]]); n <- length(ids)
  V <- as.matrix(d[first, visit_covs, drop = FALSE]); storage.mode(V) <- "double"
  if (any(!is.finite(V))) stop("Baseline visit covariates must be finite.", call. = FALSE)
  for (v in visit_covs) if (any(is.finite(d[[v]]) &
      abs(d[[v]] - d[[v]][first][si]) > 1e-10, na.rm = TRUE))
    stop("Liang requires time-invariant covariates: ", v, call. = FALSE)
  actual <- is.finite(d[[time]])
  if (!any(actual)) stop("No visit events.", call. = FALSE)
  entry <- rep(-Inf, n); censor <- as.numeric(d[[censor_col]][first])
  if (any(d[[time]][actual] > censor[si[actual]] + 1e-10))
    stop("A clinic visit occurs after its subject's censoring time.", call. = FALSE)
  ei <- si[actual]; et <- d[[time]][actual]
  rate_engine <- .liang_visit_gamma_evaluator(V, ei, et, entry, censor)
  gamma_at <- rate_engine$evaluate
  og <- stats::optim(rep(0, ncol(V)), function(g) gamma_at(g)$objective,
    gr = function(g) -gamma_at(g)$score, method = "BFGS",
    control = list(maxit = 1000L), hessian = TRUE)
  pg <- og$par
  for (it in seq_len(25L)) {
    pp <- gamma_at(pg)
    if (max(abs(pp$score)) <= 1e-9 * (1 + length(et))) break
    st <- tryCatch(drop(solve(pp$information, pp$score)), error = function(e) NULL)
    if (is.null(st)) break
    f <- 1; accepted <- FALSE
    while (f >= 2^-20) {
      candidate <- pg + f * st; value <- gamma_at(candidate)$objective
      if (is.finite(value) && value <= pp$objective + 1e-12 * (1 + abs(pp$objective))) {
        pg <- candidate; accepted <- TRUE; break
      }
      f <- f / 2
    }
    if (!accepted) break
  }
  gs <- gamma_at(pg)
  if (max(abs(gs$score)) > 1e-5 * (1 + length(et)))
    stop("Rate nuisance did not converge.", call. = FALSE)
  bl <- rate_engine$breslow(pg)
  exposure <- .liang_visit_stable_interval(bl$time, bl$log_jump,
    matrix(1, length(bl$time), 1L), entry, censor)
  Lambda <- exp(exposure$log_sum)
  m <- tabulate(ei, nbins = n)
  mu <- exp(drop(V %*% pg) + exposure$log_sum)
  den <- sum(mu^2)
  if (!is.finite(den) || den <= 0) stop("Frailty variance moment is unidentified.", call. = FALSE)
  raw <- sum(m * (m - 1) - mu^2) / den; sig <- max(raw, 0)
  use <- sig > sigma_floor
  B <- if (use) (1 + m * sig) / (1 + mu * sig) - 1 else rep(0, n)
  ga <- list(gamma = setNames(pg, visit_covs), ids = ids, si = si, first = first,
    V = V, m = m, M = sum(m), exp_gamma = exp(drop(V %*% pg)),
    converged = TRUE, code = 0L, score_max = max(abs(gs$score)))
  list(ga = ga, Lambda = setNames(Lambda, as.character(ids)),
    sigma2_raw = raw, sigma2 = sig, use = use, B = B, baseline_rate = bl,
    censor = setNames(censor, as.character(ids)))
}

.liang_completed_followup_fit <- function(d, id, time, outcome, visit_covs,
    long_covs, random_covs, sigma_floor, censor_col, estimate_intercept = FALSE,
    baseline_formula = ~1, baseline_terms = NULL) {
  actual <- is.finite(d[[time]])
  if (any(actual & !is.finite(d[[outcome]])))
    stop("Liang analysis requires a finite completed outcome at every actual visit.", call. = FALSE)
  ids <- unique(d[[id]]); first <- match(ids, d[[id]]); si <- match(d[[id]], ids)
  if (any(d[[time]][actual] > d[[censor_col]][first][si[actual]] + 1e-10))
    stop("A clinic visit occurs after its subject's censoring time.", call. = FALSE)
  internal_R <- ".liang_completed_observed"
  while (internal_R %in% names(d)) internal_R <- paste0(internal_R, "_")
  d[[internal_R]] <- as.integer(actual & is.finite(d[[outcome]]))
  fit <- .fit_liang_visit(d, id = id, time = time, outcome = outcome,
    obs_indicator = internal_R, visit_covs = visit_covs, long_covs = long_covs,
    random_covs = random_covs, process = "visit", censor_col = censor_col,
    estimate_intercept = estimate_intercept, baseline_formula = baseline_formula,
    baseline_terms = baseline_terms, frailty_threshold = sigma_floor,
    inference = "stacked")
  renamed <- function(nm) {
    nm <- sub("^beta:", "", nm)
    nm <- sub("^theta:frailty_intercept$", "theta:(Intercept)", nm)
    sub("^theta:frailty_", "theta:", nm)
  }
  ca <- fit$inference$coefficients_outcome_all
  names(ca) <- renamed(names(ca))
  VA <- fit$inference$vcov_outcome_theta
  dimnames(VA) <- list(names(ca), names(ca))
  theta <- ca[setdiff(names(ca), names(fit$coefficients))]
  if (!length(theta)) theta <- setNames(rep(NA_real_, 1L + length(random_covs)),
    c("(Intercept)", random_covs))
  bl <- fit$nuisance$baseline_rate
  exposure <- .liang_visit_stable_interval(bl$time, bl$log_jump,
    matrix(1, length(bl$time), 1L), rep(-Inf, length(ids)), d[[censor_col]][first])
  baseline <- .mi_liang_baseline_design(d[[time]][actual], time,
    estimate_intercept, baseline_formula, prediction_terms = baseline_terms)
  list(coef = fit$coefficients, vcov = fit$vcov, coef_all = ca, vcov_all = VA,
    vcov_full = VA, theta = theta, estimate_intercept = estimate_intercept,
    baseline_formula = baseline$formula, baseline_terms = fit$nuisance$baseline_terms,
    baseline_assumption = baseline$assumption,
    model_scope = if (estimate_intercept) "Liang-time with an explicit deterministic outcome baseline" else
      "Liang profile estimating equation with unspecified outcome baseline",
    outcome_equation = if (estimate_intercept) "W'W c=W'Y with subject-specific visiting exposure" else
      "H'D b=H'Y with subject-specific at-risk centering and visiting exposure",
    vcov_type = fit$vcov_type,
    gamma = fit$nuisance$gamma, vcov_gamma = fit$nuisance$vcov_gamma,
    vcov_gamma_type = "Patient-level stacked covariance including estimated visiting rate and baseline",
    Lambda = setNames(exp(exposure$log_sum), as.character(ids)),
    sigma2 = fit$nuisance$frailty_variance,
    sigma2_raw = fit$nuisance$frailty_variance_untruncated,
    use_B = fit$diagnostics$theta_identified,
    converged = fit$converged, gamma_code = 0L, code = 0L,
    gamma_score_max = fit$diagnostics$gamma_score_max,
    score_max = fit$diagnostics$gamma_score_max,
    baseline_prediction = fit$prediction$baseline,
    condition = NA_real_, n_outcome = fit$diagnostics$n_outcomes,
    n_visit = fit$diagnostics$n_visit_events,
    zero_visit = fit$diagnostics$zero_event_subjects,
    followup_nuisance = list(baseline_rate = bl, subject_censoring = setNames(d[[censor_col]][first], as.character(ids)),
      posterior_frailty_minus_one = fit$nuisance$posterior_frailty_minus_one,
      full_parameters = fit$inference$full_parameters,
      full_vcov = fit$inference$vcov_all,
      nuisance_estimation_propagated = TRUE,
      conditional_active_branch = !fit$diagnostics$theta_identified))
}
