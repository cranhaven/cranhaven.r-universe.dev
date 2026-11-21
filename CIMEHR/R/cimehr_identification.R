.cimehr_id_basis <- function(X, tol = 1e-7) {
  X <- as.matrix(X)
  s <- sqrt(colSums(X^2)); keep <- is.finite(s) & s > 0
  if (!any(keep)) return(matrix(0, nrow(X), 0L))
  A <- sweep(X[, keep, drop = FALSE], 2L, s[keep], '/')
  z <- svd(A, nu = min(dim(A)), nv = 0L)
  rank <- sum(z$d > tol * max(z$d))
  z$u[, seq_len(rank), drop = FALSE]
}

.cimehr_id_collinearity <- function(gram, compensation, tol = 1e-7) {
  ans <- list(status = 'not_applicable', minimum_residual_fraction = NA_real_,
    condition_number = NA_real_, warning_threshold = .1,
    definition = 'smallest residual norm of a unit compensation direction after projection on the outcome design; diagnostic threshold only')
  if (!length(compensation)) return(ans)
  G <- as.matrix(gram)
  if (any(!is.finite(G)) || any(diag(G) <= 0)) {
    ans$status <- 'not_assessed'; return(ans)
  }
  G <- G / outer(sqrt(diag(G)), sqrt(diag(G)))
  G <- (G + t(G))/2
  ev <- eigen(G, symmetric = TRUE, only.values = TRUE)$values
  ans$condition_number <- if (min(ev) > 0) sqrt(max(ev)/min(ev)) else Inf
  b <- compensation; x <- setdiff(seq_len(ncol(G)), b)
  R <- G[b, b, drop = FALSE]
  if (length(x)) {
    xx <- eigen(G[x, x, drop = FALSE], symmetric = TRUE)
    keep <- xx$values > tol^2 * max(xx$values)
    Q <- xx$vectors[, keep, drop = FALSE]
    inv <- sweep(Q, 2L, xx$values[keep], '/') %*% t(Q)
    R <- R - G[b, x, drop = FALSE] %*% inv %*% G[x, b, drop = FALSE]
  }
  bb <- eigen(G[b, b, drop = FALSE], symmetric = TRUE)
  if (any(bb$values <= 0)) { ans$status <- 'not_assessed'; return(ans) }
  W <- sweep(bb$vectors, 2L, sqrt(bb$values), '/')
  E <- crossprod(W, R %*% W)
  d <- eigen((E+t(E))/2, symmetric = TRUE, only.values = TRUE)$values
  ans$minimum_residual_fraction <- sqrt(max(0, min(1, min(d))))
  ans$status <- if (ans$minimum_residual_fraction < ans$warning_threshold)
    'near_collinear' else 'no_warning_detected'
  ans
}

.cimehr_identification_diagnostics <- function(XO, outcome_modifiers, alpha,
    observation_vcov, observed_subject, n_subjects, gram, compensation,
    zero_spec, delta = numeric(), gram_scope = '', tol = 1e-7,
    compensation_rule = NULL) {
  XO <- as.matrix(XO)[observed_subject, , drop = FALSE]
  O <- as.matrix(outcome_modifiers)[observed_subject, , drop = FALSE]
  Q <- .cimehr_id_basis(O, tol)
  R <- XO - Q %*% crossprod(Q, XO)
  small <- sqrt(colSums(R^2)) <= tol * sqrt(colSums(XO^2))
  R[, small] <- 0
  E <- .cimehr_id_basis(R, tol)
  r <- ncol(E)
  threshold <- sqrt(log(n_subjects))
  exclusion <- list(extra_rank = r, outcome_rank = ncol(Q),
    observation_rank = ncol(.cimehr_id_basis(XO, tol)),
    n_subjects_with_visits = nrow(XO), comparison_scope =
      'subject-level observation design versus intercept and outcome fixed/time/trajectory modifier space, among subjects with visits; no comparison of variable names',
    exclusion_validity = 'not_testable_from_design_or_relevance')
  relevance <- list(status = if (r) 'not_assessed' else 'no_excluded_direction',
    statistic = NA_real_, dimension = r, threshold = threshold,
    rule = 'Andrews-Cheng-inspired signal diagnostic: sqrt(b^T Var(b)^(-1) b / dimension), threshold sqrt(log(n_subjects)); not a calibrated CIMEHR identification test')
  warning_codes <- warning_text <- character()
  add_warning <- function(code, text) {
    warning_codes <<- c(warning_codes, code)
    warning_text <<- c(warning_text, text)
  }
  if (identical(compensation_rule$status, 'omitted_no_relevant_exclusion'))
    add_warning('compensation_omitted', compensation_rule$message)
  if (!r) {
    if(length(compensation)) add_warning('functional_form', paste(
      'No observation covariate direction lies outside the outcome modifier space;',
      'separation of compensation relies on the model functional form. Computed SEs and CIs are retained.'))
  } else {
    an <- paste0('alpha.', colnames(XO))
    if (is.matrix(observation_vcov) && all(an %in% rownames(observation_vcov))) {
      Va <- observation_vcov[an, an, drop = FALSE]
      L <- crossprod(E, XO) / sqrt(nrow(XO))
      b <- drop(L %*% alpha)
      V <- L %*% Va %*% t(L)
      if (all(is.finite(V)) && all(is.finite(b))) {
        ee <- eigen((V+t(V))/2, symmetric = TRUE)
        if (all(ee$values > 0) && min(ee$values)/max(ee$values) > 1e-10) {
          relevance$statistic <- sqrt(sum(drop(crossprod(ee$vectors, b))^2/ee$values)/r)
          relevance$status <- if (relevance$statistic <= threshold)
            'weak_relevance' else 'no_warning_detected'
        }
      }
    }
    if (length(compensation) && relevance$status == 'weak_relevance')
      add_warning('weak_relevance', paste(
        'The additional observation covariate directions have weak standardized recording signal;',
        'the Andrews-Cheng-inspired relevance screen is a diagnostic, not a validity test. Computed SEs and CIs are retained.'))
    if (length(compensation) && relevance$status == 'not_assessed')
      add_warning('relevance_not_assessed', 'Relevance could not be assessed from the available observation covariance; this does not remove computed outcome SEs or CIs.')
  }
  collinearity <- .cimehr_id_collinearity(gram, compensation, tol)
  collinearity$design_scope <- gram_scope
  if (collinearity$status == 'near_collinear')
    add_warning('near_collinear', paste(
      'A compensation direction is nearly explained by the outcome design (residual norm fraction below 0.1);',
      'estimation may be unstable. Computed SEs and CIs are retained.'))
  loading <- list(status = 'not_applicable', standardized_loadings = numeric(),
    third_largest_signal = NA_real_, threshold = threshold,
    population_identification = 'not_verified',
    rule = 'third largest absolute loading/stacked-SE compared with sqrt(log(n_subjects)); descriptive screen, not a test establishing population identification')
  if (identical(zero_spec$kind, 'raw_unique')) {
    dn <- paste0('delta.', names(delta))
    loading$status <- 'not_assessed'
    if (length(delta) >= 3L && is.matrix(observation_vcov) &&
        all(dn %in% rownames(observation_vcov))) {
      v <- diag(observation_vcov[dn, dn, drop = FALSE])
      z <- rep(NA_real_, length(v)); names(z) <- names(delta)
      good <- is.finite(v) & v > 0 & is.finite(delta)
      z[good] <- abs(delta[good])/sqrt(v[good])
      loading$standardized_loadings <- z
      if (sum(good) >= 3L) {
        loading$third_largest_signal <- sort(z[good], decreasing = TRUE)[3L]
        loading$status <- if (loading$third_largest_signal <= threshold)
          'weak_loading' else 'no_warning_detected'
      }
    }
    if (loading$status == 'weak_loading')
      add_warning('weak_loading', paste(
        'The fitted common-sign loading decomposition is close to a potentially unidentified loading structure;',
        'nonzero fitted loadings may reflect sampling noise. Local numerical rank does not establish reliable loading inference. Computed SEs and CIs are retained.'))
    add_warning('loading_regularity_not_verified', paste(
      'The zero-frailty loading decomposition is locally unique at the fitted point only;',
      'population loading regularity and common-sign orientation are not verified by numerical rank or the signal screen.'))
  }
  list(zero_frailty = TRUE, exclusion = exclusion, relevance = relevance,
    compensation = collinearity, loading = loading,
    warning_codes = unique(warning_codes), warnings = unique(warning_text),
    status = if (length(warning_codes)) paste(unique(warning_codes), collapse = ';') else 'no_warning_detected',
    coverage_status = 'not_established_by_diagnostics')
}

.cimehr_identification_assessment <- function(details, stage = 'outcome') {
  scope <- paste('conditional on the specified model, the identified compensation',
    'parameterization and the selected zero-frailty branch/variance face;',
    'diagnostic screens do not establish population identification or coverage')
  result <- function(valid, status, reason) list(valid = valid, status = status,
    reason = reason, scope = scope)
  if (identical(stage, 'visiting')) return(result(TRUE, 'conditional_supported',
    'observation and outcome identification screens do not apply to visiting parameters'))
  descriptive <- is.list(details) && isTRUE(details$zero_frailty)
  sign_scope <- if (!identical(stage, 'observation_mean') && descriptive &&
      !is.null(details$loading$status) && !identical(details$loading$status, 'not_applicable'))
    '; individual loading inference is conditional on the returned common-sign orientation' else ''
  result(TRUE, 'conditional_supported', paste0(
    'conditional inference is retained when the fit passes its convergence and target-estimability checks and the relevant covariance is computable; loading, relevance and collinearity screens do not determine this status',
    if (!descriptive) '; descriptive identification diagnostics are unavailable' else '',
    sign_scope, '; validity remains conditional on the stated model and selected-branch/face assumptions, not a guarantee of unconditional coverage'))
}

.cimehr_attach_identification_diagnostics <- function(fit, diagnostics) {
  fit$diagnostics$identification_diagnostics <- diagnostics
  fit$diagnostics$identification_warning <- diagnostics$warnings
  fit$diagnostics$identification_status <- diagnostics$status
  assessment <- .cimehr_identification_assessment(diagnostics)
  fit$diagnostics$identification_assessment <- assessment
  fit$diagnostics$inference_validity <- assessment$status
  if (length(diagnostics$warnings)) {
    msg <- paste0('Zero-frailty identification diagnostics: ',
      paste(diagnostics$warnings, collapse = ' '))
    warning(structure(list(message = msg, call = NULL,
      diagnostics = diagnostics), class = c('cimehr_identification_warning', 'warning', 'condition')))
  }
  fit
}

.cimehr_identification_signal <- new.env(parent = emptyenv())
.cimehr_identification_signal$quiet <- FALSE

.cimehr_without_identification_signal <- function(expr) {
  old <- .cimehr_identification_signal$quiet
  .cimehr_identification_signal$quiet <- TRUE
  on.exit(.cimehr_identification_signal$quiet <- old, add = TRUE)
  expr
}

.cimehr_signal_identification_warning <- function(fit) {
  warnings <- fit$diagnostics$identification_warning
  if (isTRUE(.cimehr_identification_signal$quiet)) return(invisible(fit))
  if (length(warnings)) {
    msg <- paste0('Zero-frailty identification diagnostics: ', paste(warnings, collapse = ' '))
    warning(structure(list(message = msg, call = NULL,
      diagnostics = fit$diagnostics$identification_diagnostics),
      class = c('cimehr_identification_warning', 'warning', 'condition')))
  }
  .cimehr_signal_numerical_warning(fit)
  invisible(fit)
}

.cimehr_identification_line <- function(diagnostics) {
  if (!length(diagnostics$identification_warning)) return(NULL)
  codes <- diagnostics$identification_diagnostics$warning_codes %||% diagnostics$identification_status
  paste0('Identification warnings: ', paste(codes, collapse = ', '),
    '; see diagnostics$identification_warning')
}

# Affine equation: H' W D.
.cimehr_numerical_diagnostics <- function(bread = NULL, relative_minimum = NULL,
    solver = NULL, spline = FALSE) {
  threshold <- 1e-7
  measure <- if (spline) 'smallest-to-largest absolute eigenvalue ratio of the existing standardized outcome Gram matrix' else
    'smallest-to-largest singular-value ratio of the column-scaled original outcome estimating matrix'
  if (is.null(relative_minimum)) {
    relative_minimum <- tryCatch({
      matrix <- as.matrix(bread)
      scales <- sqrt(colSums(matrix^2))
      if (!length(scales) || any(!is.finite(matrix)) || any(!is.finite(scales))) NA_real_ else
      if (any(scales == 0)) 0 else {
        values <- svd(sweep(matrix, 2L, scales, '/'), nu = 0L, nv = 0L)$d
        min(values)/max(values)
      }
    }, error = function(e) NA_real_)
  }
  relative_minimum <- as.numeric(relative_minimum)[1L]
  assessed <- length(relative_minimum) == 1L && is.finite(relative_minimum)
  near <- assessed && relative_minimum < threshold
  rescued <- isTRUE(solver$numerical_rescue)
  status <- if (rescued) 'qr_rescue' else if (near) 'near_singular' else
    if (assessed) 'no_warning_detected' else 'not_assessed'
  text <- character()
  if (rescued || near) {
    text <- paste0(
      if (rescued) 'QR rescue was used to solve the original Stage-3 outcome equation. ' else '',
      'The original outcome design or estimating matrix is numerically near singular; ',
      'this can involve fixed effects, baseline terms or compensation, rather than compensation alone. ',
      'Coefficient estimates and standard errors may be sensitive to small numerical or data changes. ',
      'Estimates and all computed covariance, SE and CI values are retained. ',
      'This warning does not change inference flags or establish population identification or confidence-interval coverage.')
  }
  list(status = status, warnings = text, qr_rescue = rescued,
    near_singular_equation = near, measure = measure, relative_minimum = relative_minimum,
    warning_threshold = threshold,
    equation_scope = if (spline) 'original spline outcome estimating equation' else 'original affine outcome estimating equation H\' W D',
    scope = 'warning-only numerical conditioning screen; the threshold never controls fitting, covariance, interval availability or inference flags',
    solver = solver)
}

.cimehr_numerical_warning_condition <- function(fit) {
  structure(list(message = paste0('CIMEHR numerical diagnostics: ',
    paste(fit$diagnostics$numerical_warning, collapse = ' ')), call = NULL,
    diagnostics = fit$diagnostics$numerical_diagnostics),
    class = c('cimehr_numerical_warning', 'warning', 'condition'))
}

.cimehr_attach_numerical_diagnostics <- function(fit, diagnostics) {
  fit$diagnostics$numerical_diagnostics <- diagnostics
  fit$diagnostics$numerical_warning <- diagnostics$warnings
  fit$diagnostics$numerical_status <- diagnostics$status
  if (length(diagnostics$warnings)) warning(.cimehr_numerical_warning_condition(fit))
  fit
}

.cimehr_signal_numerical_warning <- function(fit) {
  if (length(fit$diagnostics$numerical_warning) && !isTRUE(.cimehr_identification_signal$quiet))
    warning(.cimehr_numerical_warning_condition(fit))
  invisible(fit)
}

.cimehr_numerical_line <- function(diagnostics) {
  if (!length(diagnostics$numerical_warning)) return(NULL)
  paste0('Numerical warning: ', paste(diagnostics$numerical_warning, collapse = ' '))
}
