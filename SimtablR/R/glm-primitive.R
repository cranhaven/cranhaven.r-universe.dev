# GLM COMPUTATION PRIMITIVES AND ROBUST COVARIANCE
# Shared numerical routines for GLM model fitting, sandwich robust standard errors,
# log-binomial optimization, and robust Poisson prevalence ratio fallback machines.

#########
# ROBUST COVARIANCE AND WALD INFERENCE
# Heteroscedasticity-consistent (HC) sandwich matrices and Wald confidence intervals.

#' Compute model-based or heteroscedasticity-consistent sandwich covariance matrix
#' @keywords internal
#' @noRd
.glm_covariance <- function(fit, vcov = "HC0") {
  if (!is.character(vcov) || length(vcov) != 1 ||
      !vcov %in% c("HC0", "HC1", "HC2", "HC3", "none", "model")) {
    return(NULL)
  }
  tryCatch(
    {
      if (vcov %in% c("none", "model")) {
        stats::vcov(fit)
      } else {
        sandwich::vcovHC(fit, type = vcov)
      }
    },
    error = function(e) NULL
  )
}

#' Calculate robust Wald confidence intervals and p-values from GLM fit
#' @keywords internal
#' @noRd
.robust_wald_ci <- function(fit,
                            vcov = "HC0",
                            conf.level = 0.95,
                            exponentiate = TRUE,
                            include_intercept = FALSE,
                            terms = NULL) {
  empty_out <- data.frame(
    term = character(),
    estimate = numeric(),
    lower = numeric(),
    upper = numeric(),
    p = numeric()
  )

  if (!inherits(fit, "glm")) {
    return(empty_out)
  }
  if (!is.numeric(conf.level) || length(conf.level) != 1 || conf.level <= 0 || conf.level >= 1) {
    return(empty_out)
  }
  if (!is.character(vcov) || length(vcov) != 1 || !vcov %in% c("HC0", "HC1", "HC2", "HC3", "none", "model")) {
    return(empty_out)
  }

  coef_vec <- tryCatch(stats::coef(fit), error = function(e) NULL)
  vcov_mat <- .glm_covariance(fit, vcov)
  if (is.null(coef_vec) || is.null(vcov_mat)) {
    return(empty_out)
  }

  sel_terms <- if (is.null(terms)) names(coef_vec) else as.character(terms)
  if (!isTRUE(include_intercept)) {
    sel_terms <- setdiff(sel_terms, "(Intercept)")
  }
  sel_terms <- sel_terms[sel_terms %in% names(coef_vec)]
  if (length(sel_terms) == 0) {
    return(empty_out)
  }

  se <- sqrt(diag(vcov_mat))
  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  estimate_link <- coef_vec[sel_terms]
  lower_link <- estimate_link - z * se[sel_terms]
  upper_link <- estimate_link + z * se[sel_terms]
  transform_link <- if (isTRUE(exponentiate)) exp else identity

  data.frame(
    term = sel_terms,
    estimate = as.numeric(transform_link(estimate_link)),
    lower = as.numeric(transform_link(lower_link)),
    upper = as.numeric(transform_link(upper_link)),
    p = as.numeric(2 * stats::pnorm(-abs(coef_vec[sel_terms] / se[sel_terms])))
  )
}

#########
# GENERALIZED LINEAR MODEL EFFECT ESTIMATION
# Model formula construction, initialization heuristics, and convergence monitoring.

#' Fit a GLM effect table with shared robust Wald intervals
#'
#' Internal primitive used by descriptive and GLM engines. It returns raw
#' numerics only; presentation methods own all rounding and string formatting.
#'
#' @param data Data frame containing the outcome and predictors.
#' @param outcome Outcome column name.
#' @param focus Optional focal predictor column name.
#' @param covars Optional covariate column names.
#' @param family A `stats::family` object for `glm()`.
#' @param vcov One of `"HC0"`, `"HC1"`, `"HC2"`, `"HC3"`, `"none"`, or
#'   `"model"`.
#' @param conf.level Confidence level.
#' @param ref Optional reference level for binary outcomes or the focal
#'   categorical predictor.
#' @param exponentiate Whether to exponentiate estimates and intervals.
#' @param include_intercept Whether to include the intercept row.
#' @param model_na Missing-data policy for model variables: `"drop"`,
#'   `"fail"`, or `"explicit"`.
#' @return A data frame with columns `term`, `level`, `estimate`, `lower`,
#'   `upper`, and `p`.
#' @keywords internal
#' @noRd
.fit_glm_effect <- function(data,
                            outcome,
                            focus = NULL,
                            covars = NULL,
                            family,
                            vcov = "HC0",
                            conf.level = 0.95,
                            ref = NULL,
                            exponentiate = TRUE,
                            include_intercept = FALSE,
                            model_na = c("drop", "fail", "explicit")) {
  empty_out <- function() {
    data.frame(
      term = character(),
      level = character(),
      estimate = numeric(),
      lower = numeric(),
      upper = numeric(),
      p = numeric()
    )
  }
  model_na <- match.arg(model_na)

  if (
    missing(family) ||
      !is.data.frame(data) ||
      !is.character(outcome) || length(outcome) != 1 ||
      !outcome %in% names(data)
  ) {
    return(empty_out())
  }

  predictors <- unique(c(focus, covars))
  predictors <- predictors[!is.na(predictors) & nzchar(predictors)]
  if (!all(predictors %in% names(data))) {
    return(empty_out())
  }

  if (!is.null(focus) && (!is.character(focus) || length(focus) != 1 || !focus %in% names(data))) {
    return(empty_out())
  }
  if (!is.numeric(conf.level) || length(conf.level) != 1 || conf.level <= 0 || conf.level >= 1) {
    return(empty_out())
  }
  if (!is.character(vcov) || length(vcov) != 1 || !vcov %in% c("HC0", "HC1", "HC2", "HC3", "none", "model")) {
    return(empty_out())
  }

  d2 <- data
  model_vars <- unique(c(outcome, predictors))
  if (identical(model_na, "fail") &&
      any(!stats::complete.cases(d2[, model_vars, drop = FALSE]))) {
    offending <- model_vars[vapply(d2[, model_vars, drop = FALSE], anyNA, logical(1))]
    simtab_abort_engine(c(
      "Adjusted model variable(s) contain missing values under
       {.code na_model = \"fail\"}: {.val {offending}}.",
      "i" = "{.val fail} asks SimtablR to stop rather than silently drop rows.",
      "v" = "Use {.code na_model = \"drop\"} for listwise deletion, or
             {.val explicit} to model missingness as its own category."
    ))
  }
  # Model semantics use the strict numeric-means-continuous rule: the
  # descriptive low-cardinality heuristic must never silently turn a numeric
  # model term (e.g. a 0/1 or dose-coded predictor) into a factor.
  if (identical(model_na, "explicit")) {
    for (var in predictors) {
      if (!identical(.detect_var_type(d2[[var]], strict = TRUE), "continuous")) {
        d2[[var]] <- .explicit_missing_factor(d2[[var]])
      }
    }
  }

  focus_ref_matches <- FALSE
  if (!is.null(focus) && .detect_var_type(d2[[focus]], strict = TRUE) != "continuous") {
    focus_ref_matches <- !is.null(ref) && as.character(ref) %in% levels(factor(d2[[focus]]))
  }

  y <- d2[[outcome]]
  if (is.factor(y) || is.character(y) || is.logical(y)) {
    yf <- droplevels(factor(y))
    if (nlevels(yf) != 2) {
      return(empty_out())
    }
    ylev <- levels(yf)
    event_lev <- ylev[2]
    if (!focus_ref_matches && !is.null(ref) && as.character(ref) %in% ylev) {
      event_lev <- setdiff(ylev, as.character(ref))[1]
    }
    d2$.simtab_y <- as.integer(yf == event_lev)
  } else {
    d2$.simtab_y <- as.numeric(y)
  }

  focus_is_continuous <- FALSE
  if (!is.null(focus)) {
    focus_is_continuous <- .detect_var_type(d2[[focus]], strict = TRUE) == "continuous"
    if (!focus_is_continuous) {
      pf <- factor(d2[[focus]])
      if (!is.null(ref) && as.character(ref) %in% levels(pf)) {
        pf <- stats::relevel(pf, ref = as.character(ref))
      }
      d2[[focus]] <- droplevels(pf)
    }
  }

  form <- stats::reformulate(predictors, response = ".simtab_y")
  glm_warning <- FALSE
  glm_error <- NULL
  start_used <- FALSE

  fit_once <- function(start = NULL) {
    tryCatch(
      withCallingHandlers(
        if (is.null(start)) {
          stats::glm(form, family = family, data = d2)
        } else {
          stats::glm(form, family = family, data = d2, start = start)
        },
        warning = function(w) {
          glm_warning <<- TRUE
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        glm_error <<- e
        NULL
      }
    )
  }

  fit <- fit_once()

  # A log-link binomial model has no usable default initialization in
  # stats::glm(): the identity start puts fitted probabilities outside (0, 1]
  # and R aborts before the first IRLS step with "no valid set of coefficients".
  # That is an initialization failure, not evidence that the model does not
  # converge. Seeding the intercept at the marginal log risk and the slopes at
  # zero is the conventional remedy and is always inside the parameter space.
  #
  # The retry is deliberately narrow. It fires only when `glm()` produced no
  # fit at all, so a model that already fits keeps its exact numerical path and
  # its estimates are untouched; only the previously unrecoverable case moves.
  if (is.null(fit)) {
    start_vals <- .log_binomial_start(form, d2, family)
    if (!is.null(start_vals)) {
      init_error <- glm_error
      glm_error <- NULL
      fit <- fit_once(start = start_vals)
      if (is.null(fit)) {
        glm_error <- init_error
      } else {
        start_used <- TRUE
      }
    }
  }
  if (is.null(fit)) {
    out <- empty_out()
    attr(out, "converged") <- FALSE
    attr(out, "boundary") <- NA
    attr(out, "glm_warning") <- glm_warning
    attr(out, "glm_error") <- glm_error
    attr(out, "start_used") <- start_used
    return(out)
  }
  coef_names <- tryCatch(names(stats::coef(fit)), error = function(e) character())

  terms_sel <- character()
  levels_sel <- character()

  if (isTRUE(include_intercept) && "(Intercept)" %in% coef_names) {
    terms_sel <- "(Intercept)"
    levels_sel <- NA_character_
  }

  if (is.null(focus)) {
    model_terms <- coef_names
    if (!isTRUE(include_intercept)) {
      model_terms <- setdiff(model_terms, "(Intercept)")
    }
    terms_sel <- model_terms
    levels_sel <- rep(NA_character_, length(terms_sel))
  } else if (focus_is_continuous) {
    terms_sel <- c(terms_sel, focus)
    levels_sel <- c(levels_sel, NA_character_)
  } else {
    plevs <- levels(d2[[focus]])
    terms_sel <- c(terms_sel, paste0(focus, plevs[-1]))
    levels_sel <- c(levels_sel, plevs[-1])
  }

  keep <- terms_sel %in% coef_names
  terms_sel <- terms_sel[keep]
  levels_sel <- levels_sel[keep]
  if (length(terms_sel) == 0) {
    return(empty_out())
  }

  out <- .robust_wald_ci(
    fit,
    vcov = vcov,
    conf.level = conf.level,
    exponentiate = exponentiate,
    include_intercept = include_intercept,
    terms = terms_sel
  )
  if (nrow(out) == 0) {
    return(empty_out())
  }

  out$level <- levels_sel[match(out$term, terms_sel)]
  out <- out[c("term", "level", "estimate", "lower", "upper", "p")]
  attr(out, "model_n") <- as.integer(stats::nobs(fit))
  attr(out, "converged") <- isTRUE(fit$converged) && !isTRUE(fit$boundary)
  attr(out, "boundary") <- isTRUE(fit$boundary)
  attr(out, "glm_warning") <- glm_warning
  attr(out, "glm_error") <- glm_error
  attr(out, "start_used") <- start_used
  out
}

#' Conventional starting values for a log-link binomial model
#'
#' `stats::glm()` has no usable default initialization for `binomial("log")`:
#' it aborts before the first iteration whenever the identity start implies a
#' fitted probability outside `(0, 1]`. Seeding the intercept at the marginal
#' log risk and every slope at zero is inside the parameter space by
#' construction, so the fit begins at a legal point and the reported
#' convergence status describes the model rather than the initialization.
#'
#' @param form Model formula with response `.simtab_y`.
#' @param data Model frame containing `.simtab_y` and the predictors.
#' @param family The GLM family being fitted.
#' @return A numeric starting vector, or `NULL` when the family is not
#'   log-link binomial or starting values cannot be derived.
#' @keywords internal
#' @noRd
.log_binomial_start <- function(form, data, family) {
  if (!is.list(family) ||
      !identical(family$family, "binomial") ||
      !identical(family$link, "log")) {
    return(NULL)
  }
  y <- data[[".simtab_y"]]
  if (is.null(y)) {
    return(NULL)
  }
  y <- y[!is.na(y)]
  if (length(y) == 0) {
    return(NULL)
  }
  risk <- mean(y)
  # A degenerate marginal risk gives log(0) or log(1) = 0; neither seeds a
  # usable fit, so defer to the unseeded attempt.
  if (!is.finite(risk) || risk <= 0 || risk >= 1) {
    return(NULL)
  }
  n_coef <- tryCatch(
    ncol(stats::model.matrix(form, data)),
    error = function(e) NA_integer_
  )
  if (!is.finite(n_coef) || n_coef < 1) {
    return(NULL)
  }
  c(log(risk), rep(0, n_coef - 1))
}

#########
# PREVALENCE RATIO DUAL-MACHINE PIPELINE
# Log-binomial estimation with automatic robust Poisson fallback upon non-convergence.

#' Fit prevalence ratio model with log-binomial to robust Poisson fallback
#' @keywords internal
#' @noRd
.fit_pr_effect <- function(data,
                           outcome,
                           focus,
                           covars = character(),
                           conf.level = 0.95,
                           ref = NULL,
                           exponentiate = TRUE,
                           include_intercept = FALSE,
                           model_na = c("drop", "fail", "explicit")) {
  model_na <- match.arg(model_na)
  lb <- tryCatch(
    .fit_glm_effect(
      data,
      outcome = outcome,
      focus = focus,
      covars = covars,
      family = stats::binomial("log"),
      vcov = "model",
      conf.level = conf.level,
      ref = ref,
      exponentiate = exponentiate,
      include_intercept = include_intercept,
      model_na = model_na
    ),
    error = function(e) NULL
  )

  logbinomial_converged <- !is.null(lb) && isTRUE(attr(lb, "converged", exact = TRUE))
  logbinomial_boundary <- if (is.null(lb)) {
    NA
  } else {
    attr(lb, "boundary", exact = TRUE) %||% NA
  }
  # Separate the two ways a log-binomial attempt can end without an estimate.
  # A model that was fitted and failed to converge is evidence about the data;
  # a model that never began because `glm()` errored is evidence about the fit
  # itself. Reporting the second as the first misdescribes the analysis.
  logbinomial_status <- if (logbinomial_converged) {
    "converged"
  } else if (is.null(lb) || !is.null(attr(lb, "glm_error", exact = TRUE))) {
    "failed_to_fit"
  } else {
    "not_converged"
  }

  if (logbinomial_converged) {
    attr(lb, "estimator_used") <- "log-binomial"
    attr(lb, "logbinomial_fallback") <- FALSE
    attr(lb, "logbinomial_converged") <- TRUE
    attr(lb, "logbinomial_boundary") <- as.logical(logbinomial_boundary)
    attr(lb, "logbinomial_status") <- logbinomial_status
    return(lb)
  }

  out <- .fit_glm_effect(
    data,
    outcome = outcome,
    focus = focus,
    covars = covars,
    family = stats::poisson("log"),
    vcov = "HC0",
    conf.level = conf.level,
    ref = ref,
    exponentiate = exponentiate,
    include_intercept = include_intercept,
    model_na = model_na
  )
  attr(out, "estimator_used") <- "robust-poisson"
  attr(out, "logbinomial_fallback") <- TRUE
  attr(out, "logbinomial_converged") <- FALSE
  attr(out, "logbinomial_boundary") <- as.logical(logbinomial_boundary)
  attr(out, "logbinomial_status") <- logbinomial_status
  out
}

#' Encode missing values as an explicit categorical factor level
#' @keywords internal
#' @noRd
.explicit_missing_factor <- function(x, missing_level = "(Missing)") {
  vals <- as.character(x)
  base_levels <- levels(factor(x))
  if (!anyNA(vals)) {
    return(factor(vals, levels = base_levels))
  }
  if (missing_level %in% vals[!is.na(vals)]) {
    simtab_abort_engine(c(
      "Model predictor already contains the explicit missing level {.val {missing_level}}.",
      "i" = "{.code model = \"explicit\"} adds that label as a new category, which
             would silently merge with the observed one.",
      "v" = "Rename the observed level so the missing-data category stays distinct."
    ))
  }
  vals[is.na(vals)] <- missing_level
  factor(vals, levels = unique(c(base_levels, missing_level)))
}
