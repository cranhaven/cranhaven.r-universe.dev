# GENERALIZED LINEAR MODEL STATISTICAL ENGINE
# Multi-outcome regression engine supporting GLM and Firth penalized logistic regression.
# Computes raw coefficients, sandwich covariance, Wald intervals, GVIF diagnostics, and model metadata.

#########
# COLLINEARITY AND GVIF DIAGNOSTICS
# Generalized variance inflation factors (GVIF) and term-level collinearity assessment.

#' Generate empty data frame structure for GVIF collinearity diagnostics
#' @keywords internal
#' @noRd
.empty_glm_vif <- function() {
  data.frame(
    term = character(),
    gvif = numeric(),
    df = integer(),
    gvif_adjusted = numeric(),
    vif = numeric()
  )
}

#' Compute generalized variance inflation factors across predictor terms
#' @keywords internal
#' @noRd
.glm_vif <- function(fit) {
  coef_vec <- stats::coef(fit)
  if (any(is.na(coef_vec))) {
    return(.empty_glm_vif())
  }

  mm <- stats::model.matrix(fit)
  assign <- attr(mm, "assign")
  vc <- stats::vcov(fit)
  coef_names <- names(coef_vec)
  if (length(coef_names) == 0 || length(assign) == 0) {
    return(.empty_glm_vif())
  }

  if (identical(coef_names[[1]], "(Intercept)")) {
    if (nrow(vc) <= 1 || length(assign) <= 1) {
      return(.empty_glm_vif())
    }
    vc <- vc[-1, -1, drop = FALSE]
    assign <- assign[-1]
  }

  terms <- labels(stats::terms(fit))
  n_terms <- length(terms)
  if (n_terms < 2 || nrow(vc) < 2) {
    return(.empty_glm_vif())
  }

  R <- stats::cov2cor(vc)
  det_R <- det(R)
  if (!is.finite(det_R) || abs(det_R) < .Machine$double.eps) {
    return(.empty_glm_vif())
  }

  out <- .empty_glm_vif()
  for (term_idx in seq_len(n_terms)) {
    subs <- which(assign == term_idx)
    if (length(subs) == 0) {
      next
    }
    gvif <- det(as.matrix(R[subs, subs, drop = FALSE])) *
      det(as.matrix(R[-subs, -subs, drop = FALSE])) / det_R
    df <- length(subs)
    adjusted <- gvif^(1 / (2 * df))
    out <- rbind(
      out,
      data.frame(
        term = terms[[term_idx]],
        gvif = as.numeric(gvif),
        df = as.integer(df),
        gvif_adjusted = as.numeric(adjusted),
        vif = as.numeric(adjusted^2)
      )
    )
  }

  rownames(out) <- NULL
  out
}

#' Map term-level GVIF values to regression model effect rows
#' @keywords internal
#' @noRd
.glm_vif_for_effects <- function(fit, outcome, effect_terms) {
  out <- data.frame(
    outcome = outcome,
    term = effect_terms,
    vif_term = NA_character_,
    gvif = NA_real_,
    vif_df = NA_integer_,
    gvif_adjusted = NA_real_,
    vif = NA_real_
  )

  vif <- .glm_vif(fit)
  if (nrow(vif) == 0 || length(effect_terms) == 0) {
    return(out)
  }

  mm <- stats::model.matrix(fit)
  assign <- attr(mm, "assign")
  mm_names <- colnames(mm)
  coef_names <- names(stats::coef(fit))
  if (identical(coef_names[[1]], "(Intercept)")) {
    mm_names <- mm_names[-1]
    assign <- assign[-1]
  }
  term_labels <- labels(stats::terms(fit))
  coef_to_term <- stats::setNames(term_labels[assign], mm_names)

  mapped <- unname(coef_to_term[effect_terms])
  matched <- match(mapped, vif$term)
  ok <- !is.na(matched)
  out$vif_term[ok] <- mapped[ok]
  out$gvif[ok] <- vif$gvif[matched[ok]]
  out$vif_df[ok] <- vif$df[matched[ok]]
  out$gvif_adjusted[ok] <- vif$gvif_adjusted[matched[ok]]
  out$vif[ok] <- vif$vif[matched[ok]]
  out
}

#' Generate empty GVIF data frame placeholder for effect terms
#' @keywords internal
#' @noRd
.glm_empty_vif_for_terms <- function(effect_terms) {
  data.frame(
    vif_term = rep(NA_character_, length(effect_terms)),
    gvif = rep(NA_real_, length(effect_terms)),
    vif_df = rep(NA_integer_, length(effect_terms)),
    gvif_adjusted = rep(NA_real_, length(effect_terms)),
    vif = rep(NA_real_, length(effect_terms))
  )
}

#########
# MODEL EVIDENCE AND PENALIZED REGRESSION PRIMITIVES
# Extracts link-scale evidence and manages Firth profile penalized estimation.

#' Retain link-scale coefficients, covariance, and terms for model accessors
#' @keywords internal
#' @noRd
.glm_model_evidence <- function(
    fit,
    vcov = "none",
    formula = NULL,
    n = NULL,
    interval = NULL,
    interval_method = "wald",
    conf.level = 0.95
) {
  coefficients <- tryCatch(stats::coef(fit), error = function(e) NULL)
  covariance <- .glm_covariance(fit, vcov)
  model_matrix <- tryCatch(stats::model.matrix(fit), error = function(e) NULL)
  fit_terms <- tryCatch(stats::terms(fit), error = function(e) NULL)
  if (is.null(coefficients) || is.null(covariance)) {
    return(NULL)
  }

  coefficient_terms <- stats::setNames(
    rep(NA_character_, length(coefficients)),
    names(coefficients)
  )
  if (!is.null(model_matrix) && !is.null(fit_terms)) {
    assignment <- attr(model_matrix, "assign")
    term_labels <- attr(fit_terms, "term.labels")
    mapped_terms <- rep(NA_character_, ncol(model_matrix))
    mapped_terms[assignment == 0L] <- "(Intercept)"
    assigned <- assignment > 0L & assignment <= length(term_labels)
    mapped_terms[assigned] <- term_labels[assignment[assigned]]
    names(mapped_terms) <- colnames(model_matrix)
    coefficient_terms <- mapped_terms[names(coefficients)]
  }

  if (is.null(formula)) {
    formula <- tryCatch(stats::formula(fit), error = function(e) NULL)
  }
  if (is.null(n)) {
    n <- tryCatch(stats::nobs(fit), error = function(e) NA_integer_)
  }

  list(
    coefficients = coefficients,
    covariance = covariance,
    coefficient_terms = coefficient_terms,
    formula = formula,
    n = as.integer(n),
    interval = interval,
    interval_method = interval_method,
    conf.level = conf.level
  )
}

#' Stop when the complete-case outcome takes a single value
#'
#' A constant response has no variation to model: glm() "fits" it with a
#' diverging intercept and reports slopes of exactly 1 with degenerate
#' intervals. The error is caught by the per-outcome handler, which records the
#' outcome as failed.
#' @keywords internal
#' @noRd
.glm_check_outcome_varies <- function(formula, data, outcome) {
  model_frame <- stats::model.frame(formula, data = data, na.action = stats::na.omit)
  y <- stats::model.response(model_frame)
  n_values <- length(unique(y[!is.na(y)]))
  if (n_values < 2L) {
    simtab_abort_engine(c(
      "Outcome {.val {outcome}} takes a single value among complete cases.",
      "i" = "A constant outcome has no variation to model, so no effect is estimable.",
      "v" = "Check the outcome coding, or drop it from {.arg outcomes}."
    ))
  }
  invisible(TRUE)
}

#' Count outcome events from model response frame for Firth regression
#' @keywords internal
#' @noRd
.glm_firth_event_count <- function(model_frame) {
  y <- stats::model.response(model_frame)
  if (is.factor(y) || is.character(y) || is.logical(y)) {
    fy <- droplevels(factor(y))
    if (nlevels(fy) != 2) {
      return(NA_integer_)
    }
    return(as.integer(sum(fy == levels(fy)[[2]], na.rm = TRUE)))
  }
  vals <- y[!is.na(y)]
  if (length(vals) == 0) {
    return(NA_integer_)
  }
  if (all(vals %in% c(0, 1))) {
    return(as.integer(sum(vals == 1)))
  }
  as.integer(sum(vals != 0))
}

#' Extract profile penalized confidence intervals from Firth fit
#' @keywords internal
#' @noRd
.glm_firth_ci <- function(fit) {
  ci <- tryCatch(stats::confint(fit), error = function(e) NULL)
  if (is.null(ci)) {
    lower <- fit$ci.lower %||% rep(NA_real_, length(stats::coef(fit)))
    upper <- fit$ci.upper %||% rep(NA_real_, length(stats::coef(fit)))
    ci <- cbind(lower, upper)
    rownames(ci) <- names(stats::coef(fit))
  }
  ci
}

#' Assess convergence criteria for Firth penalized regression fits
#' @keywords internal
#' @noRd
.glm_firth_converged <- function(fit) {
  conv <- fit$conv
  control <- fit$control
  if (is.null(conv) || is.null(control)) {
    return(FALSE)
  }
  checks <- c(
    abs(conv[["LL change"]]) <= control$lconv,
    abs(conv[["max abs score"]]) <= control$gconv,
    abs(conv[["beta change"]]) <= control$xconv
  )
  length(checks) == 3 && all(is.finite(checks)) && all(checks)
}

#' Assemble effect estimate data frame from Firth penalized logistic fit
#' @keywords internal
#' @noRd
.glm_firth_effects <- function(fit, conf.level = 0.95, exponentiate = TRUE, include_intercept = FALSE) {
  coef_vec <- stats::coef(fit)
  if (!isTRUE(include_intercept)) {
    coef_vec <- coef_vec[setdiff(names(coef_vec), "(Intercept)")]
  }
  if (length(coef_vec) == 0) {
    return(data.frame(
      term = character(), estimate = numeric(), lower = numeric(), upper = numeric(), p = numeric()
    ))
  }

  ci <- .glm_firth_ci(fit)
  ci <- ci[names(coef_vec), , drop = FALSE]
  p <- fit$prob[names(coef_vec)] %||% rep(NA_real_, length(coef_vec))
  transform_link <- if (isTRUE(exponentiate)) exp else identity

  data.frame(
    term = names(coef_vec),
    estimate = as.numeric(transform_link(coef_vec)),
    lower = as.numeric(transform_link(ci[, 1])),
    upper = as.numeric(transform_link(ci[, 2])),
    p = as.numeric(p)
  )
}

#########
# SPECIFICATION VALIDATION AND GLM ENGINE DISPATCH
# Validates offset constraints and orchestrates multi-outcome GLM fitting loops.

#' Validate offset role bindings and numerical constraints in GLM specification
#' @keywords internal
#' @noRd
.validate_glm <- function(spec) {
  cfg <- spec$engine_opts$glm %||% list()
  offset_col <- .resolve_single_role(spec, "offset")
  if (is.null(offset_col)) {
    return(invisible(spec))
  }
  data <- spec$data_src$ref$data
  if (!offset_col %in% names(data)) {
    simtab_abort_engine(c(
      "The {.val offset} role must resolve to a column present in data.",
      "i" = sprintf("'%s' was not found.", offset_col),
      "v" = "Use {.code regtab(data, ..., offset = person_time)}."
    ))
  }
  vals <- data[[offset_col]]
  if (!is.numeric(vals)) {
    simtab_abort_engine(c(
      "The {.val offset} column must be numeric.",
      "i" = sprintf("'%s' is not numeric.", offset_col),
      "v" = "Supply a numeric person-time column, e.g. follow-up days or years."
    ))
  }
  if (any(vals[!is.na(vals)] <= 0)) {
    simtab_abort_engine(c(
      "The {.val offset} column must be strictly positive.",
      "i" = "Person-time cannot be zero or negative; log(offset) is undefined otherwise.",
      "v" = "Check for zero-follow-up rows before fitting a rate model."
    ))
  }
  invisible(spec)
}

#' Execute multi-outcome GLM regression engine across specified endpoints
#' @keywords internal
#' @noRd
.engine_glm <- function(spec, data) {
  spec <- validate_simtab_spec(spec)
  cfg <- spec$engine_opts$glm
  if (is.null(cfg)) {
    simtab_abort_spec(c(
      "GLM specifications require a {.code glm} engine-options block.",
      "i" = "The {.val glm} engine was selected without its configuration.",
      "v" = "Build the specification through {.fn regtab} rather than by hand."
    ))
  }

  cfg$predictors <- .formula_with_adjust(cfg$predictors, spec)
  conf.level <- cfg$conf.level %||% 0.95
  method <- cfg$method %||% "glm"
  if (!method %in% c("glm", "firth")) {
    simtab_abort_spec("GLM method must be one of {.val glm} or {.val firth}.")
  }
  if (identical(method, "firth")) {
    if (!identical(cfg$family$family, "binomial") || !identical(cfg$family$link, "logit")) {
      simtab_abort_spec(c(
        "{.code method = \"firth\"} is available only for binomial(logit) models.",
        "i" = "Requested: {.val {cfg$family$family}}({.val {cfg$family$link}}).",
        "v" = "Use {.code family = binomial(\"logit\")}, or drop {.arg method}."
      ))
    }
    .require_pkg("logistf", "method = \"firth\"")
  }
  robust_opt <- cfg$robust
  vcov_method <- if (
    identical(method, "firth")
  ) {
    "profile"
  } else if (
    is.character(robust_opt) && length(robust_opt) == 1 &&
      robust_opt %in% c("HC0", "HC1", "HC2", "HC3", "none")
  ) {
    robust_opt
  } else if (isTRUE(robust_opt)) {
    "HC0"
  } else {
    "none"
  }
  robust <- !identical(vcov_method, "none") && !identical(method, "firth")
  exponentiate <- cfg$exponentiate
  if (is.null(exponentiate)) {
    exponentiate <- cfg$family$family %in% c("poisson", "binomial", "quasipoisson", "quasibinomial")
  }
  predictor_terms <- labels(stats::terms(cfg$predictors))
  offset_col <- .resolve_single_role(spec, "offset")
  predictor_rhs <- cfg$predictors[[length(cfg$predictors)]]
  model_rhs <- if (is.null(offset_col)) {
    predictor_rhs
  } else {
    call("+", predictor_rhs, call("offset", call("log", as.name(offset_col))))
  }

  outcome_results <- list()
  model_evidence <- list()
  term_order <- character(0)
  model_rows <- vector("list", length(cfg$outcomes))

  for (i in seq_along(cfg$outcomes)) {
    outcome <- cfg$outcomes[[i]]
    model_n <- NA_integer_
    converged <- NA
    boundary <- NA
    failed <- FALSE
    error_msg <- NA_character_
    dispersion <- NA_real_
    events <- NA_integer_
    fit_evidence <- NULL

    fit_out <- tryCatch(
      {
        full_formula <- stats::as.formula(
          call("~", as.name(outcome), model_rhs),
          env = environment(cfg$predictors)
        )
        .glm_check_outcome_varies(full_formula, data, outcome)
        if (identical(method, "firth")) {
          fit <- logistf::logistf(full_formula, data = data, pl = TRUE, alpha = 1 - conf.level)
          converged <- .glm_firth_converged(fit)
          boundary <- NA
          if (!converged) {
            warning(
              sprintf("Firth model for '%s' did not converge. Results may be unreliable.", outcome),
              call. = FALSE,
              immediate. = TRUE
            )
          }
          model_frame <- stats::model.frame(full_formula, data = data, na.action = stats::na.omit)
          events <- .glm_firth_event_count(model_frame)
          effect_df <- .glm_firth_effects(
            fit,
            conf.level = conf.level,
            exponentiate = exponentiate,
            include_intercept = isTRUE(cfg$include_intercept)
          )
          model_n <- nrow(model_frame)
          vif_df <- .glm_empty_vif_for_terms(effect_df$term)
          fit_evidence <- .glm_model_evidence(
            fit,
            vcov = "model",
            formula = full_formula,
            n = model_n,
            interval = .glm_firth_ci(fit),
            interval_method = "profile",
            conf.level = conf.level
          )
        } else {
          fit <- suppressWarnings(stats::glm(full_formula, family = cfg$family, data = data))
          boundary <- isTRUE(fit$boundary)
          converged <- isTRUE(fit$converged) && !boundary
          if (!converged) {
            warning(
              sprintf(
                "Model for '%s' %s. Results may be unreliable.",
                outcome,
                if (boundary) "reached a boundary" else "did not converge"
              ),
              call. = FALSE,
              immediate. = TRUE
            )
          }
          if (identical(cfg$family$family, "poisson") && stats::df.residual(fit) > 0) {
            dispersion <- sum(stats::residuals(fit, type = "pearson")^2) / stats::df.residual(fit)
          }

          effect_df <- .robust_wald_ci(
            fit,
            vcov = vcov_method,
            conf.level = conf.level,
            exponentiate = exponentiate,
            include_intercept = isTRUE(cfg$include_intercept)
          )
          model_n <- stats::nobs(fit)
          fit_evidence <- .glm_model_evidence(
            fit,
            vcov = vcov_method,
            formula = full_formula,
            n = model_n,
            conf.level = conf.level
          )
          events <- if (identical(cfg$family$family, "binomial")) {
            .glm_firth_event_count(stats::model.frame(fit))
          } else {
            NA_integer_
          }
          vif_df <- .glm_vif_for_effects(fit, outcome, effect_df$term)
        }
        if (nrow(effect_df) == 0) {
          simtab_abort_engine(c(
            "No estimable coefficients returned for outcome {.val {outcome}}.",
            "i" = "Every model term was dropped, usually from perfect collinearity
                   or a predictor with a single observed value.",
            "v" = "Check the predictors for constant or duplicated columns."
          ))
        }

        effect_df$outcome <- outcome
        effect_df <- cbind(
          effect_df,
          vif_df[c("vif_term", "gvif", "vif_df", "gvif_adjusted", "vif")]
        )
        effect_df <- effect_df[c(
          "outcome", "term", "estimate", "lower", "upper", "p",
          "vif_term", "gvif", "vif_df", "gvif_adjusted", "vif"
        )]
        effect_df
      },
      error = function(e) {
        failed <<- TRUE
        error_msg <<- paste0("Model fitting failed for outcome '", outcome, "': ", conditionMessage(e))
        warning(error_msg, call. = FALSE, immediate. = TRUE)
        NULL
      }
    )

    if (!is.null(fit_out)) {
      outcome_results[[outcome]] <- fit_out
      if (!is.null(fit_evidence)) {
        model_evidence[[outcome]] <- fit_evidence
      }
      term_order <- unique(c(term_order, fit_out$term))
    }

    model_rows[[i]] <- data.frame(
      outcome = outcome,
      n = as.integer(model_n),
      family = cfg$family$family,
      link = cfg$family$link,
      robust = robust,
      vcov = vcov_method,
      method = method,
      dispersion = dispersion,
      events = as.integer(events %||% NA_integer_),
      exponentiate = isTRUE(exponentiate),
      converged = as.logical(converged),
      boundary = as.logical(boundary),
      failed = failed,
      error = error_msg
    )
  }

  model_info <- do.call(rbind, model_rows)
  rownames(model_info) <- NULL
  if (length(outcome_results) == 0) {
    simtab_abort_engine(c(
      "All models failed to fit.",
      "i" = paste(model_info$error, collapse = " "),
      "v" = "Check the named outcome and model specification, then try again."
    ))
  }

  data_out <- do.call(rbind, unname(outcome_results))
  rownames(data_out) <- NULL
  successful <- model_info[!model_info$failed, , drop = FALSE]

  list(
    data = data_out,
    meta = list(
      engine = "glm",
      family = cfg$family$family,
      link = cfg$family$link,
      robust = robust,
      vcov = vcov_method,
      method = method,
      estimator = method,
      exponentiate = isTRUE(exponentiate),
      conf.level = conf.level,
      conf_pct = round(conf.level * 100),
      include_intercept = isTRUE(cfg$include_intercept),
      p_values = isTRUE(cfg$p_values),
      d = as.integer(cfg$d),
      outcomes = cfg$outcomes,
      labels = cfg$labels,
      predictor_labels = cfg$predictor_labels,
      predictor_terms = predictor_terms,
      term_order = term_order,
      offset = offset_col,
      model_n = stats::setNames(as.integer(successful$n), successful$outcome),
      model_info = model_info,
      model_evidence = model_evidence,
      n_succeeded = sum(!model_info$failed),
      n_failed = sum(model_info$failed)
    )
  )
}
