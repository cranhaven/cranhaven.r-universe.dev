# COX PROPORTIONAL HAZARDS STATISTICAL ENGINE
# Fits semiparametric survival models using survival::coxph() and tests proportional hazards.
# Computes hazard ratios, Wald confidence limits, concordance, and Schoenfeld residual diagnostics.

#########
# SURVIVAL ROLE AND FORMULA HELPERS
# Event status coercion, engine argument extraction, and Surv() formula construction.

#' Validate and coerce survival event indicator to integer 0/1 vector
#' @keywords internal
#' @noRd
.survival_coerce_event <- function(x) {
  # An all-missing column passes the 0/1 membership test vacuously
  # (`all(logical(0))` is TRUE) and then fails inside survfit/coxph with a base-R
  # message. Reject it here so the caller gets a classed binding error instead.
  if (all(is.na(x))) {
    simtab_abort_binding(c(
      "{.arg event} must record at least one non-missing status.",
      "i" = "Every value in the event column is missing.",
      "v" = "Check the event indicator: 1 (or the second factor level) marks the event, 0 marks censoring."
    ))
  }
  if (is.logical(x)) {
    return(list(event = as.integer(x), note = "logical TRUE treated as event"))
  }
  if (is.numeric(x) && all(stats::na.omit(x) %in% c(0, 1))) {
    return(list(event = as.integer(x), note = "numeric 1 treated as event"))
  }
  if (is.factor(x) && length(levels(droplevels(x))) == 2) {
    levs <- levels(droplevels(x))
    return(list(
      event = as.integer(x == levs[[2]]),
      note = sprintf("factor level '%s' treated as event", levs[[2]])
    ))
  }
  simtab_abort_binding(c(
    "{.arg event} must be logical, numeric 0/1, or a two-level factor.",
    "i" = "Received a column of class {.cls {class(x)[[1]]}}.",
    "v" = "Recode the indicator so 1 (or the second factor level) marks the event."
  ))
}

#' Extract Cox engine execution options from specification object
#' @keywords internal
#' @noRd
.cox_args_from_spec <- function(spec) {
  opts <- spec$engine_opts$cox %||% list()
  list(
    time = .resolve_single_role(spec, "time"),
    event = .resolve_single_role(spec, "event"),
    predictors = .formula_with_adjust(opts$predictors %||% NULL, spec),
    # stratify() on a Cox spec or result fits a stratified model: separate
    # baseline hazards per stratum, common hazard ratios.
    strata = .resolve_single_role(spec, "by"),
    conf.level = opts$conf.level %||% 0.95,
    d = opts$d %||% 2L,
    ties = opts$ties %||% "efron"
  )
}

#' Construct survival formula with Surv() response and predictor terms
#' @keywords internal
#' @noRd
.cox_formula <- function(time, event, predictors, strata = NULL) {
  rhs_expr <- predictors[[length(predictors)]]
  if (!is.null(strata)) {
    # A stratification variable gets its own baseline hazard, so it cannot
    # also be a covariate; drop it from the linear predictor.
    rhs_terms <- setdiff(labels(stats::terms(predictors)), strata)
    if (length(rhs_terms) == 0) {
      simtab_abort_spec(c(
        "Stratifying by {.val {strata}} leaves no predictor in the Cox model.",
        "i" = "A stratification variable has its own baseline hazard and no hazard ratio.",
        "v" = "Stratify by a different variable, or add another predictor."
      ))
    }
    rhs_expr <- str2lang(paste(c(rhs_terms, sprintf("survival::strata(%s)", strata)), collapse = " + "))
  }
  rhs <- paste(deparse(rhs_expr), collapse = " ")
  stats::as.formula(
    sprintf("survival::Surv(%s, %s) ~ %s", time, event, rhs),
    env = environment(predictors)
  )
}

#########
# COX MODEL ENGINE DISPATCH
# Fits coxph model, extracts hazard ratios, concordance, and Grambsch-Therneau diagnostics.

#' Execute Cox proportional hazards regression engine
#' @keywords internal
#' @noRd
.engine_cox <- function(spec, data) {
  .require_pkg("survival", "survtab()")
  args <- .cox_args_from_spec(spec)
  event_info <- .survival_coerce_event(data[[args$event]])
  fit_data <- data
  fit_data[[args$event]] <- event_info$event

  # A Cox model needs at least one event: with none, coxph() returns all-NA
  # coefficients without complaining and cox.zph() then fails deep inside
  # survival with an error that names this function's locals.
  n_events <- sum(event_info$event, na.rm = TRUE)
  if (!is.finite(n_events) || n_events < 1) {
    simtab_abort_engine(c(
      "A Cox model requires at least one event.",
      "i" = "Column {.val {args$event}} records {n_events} event(s) across {nrow(data)} row(s).",
      "v" = "Check the event coding (1 or the second factor level marks the event), or describe an all-censored cohort with {.fn table1}."
    ))
  }

  formula <- .cox_formula(args$time, args$event, args$predictors, args$strata)
  fit <- survival::coxph(formula, data = fit_data, ties = args$ties, x = TRUE)
  fit_summary <- summary(fit, conf.int = args$conf.level)
  coef <- as.data.frame(fit_summary$coefficients)
  ci <- as.data.frame(fit_summary$conf.int)
  # The proportional-hazards diagnostic is supporting evidence, not the estimate
  # itself; a degenerate fit must not take the whole result down with it.
  zph <- tryCatch(survival::cox.zph(fit), error = function(e) NULL)
  model_evidence <- list(
    coefficients = stats::coef(fit),
    covariance = stats::vcov(fit),
    coefficient_terms = stats::setNames(names(stats::coef(fit)), names(stats::coef(fit))),
    formula = formula,
    n = as.integer(fit$n),
    interval = NULL,
    interval_method = "wald",
    conf.level = args$conf.level
  )
  model_information <- data.frame(
    outcome = args$event,
    n = as.integer(fit$n),
    events = as.integer(fit$nevent),
    family = "cox",
    link = "log hazard",
    method = "coxph",
    converged = is.null(fit$fail) && all(is.finite(stats::coef(fit))),
    boundary = NA,
    failed = FALSE,
    error = NA_character_
  )

  terms <- data.frame(
    term = rownames(coef),
    level = rownames(coef),
    estimate = coef[["exp(coef)"]],
    lower = ci[[sprintf("lower .%02d", round(args$conf.level * 100))]] %||% ci[[grep("^lower", names(ci), value = TRUE)[[1]]]],
    upper = ci[[sprintf("upper .%02d", round(args$conf.level * 100))]] %||% ci[[grep("^upper", names(ci), value = TRUE)[[1]]]],
    p = coef[["Pr(>|z|)"]]
  )
  rownames(terms) <- NULL

  list(
    data = list(terms = terms),
    meta = list(
      time = args$time,
      event = args$event,
      outcomes = args$event,
      predictors = paste(deparse(args$predictors), collapse = " "),
      # nobs.coxph() returns the number of events. fit$n is the number of
      # records retained after model-wise missing-data handling.
      n = unname(fit$n),
      n_events = unname(fit$nevent),
      concordance = unname(fit_summary$concordance[[1]]),
      lr_p = unname(fit_summary$logtest[["pvalue"]]),
      cox_zph = if (is.null(zph)) NULL else zph[["table"]],
      event_coding = event_info$note,
      conf.level = args$conf.level,
      conf_pct = round(args$conf.level * 100),
      d = as.integer(args$d),
      ties = args$ties,
      strata = args$strata,
      model_evidence = stats::setNames(list(model_evidence), args$event),
      model_info = model_information,
      style = spec$style
    )
  )
}

#########
# SPECIFICATION CONTRACT VALIDATION
# Validates non-negative follow-up times, event status codings, and predictor formulas.

#' Validate survival follow-up time and event indicator roles
#' @keywords internal
#' @noRd
.validate_survival_roles <- function(spec) {
  time <- .resolve_single_role(spec, "time")
  event <- .resolve_single_role(spec, "event")
  if (is.null(time)) {
    simtab_abort_engine(c(
      "A survival spec requires a {.val time} role.",
      "i" = "No follow-up time column was recorded before compute.",
      "v" = "Use {.code survtab(data, time = time, event = event, predictors = ~ exposure)}."
    ))
  }
  if (is.null(event)) {
    simtab_abort_engine(c(
      "A survival spec requires an {.val event} role.",
      "i" = "No event-status column was recorded before compute.",
      "v" = "Use {.code survtab(data, time = time, event = event, predictors = ~ exposure)}."
    ))
  }

  data <- spec$data_src$ref$data
  if (!time %in% names(data) || !event %in% names(data)) {
    simtab_abort_binding(c(
      "{.arg time} and {.arg event} must resolve to columns present in the data.",
      "i" = "Columns available: {.val {names(data)}}.",
      "v" = "Check the spelling of the follow-up time and event indicator."
    ))
  }
  if (!is.numeric(data[[time]]) || any(data[[time]] < 0, na.rm = TRUE)) {
    simtab_abort_binding(c(
      "{.arg time} must be numeric and non-negative.",
      "i" = "Follow-up time is measured forward from entry, so it cannot be negative.",
      "v" = "Check {.val {time}} for negative values or a non-numeric class."
    ))
  }
  .survival_coerce_event(data[[event]])
  invisible(list(time = time, event = event))
}

#' Validate required roles and predictor formula in Cox specification
#' @keywords internal
#' @noRd
.validate_cox <- function(spec) {
  .validate_survival_roles(spec)
  predictors <- (spec$engine_opts$cox %||% list())$predictors
  if (is.null(predictors) || !inherits(predictors, "formula")) {
    simtab_abort_engine(c(
      "{.fn survtab} requires at least one predictor.",
      "i" = "The Cox engine needs a right-hand-side model formula.",
      "v" = "Use {.code survtab(data, time = time, event = event, predictors = ~ exposure)}."
    ))
  }
  terms <- attr(stats::terms(predictors), "term.labels")
  if (length(terms) < 1) {
    simtab_abort_engine(c(
      "{.fn survtab} requires at least one predictor.",
      "i" = "The supplied formula has no predictor terms.",
      "v" = "Use a non-empty formula, e.g. {.code predictors = ~ exposure}."
    ))
  }
  invisible(spec)
}
