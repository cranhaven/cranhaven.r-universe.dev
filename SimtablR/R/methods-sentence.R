# MANUSCRIPT METHODS NARRATIVE SYNTHESIZER
# Translates recorded analytical decisions (summary metrics, hypothesis tests,
# effect estimators, regression models, and diagnostic frameworks) into publication-ready prose.

#########
# S3 GENERICS AND METHOD DISPATCH
# S3 method dispatch across uncomputed specifications, computed results, and composed reports.

#' Generate a manuscript methods sentence
#'
#' Builds concise manuscript prose from the decisions recorded in a computed
#' SimtablR result. Passing a bare `simtab_spec` computes it first, matching the
#' other render/extract verbs.
#'
#' @param x A `simtab_result` or `simtab_spec`.
#' @param ... Ignored.
#' @return A character string of length one.
#' @examples
#' res <- tb(epitabl, sex, diabetes)
#' as_methods(res)
#' @export
as_methods <- function(x, ...) {
  UseMethod("as_methods")
}

#' @export
as_methods.simtab_spec <- function(x, ...) {
  as_methods(evaluate(x), ...)
}

#' @export
as_methods.simtab_result <- function(x, ...) {
  x <- validate_simtab_result(x)
  fn <- .engine_renderer(x, "as_methods")
  if (is.null(fn)) {
    engine <- x$meta$engine %||% x$spec$engine %||% "unknown"
    simtab_abort_render(c(
      sprintf("No {.fn as_methods} renderer is registered for engine {.val %s}.", engine),
      "i" = "SimtablR results must report the analytic decisions used to produce the table.",
      ">" = "Register one with {.code register_engine(..., renderers = list(as_methods = function(x, ...) ...))}."
    ))
  }
  fn(x, ...)
}

#' @export
as_methods.simtab_report <- function(x, ...) {
  x <- validate_simtab_report(x)
  if (!is.null(x$methods) && is.character(x$methods) && length(x$methods) == 1 && nzchar(x$methods)) {
    return(x$methods)
  }
  .report_methods(x$items)
}

#########
# ENGINE-SPECIFIC METHODS PARSERS
# Builds narrative methodology statements for Table 1, tb, regtab, and diag_test.

#' Synthesize methodology narrative for Table 1 descriptive results
#' @keywords internal
#' @noRd
.methods_as_table1 <- function(x, ...) {
  x <- validate_simtab_result(x)
  parts <- c(
    .methods_summary_sentence(x),
    .methods_test_sentence(x),
    .methods_effect_sentence(x)
  )
  paste(parts[nzchar(parts)], collapse = " ")
}

#' Synthesize methodology narrative for focused bivariate tb results
#' @keywords internal
#' @noRd
.methods_as_tb <- function(x, ...) {
  x <- validate_simtab_result(x)
  parts <- c(
    .methods_tb_summary_sentence(x),
    .methods_tb_test_sentence(x),
    .methods_effect_sentence(x)
  )
  paste(parts[nzchar(parts)], collapse = " ")
}

#' Synthesize methodology narrative for regression regtab results
#' @keywords internal
#' @noRd
.methods_as_regtab <- function(x, ...) {
  x <- validate_simtab_result(x)
  ci <- x$meta$conf_pct %||% round((x$meta$conf.level %||% 0.95) * 100)
  if (identical(x$meta$method %||% "glm", "firth")) {
    return(sprintf(
      "Regression models were fit with Firth penalised likelihood (Firth 1993; Heinze & Schemper 2002); %d%% profile-penalised CI were reported.",
      ci
    ))
  }
  se <- if (isTRUE(x$meta$robust)) {
    sprintf("robust %s standard errors", x$meta$vcov %||% "HC0")
  } else {
    "model-based standard errors"
  }
  exp_txt <- if (isTRUE(x$meta$exponentiate)) " and exponentiated estimates" else ""
  sprintf(
    "Regression models were fit with a %s family and %s link using %s%s; %d%% CI were reported.",
    x$meta$family,
    x$meta$link,
    se,
    exp_txt,
    ci
  )
}

#' Synthesize methodology narrative for diagnostic accuracy test results
#' @keywords internal
#' @noRd
.methods_as_diag <- function(x, ...) {
  x <- validate_simtab_result(x)
  ci <- x$meta$conf_pct %||% round((x$meta$conf.level %||% 0.95) * 100)
  ci_name <- if (identical(x$meta$ci, "wilson")) "Wilson" else "exact binomial"
  sprintf(
    paste0(
      "Diagnostic accuracy was evaluated against %s as the reference standard; ",
      "sensitivity, specificity, predictive values, accuracy, and prevalence were ",
      "reported with %s %d%% CI, likelihood ratios with asymptotic (log-method) ",
      "%d%% confidence intervals, the diagnostic odds ratio with a log %d%% CI, ",
      "and Cohen's kappa was reported for index-test / reference-standard ",
      "agreement with a large-sample %d%% CI."
    ),
    x$meta$ref_var,
    ci_name,
    ci, ci, ci, ci
  )
}

#########
# STATISTICAL SUMMARY AND HYPOTHESIS TESTING CLAUSES
# Formats sentences detailing summary statistics, skewness rationales, and test methods.

#' Construct summary statistics methodology sentence for Table 1 variables
#' @keywords internal
#' @noRd
.methods_summary_sentence <- function(x) {
  stats <- vapply(names(x$data), function(v) {
    rec <- x$data[[v]]
    if (!identical(rec$type, "continuous")) {
      return(NA_character_)
    }
    label <- if (identical(rec$stat, "mean")) "mean (SD)" else "median [IQR]"
    auto <- x$meta$summary_auto[[v]] %||% rec$auto_summary
    if (!is.null(auto)) {
      if (identical(auto$decision, "mean")) {
        return(paste0(label, ", distribution approximately symmetric"))
      }
      if (grepl("skewness", auto$reason %||% "", fixed = TRUE)) {
        return(paste0(label, " owing to skewness"))
      }
    }
    label
  }, character(1))
  stats <- unique(stats[!is.na(stats)])
  if (length(stats) == 0) {
    return("Categorical variables were summarised with counts and percentages.")
  }
  paste0("Continuous variables were summarised as ", paste(stats, collapse = " and "), ".")
}

#' Construct summary statistics methodology sentence for tb bivariate comparisons
#' @keywords internal
#' @noRd
.methods_tb_summary_sentence <- function(x) {
  if (isTRUE(x$meta$is_continuous)) {
    stat <- if (identical(x$meta$stat.cont, "mean")) "mean (SD)" else "median [IQR]"
    auto <- x$meta$summary_auto
    if (!is.null(auto)) {
      if (identical(auto$decision, "mean")) {
        return(paste0("Continuous variables were summarised as ", stat, ", distribution approximately symmetric."))
      }
      if (grepl("skewness", auto$reason %||% "", fixed = TRUE)) {
        return(paste0("Continuous variables were summarised as ", stat, " owing to skewness."))
      }
    }
    return(paste0("Continuous variables were summarised as ", stat, "."))
  }
  "Categorical variables were summarised with counts and percentages."
}

#' Construct hypothesis testing methodology clause for Table 1 results
#' @keywords internal
#' @noRd
.methods_test_sentence <- function(x) {
  methods <- vapply(x$data, function(rec) {
    if (is.null(rec$test)) NA_character_ else rec$test$method %||% NA_character_
  }, character(1))
  methods <- unique(methods[!is.na(methods)])
  if (length(methods) == 0) {
    return("")
  }
  methods <- .clean_test_methods(methods)
  paste0("Group comparisons used ", paste(methods, collapse = "; "), ".")
}

#' Construct hypothesis testing methodology clause for tb results
#' @keywords internal
#' @noRd
.methods_tb_test_sentence <- function(x) {
  if (is.null(x$meta$stats)) {
    return("")
  }
  method <- x$meta$stats$method %||% NA_character_
  if (is.na(method)) {
    return("")
  }
  method <- .clean_test_methods(method)
  paste0("Group comparisons used ", method, ".")
}

#' Standardize and format statistical test names for manuscript prose
#' @keywords internal
#' @noRd
.clean_test_methods <- function(methods) {
  methods <- gsub("Pearson's Chi-squared", "Pearson chi-squared", methods, fixed = TRUE)
  methods <- gsub(" with Yates' continuity correction", "", methods, fixed = TRUE)
  methods
}

#########
# EFFECT ESTIMATION AND MODEL CLAUSES
# Synthesizes effect measure rationales, design origin, and log-binomial/Poisson fallback clauses.

#' Construct effect measure estimation methodology clause
#' @keywords internal
#' @noRd
.methods_effect_sentence <- function(x) {
  measure <- x$spec$effect$measure %||% x$meta$effect %||% NULL
  if (is.null(measure)) {
    return("")
  }
  measure <- toupper(measure)
  ci <- x$meta$conf_pct %||% round((x$spec$effect$conf.level %||% 0.95) * 100)
  function_decisions <- .methods_function_measure_decisions(x)
  if (!is.null(function_decisions)) {
    return(sprintf(
      "%s was estimated using %s with %s %d%% confidence intervals.",
      measure,
      function_decisions$estimator,
      function_decisions$interval,
      ci
    ))
  }
  adjust_vars <- x$meta$adjust$vars %||% character()
  if (length(adjust_vars) > 0) {
    return(.methods_adjusted_effect_sentence(x, measure, ci, adjust_vars))
  }
  sprintf(
    "%s was estimated using the %s with %d%% confidence intervals.%s",
    measure, .methods_crude_estimator(measure), ci, .methods_design_rationale(x, measure)
  )
}

#' Name the crude 2x2 estimator used for an effect measure
#' @keywords internal
#' @noRd
.methods_crude_estimator <- function(measure) {
  switch(
    measure,
    OR = "Woolf/logit odds-ratio estimator",
    PR = "Katz log prevalence-ratio estimator",
    RR = "Katz log risk-ratio estimator",
    paste0(measure, " estimator")
  )
}

#' Sentence explaining a design-resolved effect measure, or ""
#' @keywords internal
#' @noRd
.methods_design_rationale <- function(x, measure) {
  design <- .result_design(x)
  if (identical(x$spec$effect$resolved_from, "design") && !is.null(design)) {
    return(sprintf(" The %s was chosen from the recorded study design (%s).", measure, .design_label(design)))
  }
  ""
}

#' Construct methods prose for crude plus covariate-adjusted effect columns
#'
#' A table with an adjusted column reports two estimands: the crude 2x2 ratio
#' and the model-adjusted ratio. Both, and the adjustment set, belong in the
#' methods.
#' @keywords internal
#' @noRd
.methods_adjusted_effect_sentence <- function(x, measure, ci, adjust_vars) {
  covar_labels <- vapply(
    adjust_vars,
    function(v) .resolve_label(v, x$used$ref$data, x$spec$fmt$labels),
    character(1)
  )
  covars <- if (length(covar_labels) == 1) {
    covar_labels
  } else {
    paste(paste(utils::head(covar_labels, -1), collapse = ", "), "and", utils::tail(covar_labels, 1))
  }
  crude <- sprintf(
    "Crude %s was estimated using the %s with %d%% confidence intervals.",
    measure, .methods_crude_estimator(measure), ci
  )
  adjusted <- if (measure %in% c("PR", "RR") && length(x$meta$effect_estimators %||% character()) > 0) {
    .methods_prrr_adjusted_sentence(x, measure, ci, covars)
  } else if (identical(measure, "OR")) {
    sprintf(
      "Adjusted OR was estimated by logistic regression adjusting for %s, with robust (HC0) standard errors and %d%% confidence intervals.",
      covars, ci
    )
  } else {
    sprintf("Adjusted %s was estimated adjusting for %s, with %d%% confidence intervals.", measure, covars, ci)
  }
  paste0(crude, " ", adjusted, .methods_design_rationale(x, measure))
}

#' Extract custom effect measure and confidence interval metadata
#' @keywords internal
#' @noRd
.methods_function_measure_decisions <- function(x) {
  by_variable <- x$meta$effect_measure_meta %||% list()
  entries <- unlist(by_variable, recursive = FALSE, use.names = FALSE)
  entries <- entries[vapply(entries, is.list, logical(1))]
  if (length(entries) == 0) {
    return(NULL)
  }

  estimators <- unique(vapply(entries, function(entry) {
    entry$estimator %||% NA_character_
  }, character(1)))
  intervals <- unique(vapply(entries, function(entry) {
    entry$interval %||% NA_character_
  }, character(1)))
  estimators <- estimators[!is.na(estimators) & nzchar(estimators)]
  intervals <- intervals[!is.na(intervals) & nzchar(intervals)]
  if (length(estimators) == 0 || length(intervals) == 0) {
    return(NULL)
  }

  list(
    estimator = paste(estimators, collapse = "; "),
    interval = paste(intervals, collapse = "; ")
  )
}

#' Format comma-separated variable display labels for methods prose
#' @keywords internal
#' @noRd
.methods_effect_labels <- function(x, vars) {
  labels <- x$meta$labels[vars] %||% vars
  labels <- unname(ifelse(is.na(labels) | !nzchar(labels), vars, labels))
  paste(labels, collapse = ", ")
}

#' Construct methodology sentence for adjusted prevalence/risk ratio models and fallbacks
#' @keywords internal
#' @noRd
.methods_prrr_adjusted_sentence <- function(x, measure, ci, covars) {
  estimators <- x$meta$effect_estimators %||% character()
  fallbacks <- x$meta$logbinomial_fallback %||% logical()
  vars <- names(estimators)
  fallback_vars <- names(fallbacks)[as.logical(fallbacks)]
  fallback_vars <- intersect(fallback_vars, vars)
  logbin_vars <- setdiff(vars[estimators == "log-binomial"], fallback_vars)
  adjusted_for <- sprintf(" adjusting for %s", covars)

  if (length(fallback_vars) == 0) {
    return(sprintf(
      "Adjusted %s was estimated by log-binomial regression%s, with %d%% confidence intervals.",
      measure,
      adjusted_for,
      ci
    ))
  }

  # Methods prose must name the reason the fallback fired. A model that failed
  # to converge and one that could not be fitted at all are different findings,
  # and a manuscript should not report the second as the first.
  reason <- .logbinomial_reason_phrase(x, fallback_vars)

  if (length(logbin_vars) == 0) {
    return(sprintf(
      "Adjusted %s was estimated by modified Poisson regression with robust standard errors%s because %s for %s (Zou, 2004); %d%% confidence intervals were reported.",
      measure,
      adjusted_for,
      reason,
      .methods_effect_labels(x, fallback_vars),
      ci
    ))
  }

  sprintf(
    "Adjusted %s was estimated%s by log-binomial regression for %s and by modified Poisson regression with robust standard errors for %s because %s for those variable(s) (Zou, 2004); %d%% confidence intervals were reported.",
    measure,
    adjusted_for,
    .methods_effect_labels(x, logbin_vars),
    .methods_effect_labels(x, fallback_vars),
    reason,
    ci
  )
}
