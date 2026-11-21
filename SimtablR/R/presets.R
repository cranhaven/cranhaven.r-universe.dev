# PRESET CONSTRUCTORS AND PARSING HELPERS
# High-level entry points and specification builders for table analysis. This is different than the older version of simtablr where these functions where their own R files.
# Constructs specifications for tb(), regtab(), diag_test(), roc(), and survtab().

#########
# INPUT VALIDATION AND PRESET GUARDS
# Common defensive assertion guards and abort helpers for eager table constructors.

#' Validate input dataset, confidence level, and formatting arguments
#' @keywords internal
#' @noRd
.check_preset_data <- function(data, conf.level, d = NULL, percent = NULL) {
  .check_data_frame(data)
  if (nrow(data) == 0) {
    simtab_abort_input(c(
      "{.arg data} is empty (0 rows).",
      "i" = "Nothing can be estimated from a table with no observations.",
      "v" = "Supply the analytic data frame, or relax the filter that emptied it."
    ))
  }
  if (
    !is.numeric(conf.level) ||
      length(conf.level) != 1 ||
      is.na(conf.level) ||
      conf.level <= 0 ||
      conf.level >= 1
  ) {
    simtab_abort_input(c(
      "{.arg conf.level} must be a single number strictly between 0 and 1.",
      "i" = "Received: {.val {conf.level}}.",
      "v" = "Use {.code conf.level = 0.95} for conventional 95% intervals."
    ))
  }
  if (
    !is.null(d) &&
      (!is.numeric(d) || length(d) != 1 || is.na(d) || d < 0 || d > 10)
  ) {
    simtab_abort_input(c(
      "{.arg d} must be a single number between 0 and 10.",
      "i" = "Received: {.val {d}}.",
      "v" = "Use {.code d = 2} for two-decimal display."
    ))
  }
  if (
    !is.null(percent) &&
      (!is.logical(percent) || length(percent) != 1 || is.na(percent))
  ) {
    simtab_abort_input(c(
      "{.arg percent} must be a single {.code TRUE} or {.code FALSE}.",
      "v" = "Use {.code percent = TRUE} to display proportions as percentages."
    ))
  }
  invisible(NULL)
}

#' Abort when a constructor was called without its data argument
#'
#' @param fn Name of the calling constructor, for the message.
#' @param example A complete example call to show as the fix.
#' @keywords internal
#' @noRd
.abort_preset_missing_data <- function(
  fn,
  example,
  call = rlang::caller_env()
) {
  simtab_abort_input(
    c(
      "No data provided.",
      "i" = "{.fn {fn}} needs the analytic data frame as its first argument.",
      "v" = "Call {.code {example}}."
    ),
    call = call,
    .envir = environment()
  )
}

#' Abort when a required role argument was not supplied
#'
#' @param arg Name of the missing argument.
#' @param fn Name of the calling constructor.
#' @param example A complete example call to show as the fix.
#' @keywords internal
#' @noRd
.abort_preset_missing_role <- function(
  arg,
  fn,
  example,
  call = rlang::caller_env()
) {
  simtab_abort_input(
    c(
      "{.arg {arg}} was not specified.",
      "i" = "{.fn {fn}} cannot identify the column to use without it.",
      "v" = "Call {.code {example}}."
    ),
    call = call,
    .envir = environment()
  )
}

#########
# BIVARIATE TABLE SPECIFICATION BUILDER
# Parses input arguments, flags, and creates the simtab_spec for tb() calls.

#' Parse data, tidyselect expressions, and terse flags for bivariate tables
#' @keywords internal
#' @noRd
.tb_parse_inputs <- function(
  data,
  dot_exprs,
  dots_env = parent.frame(),
  m,
  m_deprecated = FALSE,
  rp,
  or,
  flags,
  measure = NULL,
  subset_expr = quote(NULL),
  strat_expr = quote(NULL)
) {
  if (missing(data)) {
    simtab_abort_input(c(
      "No data provided.",
      "i" = "{.fn tb} needs the data as its first argument.",
      "v" = "Call {.code tb(data, var1, var2)} with a data frame or a vector."
    ))
  }
  if (is.matrix(data) || (!is.data.frame(data) && !is.atomic(data))) {
    simtab_abort_input(c(
      "{.arg data} must be a data frame or an atomic vector.",
      "i" = "Received an object of class {.cls {class(data)[[1]]}}.",
      "v" = "Convert a matrix with {.code as.data.frame(data)} before calling {.fn tb}."
    ))
  }

  prepared <- if (is.data.frame(data)) {
    list(data = data, is_data_frame = TRUE)
  } else {
    list(data = data.frame(.tb_data = data), is_data_frame = FALSE)
  }
  .validate_flag_vector(flags)
  if (
    !is.null(measure) &&
      (!is.character(measure) || length(measure) != 1 || !nzchar(measure))
  ) {
    simtab_abort_input(c(
      "{.arg measure} must be a single non-empty string.",
      "i" = "Received: {.val {measure}}.",
      "v" = "Use one of {.val PR}, {.val RR}, or {.val OR}."
    ))
  }

  legacy_flags <- character()
  if (isTRUE(m)) {
    if (isTRUE(m_deprecated)) {
      .flag_deprecate("m", "miss")
    }
    legacy_flags <- c(legacy_flags, "miss")
  }
  if (isTRUE(rp)) {
    legacy_flags <- c(legacy_flags, "pr")
  }
  if (isTRUE(or)) {
    legacy_flags <- c(legacy_flags, "or")
  }

  dot_scan <- .scan_dot_flags(prepared$data, dot_exprs, surface = "tb")
  flag_tokens <- c(legacy_flags, flags %||% character(), dot_scan$flags)
  canonical_tokens <- .canonicalise_flag_tokens(flag_tokens)
  pct_requested <- any(canonical_tokens %in% c("row", "col", "cell"))

  describe_exprs <- dot_scan$values
  if (!prepared$is_data_frame && length(describe_exprs) == 0) {
    describe_exprs <- list(as.name(".tb_data"))
  }
  if (length(describe_exprs) == 0) {
    simtab_abort_input(c(
      "No variables were specified.",
      "i" = "{.fn tb} describes one or two variables passed unnamed in {.code ...}.",
      "v" = "Call {.code tb(data, exposure, outcome)}."
    ))
  }

  describe_quos <- rlang::new_quosures(
    lapply(describe_exprs, rlang::new_quosure, env = dots_env)
  )
  var_names <- .eval_select_quos(describe_quos, prepared$data)
  if (length(var_names) > 2) {
    simtab_abort_input(c(
      "Maximum of 2 variables allowed, but {length(var_names)} were selected.",
      "i" = "Selected: {.val {var_names}}.",
      "v" = "Use {.fn table1} to describe more than two variables at once."
    ))
  }

  base_spec <- simtab(prepared$data)
  flags_list <- .tb_flags_from_state(
    .apply_flags(base_spec, flag_tokens),
    pct_requested = pct_requested
  )

  list(
    data = prepared$data,
    base_spec = base_spec,
    var_names = var_names,
    flags_list = flags_list,
    flag_tokens = flag_tokens,
    pct_requested = pct_requested,
    measure = if (is.null(measure)) NULL else toupper(measure)
  )
}

#' Extract active display flag list from specification state
#' @keywords internal
#' @noRd
.tb_flags_from_state <- function(spec, pct_requested = FALSE) {
  list(
    missing = isTRUE(spec$missing$display),
    percent = isTRUE(pct_requested),
    by = spec$layout$pct %||% "total"
  )
}

#' Assemble completed simtab_spec object for bivariate contingency tables
#' @keywords internal
#' @noRd
.tb_spec <- function(
  data,
  var_names,
  m,
  d,
  big_mark = "",
  decimal_mark = ".",
  style,
  style.rp,
  style.or,
  test,
  test_explicit = TRUE,
  subset,
  subset_env = baseenv(),
  strat,
  strat_env = baseenv(),
  measure,
  ref,
  conf.level,
  var.type,
  stat.cont,
  flags_list,
  flag_tokens = NULL,
  pct_requested = NULL,
  labels,
  design = NULL,
  call = NULL,
  base_spec = NULL
) {
  .check_d(d)
  .check_conf_level(conf.level)
  if (is.character(test)) {
    test <- tolower(test)
    valid_tests <- c("chisq", "fisher", "mcnemar", "trend")
    if (!test %in% valid_tests) {
      simtab_abort_input(c(
        "Invalid test method {.val {test}}.",
        "i" = "Accepted methods: {.val {valid_tests}}.",
        "v" = "Use one of the listed methods, or {.code test = TRUE} for automatic selection."
      ))
    }
  }

  spec <- if (is.null(base_spec)) {
    simtab(data)
  } else {
    validate_simtab_spec(base_spec)
  }
  spec <- missingness(spec, display = FALSE)
  spec <- .bind_engine(spec, "bivariate")
  spec <- describe(spec, tidyselect::all_of(var_names))
  spec <- set_summary(spec, stat.cont)

  if (is.null(flag_tokens)) {
    if (isTRUE(flags_list$missing)) {
      spec <- missingness(spec, display = TRUE)
    }
    if (isTRUE(flags_list$percent)) {
      spec <- .set_pct_direction(spec, flags_list$by %||% "total")
    }
  } else {
    spec <- .apply_flags(spec, flag_tokens)
  }

  spec <- .set_effect_defaults(spec, ref = ref, conf.level = conf.level)
  if (!is.null(measure)) {
    spec <- measure(spec, measure, ref = ref, conf.level = conf.level)
  }
  if (!is.null(design)) {
    spec <- set_design(spec, design)
  }
  if (isTRUE(test_explicit)) {
    spec <- test(spec, method = test)
  } else if (!is.null(spec$comparison$test)) {
    spec <- test(spec, method = spec$comparison$test)
  }
  spec <- style(spec, style)
  spec <- fmt(
    spec,
    d = d,
    conf_pct = round(conf.level * 100),
    labels = labels,
    big_mark = big_mark,
    decimal_mark = decimal_mark
  )

  if (is.null(flag_tokens)) {
    pct_requested <- isTRUE(flags_list$percent)
  } else if (is.null(pct_requested)) {
    pct_requested <- any(
      .canonicalise_flag_tokens(flag_tokens) %in% c("row", "col", "cell")
    )
  }
  flags_list <- .tb_flags_from_state(spec, pct_requested = pct_requested)
  test_engine <- .comparison_test_arg(spec$comparison$test)

  engine_opts <- list(
    var_names = var_names,
    flags = flags_list,
    style.rp = style.rp,
    style.or = style.or,
    subset = subset,
    subset_env = subset_env,
    strat = strat,
    strat_env = strat_env,
    var.type = var.type,
    stat.cont = stat.cont,
    labels = labels,
    test = test_engine,
    p.adjust = spec$comparison$p.adjust %||% "none",
    paired = isTRUE(spec$comparison$paired)
  )
  spec <- .set_engine_opts(spec, "bivariate", engine_opts)
  spec["call"] <- list(call)
  spec
}

#########
# REGRESSION SPECIFICATION BUILDER
# Family coercion, formula parsing, and covariance resolution for regtab().

#' Validate and coerce regression family input to a family object
#' @keywords internal
#' @noRd
.coerce_glm_family <- function(family) {
  if (inherits(family, "family")) {
    return(family)
  }
  if (is.character(family) && length(family) == 1 && nzchar(family)) {
    fam_fun <- tryCatch(
      get(family, envir = asNamespace("stats"), inherits = FALSE),
      error = function(e) NULL
    )
    if (is.function(fam_fun)) {
      return(fam_fun())
    }
  }
  simtab_abort_input(c(
    "{.arg family} must be a {.cls family} object or a single family name.",
    "i" = "Received: {.val {family}}.",
    "v" = "Use {.code family = binomial()} or {.code family = \"poisson\"}."
  ))
}

#' Parse formula or character predictor specification into a one-sided formula
#' @keywords internal
#' @noRd
.parse_regtab_predictors <- function(predictors) {
  if (
    is.character(predictors) && length(predictors) == 1 && nzchar(predictors)
  ) {
    rhs <- predictors
    if (!grepl("~", rhs, fixed = TRUE)) {
      rhs <- paste("~", rhs)
    }
    return(stats::as.formula(rhs, env = baseenv()))
  }
  if (!inherits(predictors, "formula")) {
    simtab_abort_input(c(
      "{.arg predictors} must be a formula or a single character string.",
      "i" = "Received an object of class {.cls {class(predictors)[[1]]}}.",
      "v" = "Use {.code predictors = ~ age + sex} or {.code predictors = \"age + sex\"}."
    ))
  }

  rhs <- predictors[[length(predictors)]]
  stats::as.formula(
    paste("~", paste(deparse(rhs), collapse = " ")),
    env = environment(predictors)
  )
}

#' Resolve `regtab()`'s `robust` argument to a `.fit_glm_effect`-compatible vcov type
#'
#' `robust` accepts the historical `TRUE`/`FALSE` shorthand (HC0 / model-based)
#' plus an explicit HC type string for small-sample-appropriate robust SEs.
#'
#' @param robust `TRUE`, `FALSE`, or one of `"HC0"`, `"HC1"`, `"HC2"`, `"HC3"`,
#'   `"none"`.
#' @return A single string: one of `"HC0"`, `"HC1"`, `"HC2"`, `"HC3"`, `"none"`.
#' @references Long, J. S., & Ervin, L. H. (2000). Using heteroscedasticity
#'   consistent standard errors in the linear regression model. \emph{The
#'   American Statistician}, 54(3), 217-224.
#' @keywords internal
#' @noRd
.normalise_regtab_vcov <- function(robust) {
  if (isTRUE(robust)) {
    return("HC0")
  }
  if (identical(robust, FALSE)) {
    return("none")
  }
  if (
    is.character(robust) &&
      length(robust) == 1 &&
      robust %in% c("HC0", "HC1", "HC2", "HC3", "none")
  ) {
    return(robust)
  }
  simtab_abort_input(c(
    "{.arg robust} must be {.code TRUE}, {.code FALSE}, or a sandwich type.",
    "i" = "Sandwich types: {.val HC0}, {.val HC1}, {.val HC2}, {.val HC3}, {.val none}.",
    "v" = "Use {.code robust = TRUE} for the default sandwich covariance."
  ))
}

#' Assemble completed simtab_spec object for multi-outcome regression tables
#' @keywords internal
#' @noRd
.regtab_spec <- function(
  data,
  outcomes,
  predictors,
  family,
  offset_quo = rlang::quo(NULL),
  robust,
  method,
  exponentiate,
  labels,
  predictor_labels,
  d,
  conf.level,
  include_intercept,
  p_values,
  call = NULL
) {
  .check_data_frame(data)
  if (
    !is.character(outcomes) || length(outcomes) == 0 || any(!nzchar(outcomes))
  ) {
    simtab_abort_input(c(
      "{.arg outcomes} must be a non-empty character vector of column names.",
      "i" = "Received an object of class {.cls {class(outcomes)[[1]]}} of length {length(outcomes)}.",
      "v" = "Use {.code outcomes = c(\"event_30d\", \"event_365d\")}."
    ))
  }
  if (anyDuplicated(outcomes)) {
    simtab_abort_input(c(
      "{.arg outcomes} must contain unique column names.",
      "i" = "Duplicated: {.val {unique(outcomes[duplicated(outcomes)])}}.",
      "v" = "Remove the repeated outcome name."
    ))
  }
  if (
    !is.character(method) ||
      length(method) != 1 ||
      !tolower(method) %in% c("glm", "firth")
  ) {
    simtab_abort_spec(
      "{.arg method} must be one of {.val glm} or {.val firth}."
    )
  }
  method_normalised <- tolower(method)
  if (
    identical(method_normalised, "firth") &&
      is.character(robust) &&
      length(robust) == 1 &&
      robust %in% c("HC0", "HC1", "HC2", "HC3")
  ) {
    simtab_abort_spec(
      "Firth profile-likelihood inference cannot be combined with an explicit sandwich covariance type."
    )
  }
  .check_d(d)
  .check_conf_level(conf.level)
  if (!is.null(labels) && (!is.character(labels) || is.null(names(labels)))) {
    simtab_abort_input(c(
      "{.arg labels} must be a named character vector.",
      "i" = "Names are the outcome columns; values are the display labels.",
      "v" = "Use {.code labels = c(event_365d = \"365-day MACE\")}."
    ))
  }
  if (
    !is.null(predictor_labels) &&
      (!is.character(predictor_labels) || is.null(names(predictor_labels)))
  ) {
    simtab_abort_input(c(
      "{.arg predictor_labels} must be a named character vector.",
      "i" = "Names are model term names; values are the display labels.",
      "v" = "Use {.code predictor_labels = c(sexMale = \"Male sex\")}."
    ))
  }

  predictors <- .parse_regtab_predictors(predictors)
  missing_outcomes <- setdiff(outcomes, names(data))
  missing_predictors <- setdiff(all.vars(predictors), names(data))
  missing_columns <- unique(c(missing_outcomes, missing_predictors))
  if (length(missing_columns) > 0) {
    simtab_abort_binding(c(
      "Could not resolve {.fn regtab} column bindings.",
      stats::setNames(
        sprintf("Column '%s' doesn't exist.", missing_columns),
        rep("x", length(missing_columns))
      )
    ))
  }

  family <- .coerce_glm_family(family)
  incompatible <- outcomes[vapply(
    data[outcomes],
    function(x) {
      (is.factor(x) || is.character(x) || is.logical(x)) &&
        identical(family$family, "poisson") &&
        identical(family$link, "log")
    },
    logical(1)
  )]
  if (length(incompatible) > 0) {
    simtab_abort_spec(c(
      sprintf(
        "Outcome %s is incompatible with %s.",
        paste(sprintf("'%s'", incompatible), collapse = ", "),
        "poisson(link = \"log\")"
      ),
      "i" = "Poisson-log models require a numeric count or rate outcome; this outcome is factor, character, or logical.",
      "v" = "For a binary outcome, use family = binomial()."
    ))
  }

  spec <- simtab(data)
  spec <- .bind_engine(spec, "glm")
  if (!rlang::quo_is_null(offset_quo)) {
    spec <- .bind_role(spec, "offset", offset_quo)
  }
  spec <- fmt(spec, d = d, conf_pct = round(conf.level * 100), labels = labels)
  spec <- .set_engine_opts(
    spec,
    "glm",
    list(
      outcomes = outcomes,
      predictors = predictors,
      family = family,
      robust = .normalise_regtab_vcov(robust),
      method = method_normalised,
      exponentiate = exponentiate,
      labels = labels,
      predictor_labels = predictor_labels,
      include_intercept = isTRUE(include_intercept),
      p_values = isTRUE(p_values),
      d = as.integer(d),
      conf.level = conf.level
    )
  )
  spec["call"] <- list(call)
  spec
}

#########
# DIAGNOSTIC TEST CONSTRUCTOR
# Eager evaluation of binary diagnostic test accuracy against reference standards.

#' Assess the accuracy of a binary diagnostic test
#'
#' `diag_test()` compares a binary index test with a binary reference
#' standard and reports the confusion matrix with sensitivity, specificity,
#' predictive values, likelihood ratios, and related metrics, each with a
#' confidence interval. Positive levels are detected automatically but can be
#' set with `positive` and `test_positive`. For a continuous marker use
#' [roc()].
#'
#' @param data A data frame.
#' @param test The index test column: a bare name or string. Must have two
#'   levels.
#' @param ref The reference standard column: a bare name or string. Must have
#'   two levels.
#' @param positive The level of `ref` that means "disease present". If
#'   `NULL`, common labels such as `"Yes"`, `"1"`, or `"Positive"` are
#'   detected, falling back to the last level.
#' @param test_positive The level of `test` that means "test positive". If
#'   `NULL`, `positive` is reused when `test` has that level; otherwise it is
#'   detected the same way.
#' @param conf.level Number between 0 and 1. Confidence level for intervals.
#' @param ci String. Interval method for the proportions: `"exact"`
#'   (Clopper-Pearson) or `"wilson"`.
#' @param d Integer. Decimal places for all displayed estimates and intervals.
#' @param percent Logical. If `TRUE`, show sensitivity, specificity,
#'   predictive values, accuracy, and prevalence as percentages. Ratios and
#'   indices are always shown as decimals.
#'
#' @details
#' ## Statistical methods
#' Sensitivity, specificity, PPV, NPV, accuracy, and prevalence are
#' proportions from the confusion matrix, with Clopper-Pearson intervals by
#' default or Wilson intervals with `ci = "wilson"`. Predictive values and
#' accuracy depend on the prevalence in `data`. Likelihood ratios use the
#' log-method interval for a ratio of proportions (Simel et al., 1991; Altman
#' et al., 2000). The diagnostic odds ratio uses the Woolf logit interval, with
#' standard error \eqn{\sqrt{1/TP + 1/FP + 1/FN + 1/TN}}{sqrt(1/TP + 1/FP + 1/FN + 1/TN)}
#' (Glas et al., 2003).
#' No continuity correction is applied, so both intervals are `NA` when any
#' cell of the matrix is zero. Cohen's kappa measures
#' chance-corrected agreement between test and reference, with the Fleiss,
#' Cohen & Everitt (1969) standard error. The Youden index and F1 score are
#' reported without intervals.
#'
#' ## Missing data
#' Rows missing either the test or the reference are dropped, and a message
#' reports how many.
#'
#' ## Modifying the result
#' Use [fmt()] to change decimals or percentages, `plot()` for a fourfold
#' display, and `ggplot2::autoplot()` with `type = "matrix"` or
#' `type = "metrics"` for a heatmap or a plot of metrics with intervals.
#' `as.data.frame(x, tidy = TRUE)` returns the unrounded metrics.
#'
#' @return A `simtab_result` of class `simtab_diag`. Print it to see the
#'   confusion matrix and metrics, convert it with `as.data.frame()`, or save
#'   it with [export_docx()], [export_pptx()], or [export_xlsx()]. Unrounded
#'   results are stored in `$data`.
#'
#' @seealso [roc()] for continuous markers, [plot.simtab_diag()], and
#'   [simtablr_references] for all references cited by SimtablR.
#'
#' @references Clopper, C. J., & Pearson, E. S. (1934). The use of confidence
#'   or fiducial limits illustrated in the case of the binomial.
#'   \emph{Biometrika}, 26(4), 404--413. \doi{10.1093/biomet/26.4.404}.
#'
#'   Simel, D. L., Samsa, G. P., & Matchar, D. B. (1991). Likelihood ratios
#'   with confidence: sample size estimation for diagnostic test studies.
#'   \emph{Journal of Clinical Epidemiology}, 44(8), 763--770.
#'   \doi{10.1016/0895-4356(91)90128-V}.
#'
#'   Altman, D. G., Machin, D., Bryant, T. N., & Gardner, M. J. (2000).
#'   \emph{Statistics with Confidence} (2nd ed.). BMJ Books.
#'
#'   Glas, A. S., Lijmer, J. G., Prins, M. H., Bonsel, G. J., & Bossuyt, P. M.
#'   M. (2003). The diagnostic odds ratio: a single indicator of test
#'   performance. \emph{Journal of Clinical Epidemiology}, 56(11), 1129--1135.
#'   \doi{10.1016/S0895-4356(03)00177-X}.
#'
#'   Cohen, J. (1960). A coefficient of agreement for nominal scales.
#'   \emph{Educational and Psychological Measurement}, 20(1), 37--46.
#'   \doi{10.1177/001316446002000104}.
#'
#'   Fleiss, J. L., Cohen, J., & Everitt, B. S. (1969). Large sample standard
#'   errors of kappa and weighted kappa. \emph{Psychological Bulletin},
#'   72(5), 323--327. \doi{10.1037/h0028106}.
#'
#' @examples
#' # Point-of-care troponin against adjudicated ACS
#' substudy <- subset(epitabl, diagnostic_substudy == "Yes")
#' acc <- diag_test(substudy, test = poc_hstn_positive, ref = adjudicated_acs)
#' acc
#'
#' # Wilson intervals, decimals instead of percentages
#' diag_test(
#'   substudy, poc_hstn_positive, adjudicated_acs,
#'   positive = "Yes", test_positive = "Positive",
#'   ci = "wilson", percent = FALSE
#' )
#'
#' # Metrics as a data frame
#' as.data.frame(acc)
#'
#' @export
diag_test <- function(
  data,
  test,
  ref,
  positive = NULL,
  test_positive = NULL,
  conf.level = 0.95,
  ci = c("exact", "wilson"),
  d = 2,
  percent = TRUE
) {
  if (missing(data)) {
    .abort_preset_missing_data(
      "diag_test",
      "diag_test(data, test = rapid, ref = gold)"
    )
  }
  .check_preset_data(data, conf.level, d, percent)

  if (missing(ci)) {
    ci <- "exact"
  } else if (
    !is.character(ci) || length(ci) != 1 || !ci %in% c("exact", "wilson")
  ) {
    simtab_abort_input(c(
      "{.arg ci} must be either {.val exact} or {.val wilson}.",
      "i" = "Received: {.val {ci}}.",
      "v" = "Use {.code ci = \"exact\"} for Clopper-Pearson intervals."
    ))
  }

  test_quo <- rlang::enquo(test)
  ref_quo <- rlang::enquo(ref)
  if (rlang::quo_is_missing(test_quo)) {
    .abort_preset_missing_role(
      "test",
      "diag_test",
      "diag_test(data, test = rapid, ref = gold)"
    )
  }
  if (rlang::quo_is_missing(ref_quo)) {
    .abort_preset_missing_role(
      "ref",
      "diag_test",
      "diag_test(data, test = rapid, ref = gold)"
    )
  }

  spec <- simtab(data)
  spec <- .bind_engine(spec, "accuracy")
  spec <- .bind_role(spec, "test", test_quo)
  spec <- .bind_role(spec, "ref_std", ref_quo)
  spec <- fmt(
    spec,
    d = d,
    conf_pct = round(conf.level * 100),
    percent = percent
  )
  spec <- .set_engine_opts(
    spec,
    "accuracy",
    list(
      positive = positive,
      test_positive = test_positive,
      ci = ci,
      conf.level = conf.level,
      percent = percent
    )
  )
  spec["call"] <- list(match.call())

  evaluate(spec)
}

#########
# ROC CURVE ANALYSIS CONSTRUCTOR
# Receiver operating characteristic evaluation for continuous diagnostic markers.

#' Evaluate continuous markers with ROC curves
#'
#' `roc()` measures how well one or more continuous markers discriminate a
#' binary outcome. It reports the area under the ROC curve (AUC) with a
#' confidence interval and, by default, the Youden-optimal cutpoint with its
#' sensitivity, specificity, and predictive values. With two or more markers,
#' their AUCs are compared pairwise with the DeLong test. For a test that is
#' already binary use [diag_test()]. Requires the pROC package.
#'
#' @param data A data frame.
#' @param marker One or more numeric marker columns: a bare name, `c()` of
#'   names, or a tidyselect expression.
#' @param outcome The binary outcome column: a bare name or string.
#' @param positive The level of `outcome` that means "disease present". If
#'   `NULL`, common labels such as `"Yes"`, `"1"`, or `"Positive"` are
#'   detected, falling back to the last level.
#' @param direction String. Which marker values indicate disease: `"<"` if
#'   higher values do, `">"` if lower values do, or `"auto"` to let pROC
#'   choose by comparing the group medians.
#' @param cutpoint String. `"youden"` reports the cutpoint that maximises
#'   sensitivity + specificity - 1; `"none"` reports the AUC only.
#' @param conf.level Number between 0 and 1. Confidence level for AUC
#'   intervals.
#' @param d Integer. Decimal places for displayed estimates.
#' @param percent Logical. If `TRUE`, show the cutpoint's sensitivity,
#'   specificity, and predictive values as percentages. The AUC and the
#'   cutpoint itself are always shown as decimals.
#'
#' @details
#' ## Statistical methods
#' ROC curves and AUCs are computed with pROC (Robin et al., 2011), using
#' DeLong intervals for the AUC and the paired DeLong test to compare markers
#' measured on the same patients (DeLong et al., 1988). A cutpoint chosen from
#' the same data is optimistic: its sensitivity and specificity will usually
#' be lower in new patients (Ewald, 2006), and a note says so. The AUC
#' describes discrimination only, not calibration.
#'
#' ## Missing data
#' Rows with a missing outcome or marker value are dropped, across
#' all markers at once, so every marker is evaluated on the same patients.
#' A message reports how many rows were removed. Each marker needs both
#' outcome classes among the remaining rows.
#'
#' ## Modifying the result
#' Use [fmt()] to change decimals or percentages and `ggplot2::autoplot()` to
#' draw the ROC curves. The curve coordinates and DeLong comparisons are
#' stored in `$data`.
#'
#' @return A `simtab_result` of class `simtab_roc`. Print it to see the AUC
#'   table and comparisons, convert it with `as.data.frame()`, or save it with
#'   [export_docx()], [export_pptx()], or [export_xlsx()]. Unrounded results
#'   are stored in `$data`.
#' @seealso [diag_test()] for binary tests and [simtablr_references] for all
#'   references cited by SimtablR.
#' @references Robin, X., Turck, N., Hainard, A., et al. (2011). pROC: an
#'   open-source package for R and S+ to analyze and compare ROC curves.
#'   \emph{BMC Bioinformatics}, 12, 77. \doi{10.1186/1471-2105-12-77}.
#'
#'   DeLong, E. R., DeLong, D. M., & Clarke-Pearson, D. L. (1988). Comparing
#'   the areas under two or more correlated receiver operating characteristic
#'   curves: a nonparametric approach. \emph{Biometrics}, 44(3), 837--845.
#'   \doi{10.2307/2531595}.
#'
#'   Ewald, B. (2006). Post hoc choice of cut points introduced bias to
#'   diagnostic research. \emph{Journal of Clinical Epidemiology}, 59(8),
#'   798--801. \doi{10.1016/j.jclinepi.2005.11.025}.
#' @examples
#' if (requireNamespace("pROC", quietly = TRUE)) {
#'   substudy <- subset(epitabl, diagnostic_substudy == "Yes")
#'
#'   # AUC and Youden cutpoint for point-of-care troponin
#'   roc(substudy, poc_hstn_value, adjudicated_acs)
#'
#'   # Compare two markers with the paired DeLong test
#'   fit <- roc(substudy, c(poc_hstn_value, systolic_bp), adjudicated_acs,
#'              positive = "Yes")
#'   fit
#'
#'   # AUC only, no data-driven cutpoint
#'   roc(substudy, poc_hstn_value, adjudicated_acs, cutpoint = "none")
#' }
#' @export
roc <- function(
  data,
  marker,
  outcome,
  positive = NULL,
  direction = c("auto", ">", "<"),
  cutpoint = c("youden", "none"),
  conf.level = 0.95,
  d = 2,
  percent = TRUE
) {
  if (missing(data)) {
    .abort_preset_missing_data(
      "roc",
      "roc(data, marker = troponin, outcome = acs)"
    )
  }
  .check_preset_data(data, conf.level, d, percent)

  direction <- match.arg(direction)
  cutpoint <- match.arg(cutpoint)
  marker_quo <- rlang::enquo(marker)
  outcome_quo <- rlang::enquo(outcome)
  if (rlang::quo_is_missing(marker_quo)) {
    .abort_preset_missing_role(
      "marker",
      "roc",
      "roc(data, marker = troponin, outcome = acs)"
    )
  }
  if (rlang::quo_is_missing(outcome_quo)) {
    .abort_preset_missing_role(
      "outcome",
      "roc",
      "roc(data, marker = troponin, outcome = acs)"
    )
  }

  marker_quos <- rlang::new_quosures(list(marker_quo))

  spec <- simtab(data)
  spec <- .bind_engine(spec, "roc")
  spec <- .bind_role(spec, "describe", marker_quos)
  spec <- .bind_role(spec, "ref_std", outcome_quo)
  spec <- measure(spec, "AUC", conf.level = conf.level)
  spec <- fmt(
    spec,
    d = d,
    conf_pct = round(conf.level * 100),
    percent = percent
  )
  spec <- .set_engine_opts(
    spec,
    "roc",
    list(
      positive = positive,
      direction = direction,
      ci = "delong",
      cutpoint = cutpoint,
      conf.level = conf.level,
      percent = percent
    )
  )
  spec["call"] <- list(match.call())

  evaluate(spec)
}

#########
# SURVIVAL ANALYSIS CONSTRUCTOR
# Cox proportional hazards regression table constructor.

#' Fit a Cox proportional hazards model
#'
#' `survtab()` fits a Cox model to time-to-event data and returns a
#' publication-style table of hazard ratios with confidence intervals and
#' p-values. Give the follow-up time, the event indicator, and the predictors;
#' the proportional hazards assumption is checked automatically. For
#' outcomes without follow-up time use [regtab()].
#'
#' @param data A data frame.
#' @param time Follow-up time column: a bare name or string.
#' @param event Event indicator column: a bare name or string. May be 0/1,
#'   logical, or a two-level factor whose second level is the event.
#' @param predictors The right-hand side of the model, as a one-sided formula
#'   (`~ age + sex`) or a character vector of column names.
#' @param design String. Optional study design recorded on the result.
#' @param conf.level Number between 0 and 1. Confidence level for intervals.
#' @param d Integer. Decimal places for hazard ratios and intervals.
#' @param labels Named character vector of display labels for model terms,
#'   e.g. `c(age = "Age (years)", sexMale = "Male sex")`. Variable labels
#'   already stored in `data` are used by default.
#' @param style String. A journal preset name (see [list_journals()]) that
#'   controls formatting such as p-values.
#'
#' @details
#' ## Statistical methods
#' The model is fitted with `survival::coxph()` using the Efron method for
#' ties. Hazard ratios have Wald confidence intervals. The proportional
#' hazards assumption is tested with the Grambsch-Therneau test on Schoenfeld
#' residuals (`survival::cox.zph()`); when any term fails, a warning is shown
#' with the result. Concordance and the likelihood-ratio test p-value are
#' available from `generics::glance()`.
#'
#' ## Missing data
#' Rows with a missing time, event, or predictor are dropped. The number of
#' rows and events analysed is shown in the table header and in
#' [model_info()].
#'
#' ## Modifying the result
#' `coef()`, `confint()`, `vcov()`, `formula()`, and `nobs()` work on the
#' result. [model_info()] reports convergence, `generics::tidy()` returns one
#' row per term, and `ggplot2::autoplot()` draws a forest plot.
#'
#' ## Limitations
#' The result is a reporting table, not a fitted model: it keeps no
#' residuals or fitted values and cannot predict. For stratified or
#' time-varying models, survival curves, or residual diagnostics, fit
#' `survival::coxph()` directly.
#'
#' @return A `simtab_result` of class `simtab_cox`. Print it to see the
#'   formatted table, convert it with `as.data.frame()`, or save it with
#'   [export_docx()], [export_pptx()], or [export_xlsx()]. Unrounded results
#'   are stored in `$data`.
#' @seealso [regtab()] for outcomes without follow-up time, [model_info()]
#'   for convergence, and [simtablr_references] for all references cited by
#'   SimtablR.
#' @references Cox, D. R. (1972). Regression models and life-tables.
#'   \emph{Journal of the Royal Statistical Society, Series B}, 34(2),
#'   187--202. \doi{10.1111/j.2517-6161.1972.tb00899.x}.
#'
#'   Grambsch, P. M., & Therneau, T. M. (1994). Proportional hazards tests and
#'   diagnostics based on weighted residuals. \emph{Biometrika}, 81(3),
#'   515--526. \doi{10.1093/biomet/81.3.515}.
#' @examples
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   # Hazard ratios for major adverse cardiovascular events
#'   fit <- survtab(
#'     epitabl,
#'     time = mace_time_days, event = mace_event,
#'     predictors = ~ age + sex + diabetes
#'   )
#'   fit
#'
#'   # Concordance, likelihood-ratio test, and events analysed
#'   generics::glance(fit)
#' }
#' @export
survtab <- function(
  data,
  time,
  event,
  predictors,
  design = NULL,
  conf.level = 0.95,
  d = 2,
  labels = NULL,
  style = "default"
) {
  if (missing(data)) {
    .abort_preset_missing_data(
      "survtab",
      "survtab(data, time = days, event = died, predictors = ~ age)"
    )
  }
  .check_preset_data(data, conf.level)
  if (missing(predictors)) {
    .abort_preset_missing_role(
      "predictors",
      "survtab",
      "survtab(data, time = days, event = died, predictors = ~ age + sex)"
    )
  }

  time_quo <- rlang::enquo(time)
  event_quo <- rlang::enquo(event)
  if (rlang::quo_is_missing(time_quo)) {
    .abort_preset_missing_role(
      "time",
      "survtab",
      "survtab(data, time = days, event = died, predictors = ~ age)"
    )
  }
  if (rlang::quo_is_missing(event_quo)) {
    .abort_preset_missing_role(
      "event",
      "survtab",
      "survtab(data, time = days, event = died, predictors = ~ age)"
    )
  }

  spec <- simtab(data)
  spec <- .bind_engine(spec, "cox")
  spec <- .bind_role(spec, "time", time_quo)
  spec <- .bind_role(spec, "event", event_quo)
  spec <- fmt(spec, d = d, conf_pct = round(conf.level * 100), labels = labels)
  spec <- style(spec, style)
  if (!is.null(design)) {
    spec <- set_design(spec, design)
  } else {
    spec <- measure(spec, "HR", conf.level = conf.level)
  }
  spec <- .set_engine_opts(
    spec,
    "cox",
    list(
      predictors = if (
        is.character(predictors) &&
          length(predictors) >= 1 &&
          all(nzchar(predictors))
      ) {
        stats::as.formula(
          paste("~", paste(predictors, collapse = " + ")),
          env = baseenv()
        )
      } else {
        .parse_regtab_predictors(predictors)
      },
      conf.level = conf.level,
      d = as.integer(d),
      ties = "efron"
    )
  )
  spec["call"] <- list(match.call())

  evaluate(spec)
}
