# REGISTRY FRAMEWORK FOR ENGINES AND MEASURES
# Extensible in-memory registries for statistical engines and epidemiological effect measures.

.simtab_engine_registry <- new.env(parent = emptyenv())
.simtab_measure_registry <- new.env(parent = emptyenv())

#########
# ENGINE REGISTRY
# Registration, lookup, and option validation for statistical computation engines.

#' Normalises case-insensitive registry keys
#' @keywords internal
#' @noRd
.normalise_registry_name <- function(name, arg = "name") {
  if (!is.character(name) || length(name) != 1 || anyNA(name) || !nzchar(name)) {
    simtab_abort_spec(c(
      sprintf("{.arg %s} must be one non-missing, non-empty string.", arg),
      "i" = "Registry entries are identified by one case-insensitive name."
    ))
  }
  tolower(name)
}

#' Register a SimtablR computation engine
#'
#' Stores the full engine contract — compute function, result subclass,
#' optional legacy class tag, optional hard-error validator, a named subset of
#' renderer verbs, and an optional engine-options validator — under a
#' case-insensitive name. A user-registered engine gets print/as.data.frame/
#' as_flextable/as_gt/tidy/glance/autoplot/as_methods dispatch through
#' [evaluate()]'s shared `simtab_result` methods with no further S3 ceremony.
#'
#' @param name Character engine name, e.g. `"roc"`.
#' @param compute Function with signature `function(spec, data)` returning
#'   `list(data = ..., meta = ...)`.
#' @param subclass Character result subclass. Defaults to `paste0("simtab_", name)`.
#' @param legacy_tag Optional extra class tag kept for backward-compatible
#'   `inherits()` checks. Never used for SimtablR's own dispatch.
#' @param validate Optional function `function(spec) -> invisible(spec)` for
#'   hard, engine-specific errors only.
#' @param renderers Named list of renderer functions, names drawn from
#'   `.renderer_verbs()` (`print`, `as_data_frame`, `as_flextable`, `as_gt`,
#'   `tidy`, `glance`, `autoplot`, `as_methods`). Each function takes
#'   `function(x, ...)`.
#' @param engine_opts Optional function `function(opts) -> invisible(opts)`
#'   validating the engine's spec-level options.
#' @param verbs Optional character vector of the evidence-changing verbs the
#'   engine uses: any of `"stratify"`, `"adjust"`, `"measure"`, `"test"`,
#'   `"set_summary"`, and `"missingness"`. Applying another of these verbs to
#'   a result from this engine warns and returns the result unchanged. `NULL`
#'   (the default) accepts every verb. `label()`, `fmt()`, and `style()`
#'   always apply.
#' @param overwrite Logical. Replacing a built-in engine (`descriptive`,
#'   `bivariate`, `glm`, `accuracy`, `roc`, `cox`, `e_value`) is an error unless
#'   `overwrite = TRUE`, because it changes what [table1()], [tb()], and the
#'   other presets compute. Re-registering your own engine needs no flag.
#' @return Invisibly, `name`.
#' @seealso [list_engines()], [is_simtab()]
#' @examples
#' \dontrun{
#' register_engine("dummy", compute = function(spec, data) list(data = data, meta = list()))
#' }
#' @export
register_engine <- function(
  name,
  compute,
  subclass = paste0("simtab_", .normalise_registry_name(name)),
  legacy_tag = NULL,
  validate = NULL,
  renderers = list(),
  engine_opts = NULL,
  verbs = NULL,
  overwrite = FALSE
) {
  key <- .normalise_registry_name(name)
  if (!is.logical(overwrite) || length(overwrite) != 1 || is.na(overwrite)) {
    simtab_abort_input("{.arg overwrite} must be {.code TRUE} or {.code FALSE}.")
  }
  if (!isTRUE(overwrite) && key %in% .builtin_engine_names() &&
      exists(key, envir = .simtab_engine_registry, inherits = FALSE)) {
    simtab_abort_input(c(
      "{.val {key}} is a built-in SimtablR engine.",
      "i" = "Replacing it changes what the built-in presets (e.g. {.fn table1}, {.fn tb}) compute.",
      "v" = "Register your engine under a new name, or pass {.code overwrite = TRUE} to replace it deliberately."
    ))
  }
  entry <- structure(
    list(
      name = key,
      compute = compute,
      subclass = subclass,
      legacy_tag = legacy_tag,
      validate = validate,
      renderers = renderers,
      engine_opts = engine_opts,
      verbs = verbs
    ),
    class = "simtab_engine"
  )
  .validate_simtab_engine(entry)

  assign(key, entry, envir = .simtab_engine_registry)
  invisible(name)
}

#' Names of the engines seeded by SimtablR itself
#' @keywords internal
#' @noRd
.builtin_engine_names <- function() {
  c("bivariate", "descriptive", "glm", "e_value", "accuracy", "roc", "cox")
}

#' List registered SimtablR engines
#'
#' @return A character vector of registered engine names.
#' @seealso [register_engine()]
#' @examples
#' list_engines()
#' @export
list_engines <- function() {
  sort(ls(envir = .simtab_engine_registry))
}

#' Retrieves a registered computation engine by name
#' @keywords internal
#' @noRd
.get_engine <- function(name) {
  key <- .normalise_registry_name(name)
  if (!exists(key, envir = .simtab_engine_registry, inherits = FALSE)) {
    simtab_abort_input(c(
      "Unknown engine {.val {name}}.",
      "i" = "Registered engines: {.val {list_engines()}}.",
      "v" = "Use a registered name, or add yours with {.fn register_engine}."
    ))
  }
  get(key, envir = .simtab_engine_registry, inherits = FALSE)
}

#' Factory for validating engine-specific specification options
#' @keywords internal
#' @noRd
.engine_opts_validator <- function(engine, known_names) {
  force(engine)
  force(known_names)
  function(opts) {
    if (is.null(opts)) {
      opts <- list()
    }
    if (!is.list(opts)) {
      simtab_abort_input(c(
        "Engine options for {.val {engine}} must be a list.",
        "i" = "Received an object of class {.cls {class(opts)[[1]]}}.",
        "v" = "Pass a named list, e.g. {.code list(conf.level = 0.95)}."
      ))
    }
    unknown <- setdiff(names(opts), known_names)
    if (length(unknown) > 0) {
      simtab_abort_input(c(
        "Unknown engine option(s) for {.val {engine}}: {.val {unknown}}.",
        "i" = "Accepted options: {.val {known_names}}.",
        "v" = "Remove the unrecognised option, or check its spelling."
      ))
    }
    invisible(opts)
  }
}

#########
# EFFECT MEASURE REGISTRY
# Registration and lookup for epidemiological effect measures and intervals.

#' Register a SimtablR effect measure
#'
#' Stores an effect-measure computation contract and metadata in the measure
#' registry. Compatibility checks are deferred until the later compute/validate
#' steps.
#'
#' For GLM-adjusted effects, `estimator` names the `family(link)` used by the
#' shared `.fit_glm_effect()` primitive. For crude 2x2 effects, `ci` names the
#' interval method that the descriptive engine maps to `R/epi_measures.R`
#' helpers. Adjusted GLM effects always use Wald intervals on the link scale;
#' `families` lists compatible engines for later validation.
#'
#' Extension measures for crude categorical 2x2 effects may instead supply a
#' pair of functions. `estimator` is called as
#' `function(a_index, n_index, a_ref, n_ref)` and must return a named list with
#' scalar numeric `estimate`; it may also return scalar numeric `null` and `p`,
#' scalar logical `corrected`, and a `meta` list. `ci` is then called as
#' `function(estimate, a_index, n_index, a_ref, n_ref, conf.level)` and must
#' return a named list with scalar numeric `lower` and `upper`; it may also
#' return `p`, `corrected`, and `meta` with the same shapes. All returned values
#' must be raw and unformatted. The descriptive engine currently requires both
#' fields to be functions and supports them for unadjusted categorical effects.
#'
#' @param name Character measure name.
#' @param estimator Character name of the estimator/engine primitive, or a
#'   crude 2x2 estimator function described in Details.
#' @param label Human-readable measure label.
#' @param families Character vector of compatible model families/engines.
#' @param ci Character name of the confidence interval method, or a crude 2x2
#'   interval function described in Details.
#' @return Invisibly, `name`.
#' @keywords internal
#' @noRd
register_measure <- function(name, estimator, label, families, ci) {
  key <- .normalise_registry_name(name)
  estimator_ok <- is.function(estimator) ||
    (is.character(estimator) && length(estimator) == 1 && !is.na(estimator) && nzchar(estimator))
  if (!estimator_ok) {
    simtab_abort_spec(c(
      "{.arg estimator} must be a non-empty string or a function.",
      "i" = "Functions must follow the raw 2x2 estimator contract documented in {.fn register_measure}."
    ))
  }
  if (!is.character(label) || length(label) != 1 || !nzchar(label)) {
    simtab_abort_spec("{.arg label} must be one non-empty string.")
  }
  if (!is.character(families) || length(families) < 1 || any(!nzchar(families))) {
    simtab_abort_spec(c(
      "{.arg families} must be a non-empty character vector.",
      "i" = "List every compatible computation family or engine."
    ))
  }
  ci_ok <- is.function(ci) ||
    (is.character(ci) && length(ci) == 1 && !is.na(ci) && nzchar(ci))
  if (!ci_ok) {
    simtab_abort_spec(c(
      "{.arg ci} must be a non-empty string or a function.",
      "i" = "Functions must follow the raw 2x2 interval contract documented in {.fn register_measure}."
    ))
  }

  value <- structure(
    list(
      name = key,
      estimator = estimator,
      label = label,
      families = families,
      ci = ci
    ),
    class = "simtab_measure"
  )
  assign(key, value, envir = .simtab_measure_registry)
  invisible(name)
}

#' List registered SimtablR measures
#'
#' @return A character vector of registered measure names.
#' @keywords internal
#' @noRd
list_measures <- function() {
  sort(ls(envir = .simtab_measure_registry))
}

#' Retrieves a registered effect measure definition by name
#' @keywords internal
#' @noRd
.get_measure <- function(name) {
  key <- .normalise_registry_name(name)
  if (!exists(key, envir = .simtab_measure_registry, inherits = FALSE)) {
    simtab_abort_input(c(
      "Unknown measure {.val {name}}.",
      "i" = "Registered measures: {.val {list_measures()}}.",
      "v" = "Use one of the registered measures listed above."
    ))
  }
  get(key, envir = .simtab_measure_registry, inherits = FALSE)
}

#########
# BUILT-IN REGISTRATIONS
# Default engine and measure contracts seeded on package initialization.

#' Seeds default engines and epidemiological effect measures into the registries
#' @keywords internal
#' @noRd
.seed_builtin_registries <- function() {
  register_engine(
    "bivariate",
    overwrite = TRUE,
    compute = .engine_tb,
    subclass = "simtab_tb",
    legacy_tag = "tb",
    renderers = .tb_renderers(),
    verbs = c("stratify", "measure", "test", "set_summary", "missingness"),
    engine_opts = .engine_opts_validator(
      "bivariate",
      c(
        "var_names", "flags", "style.rp", "style.or", "subset",
        "subset_env", "strat", "strat_env", "var.type", "stat.cont",
        "labels", "test", "p.adjust", "paired"
      )
    )
  )
  register_engine(
    "descriptive",
    overwrite = TRUE,
    compute = .engine_descriptive,
    subclass = "simtab_table1",
    validate = .validate_descriptive,
    renderers = .table1_renderers(),
    verbs = .evidence_verbs(),
    engine_opts = .engine_opts_validator("descriptive", "var.type")
  )
  register_engine(
    "glm",
    overwrite = TRUE,
    compute = .engine_glm,
    subclass = "simtab_regtab",
    legacy_tag = "regtab",
    validate = .validate_glm,
    renderers = .regtab_renderers(),
    verbs = "adjust",
    engine_opts = .engine_opts_validator(
      "glm",
      c(
        "outcomes", "predictors", "family", "robust", "exponentiate",
        "labels", "predictor_labels", "include_intercept", "p_values",
        "d", "conf.level", "method"
      )
    )
  )
  register_engine(
    "e_value",
    overwrite = TRUE,
    compute = function(spec, data) {
      simtab_abort_engine(c(
        "{.fn e_value} is an extractor and is not computed directly from a specification.",
        "i" = "E-values are derived from an effect estimate that already exists.",
        "v" = "Compute the effect first, then call {.code e_value(result)}."
      ))
    },
    subclass = "simtab_e_value",
    renderers = .e_value_renderers(),
    verbs = character(0)
  )
  register_engine(
    "accuracy",
    overwrite = TRUE,
    compute = .engine_diag,
    subclass = "simtab_diag",
    legacy_tag = "diag_test",
    validate = .validate_accuracy,
    renderers = .diag_renderers(),
    verbs = character(0),
    engine_opts = .engine_opts_validator(
      "accuracy",
      c("positive", "test_positive", "ci", "conf.level", "percent")
    )
  )
  register_engine(
    "roc",
    overwrite = TRUE,
    compute = .engine_roc,
    subclass = "simtab_roc",
    validate = .validate_roc,
    renderers = .roc_renderers(),
    verbs = character(0),
    engine_opts = .engine_opts_validator(
      "roc",
      c("positive", "direction", "ci", "cutpoint", "conf.level", "percent")
    )
  )
  register_engine(
    "cox",
    overwrite = TRUE,
    compute = .engine_cox,
    subclass = "simtab_cox",
    validate = .validate_cox,
    renderers = .cox_renderers(),
    verbs = c("stratify", "adjust"),
    engine_opts = .engine_opts_validator(
      "cox",
      c("predictors", "conf.level", "d", "ties")
    )
  )

  # Measure convention: estimator is the GLM family(link) for adjusted effects,
  # ci is the crude 2x2 interval method, and adjusted GLM CIs are Wald-log.
  # Columns: name, estimator, label, families, ci.
  measures <- list(
    list("OR", "binomial(logit)", "OR", c("glm", "2x2"), "woolf"),
    list("PR", "logbinomial>robust-poisson", "PR", c("glm", "2x2"), "katz"),
    list("RR", "logbinomial>robust-poisson", "RR", c("glm", "2x2"), "katz"),
    list("IRR", "poisson(log)", "IRR", "glm", "wald_log"),
    list("AUC", "pROC", "AUC", "roc", "delong"),
    list("HR", "coxph", "HR", "cox", "wald_log")
  )
  for (m in measures) {
    register_measure(m[[1]], estimator = m[[2]], label = m[[3]], families = m[[4]], ci = m[[5]])
  }
  invisible(TRUE)
}

#' Name the entry point that can compute a measure
#'
#' A built-in measure that the descriptive engine cannot evaluate is not an
#' unregistered measure: it belongs to another engine. Telling the user to
#' register it is not actionable, because it is already registered. This maps
#' the measure's declared families onto the user-facing function that computes
#' it, so the advice names a next step the user can actually take.
#'
#' @param measure A registered measure record.
#' @return A single `cli`-formatted sentence for the error's fix bullet.
#' @keywords internal
#' @noRd
.measure_engine_hint <- function(measure) {
  entry_points <- c(
    cox = "{.fn survtab}",
    roc = "{.fn roc}",
    glm = "{.fn regtab}"
  )
  families <- as.character(measure$families %||% character())
  hits <- entry_points[intersect(families, names(entry_points))]
  label <- toupper(as.character(measure$name %||% measure$label %||% "that measure"))

  if (length(hits) == 0) {
    return(sprintf(
      "Choose a measure the descriptive engine supports, or register %s with a supported estimator.",
      label
    ))
  }
  sprintf(
    "%s is computed by %s, not by a descriptive or bivariate table.",
    label,
    paste(unname(hits), collapse = " or ")
  )
}
