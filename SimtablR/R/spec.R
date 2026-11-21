# SPECIFICATION BUILDER AND GRAMMAR VERBS
# Inert specification constructors and setter verbs for fluent analysis pipelines.

#########
# SPECIFICATION CONSTRUCTOR AND ROLE BINDING
# Initializes simtab_spec objects and binds variables to functional roles.

#' Start a SimtablR analysis specification
#'
#' Captures `data` once into a shared data reference and returns an inert
#' `simtab_spec`. Printing the returned object shows the analysis plan; it never
#' performs estimation.
#'
#' @param data A data frame.
#' @return A `simtab_spec` object.
#' @examples
#' spec <- simtab(epitabl)
#' spec
#' @export
simtab <- function(data) {
  new_simtab_spec(.capture_data_src(data), call = match.call())
}

#' Binds a variable expression to an analysis role slot
#' @keywords internal
#' @noRd
.bind_role <- function(spec, role, quo_or_quos) {
  if (!role %in% names(spec$roles)) {
    simtab_abort_binding(c(
      "Unknown role {.val {role}}.",
      "i" = "Roles defined by the schema: {.val {names(spec$roles)}}.",
      "v" = "Bind one of the listed roles."
    ))
  }
  spec$roles[[role]] <- quo_or_quos
  spec
}

#' Binds a computation engine to the specification
#' @keywords internal
#' @noRd
.bind_engine <- function(spec, name) {
  spec$engine <- .normalise_registry_name(name, "engine")
  spec
}

#' Select a registered computation engine
#'
#' Selects the registered engine used when a specification is computed. Because
#' the engine determines the numerical evidence, applying `engine()` to a
#' computed result updates its stored specification and recomputes through the
#' same path as other evidence-changing verbs.
#'
#' @param spec A `simtab_spec` or `simtab_result`.
#' @param name A single registered engine name. See [list_engines()].
#' @return A modified `simtab_spec`, or a recomputed `simtab_result`.
#' @seealso [register_engine()], [evaluate()]
#' @examples
#' engine(simtab(epitabl), "descriptive")
#' @export
engine <- function(spec, name) {
  UseMethod("engine")
}

#' @export
engine.simtab_spec <- function(spec, name) {
  .engine_update_spec(spec, name)
}

#' Updates specification engine selection with error validation
#' @keywords internal
#' @noRd
.engine_update_spec <- function(spec, name) {
  spec <- validate_simtab_spec(spec)
  if (missing(name) || !is.character(name) || length(name) != 1 || is.na(name) || !nzchar(name)) {
    simtab_abort_spec(c(
      "{.arg name} must be one non-missing, non-empty engine name.",
      "i" = "Computation engines are selected by their registered names.",
      "v" = "Inspect available engines with {.fn list_engines}."
    ))
  }

  key <- tolower(name)
  if (!key %in% list_engines()) {
    simtab_abort_spec(c(
      sprintf("Unknown computation engine {.val %s}.", name),
      "i" = "Only engines registered with {.fn register_engine} can be selected.",
      "v" = "Inspect available engines with {.fn list_engines}."
    ))
  }

  .bind_engine(spec, key)
}

#' @export
engine.simtab_result <- function(spec, name) {
  .reapply_to_result(
    spec,
    function(spec) .engine_update_spec(spec, name),
    call = match.call()
  )
}

#' Attaches engine-specific options into the specification
#' @keywords internal
#' @noRd
.set_engine_opts <- function(spec, engine, opts) {
  if (is.null(opts)) {
    return(spec)
  }
  engine <- .normalise_registry_name(engine, "engine")
  spec$engine_opts[[engine]] <- utils::modifyList(
    spec$engine_opts[[engine]] %||% list(),
    opts,
    keep.null = TRUE
  )
  spec
}

#' Check an effect reference: one scalar level, or a named per-variable map
#' @keywords internal
#' @noRd
.is_valid_effect_ref <- function(ref) {
  scalar_ok <- function(v) length(v) == 1 && !is.list(v) && !anyNA(v)
  nms <- names(ref)
  if (!is.null(nms) && length(ref) >= 1 && all(!is.na(nms) & nzchar(nms))) {
    return(!anyDuplicated(nms) && all(vapply(ref, scalar_ok, logical(1))))
  }
  !is.list(ref) && scalar_ok(ref)
}

#' Format an effect reference for console display
#' @keywords internal
#' @noRd
.format_effect_ref <- function(ref) {
  if (is.null(ref)) {
    return("<unset>")
  }
  if (!is.null(names(ref))) {
    return(paste(sprintf("%s = %s", names(ref), vapply(ref, as.character, character(1))), collapse = ", "))
  }
  as.character(ref)
}

#' Evidence-changing verbs whose support each engine declares
#' @keywords internal
#' @noRd
.evidence_verbs <- function() {
  c("stratify", "adjust", "measure", "test", "set_summary", "missingness")
}

#' Warn and report FALSE when a result's engine does not use a verb
#'
#' Presentation verbs (`label()`, `fmt()`, `style()`) apply everywhere. An
#' evidence verb the engine does not use would only edit the stored
#' specification and recompute the same evidence, so it is refused with a
#' warning instead of being silently accepted.
#' @keywords internal
#' @noRd
.result_supports_verb <- function(result, verb) {
  engine <- result$meta$engine %||% result$spec$engine
  entry <- if (is.null(engine)) NULL else tryCatch(.get_engine(engine), error = function(e) NULL)
  if (is.null(entry) || is.null(entry$verbs) || verb %in% entry$verbs) {
    return(TRUE)
  }
  supported <- intersect(entry$verbs, .evidence_verbs())
  warning(
    sprintf(
      "`%s()` has no effect on a result from the '%s' engine and was ignored. %s",
      verb,
      engine,
      if (length(supported) > 0) {
        sprintf("Verbs this engine uses: %s.", paste0(supported, "()", collapse = ", "))
      } else {
        "This engine uses no evidence-changing verbs; label(), fmt(), and style() still apply."
      }
    ),
    call. = FALSE
  )
  FALSE
}

#' Sets default reference category and confidence level for effect measures
#' @keywords internal
#' @noRd
.set_effect_defaults <- function(spec, ref = NULL, conf.level = NULL) {
  if (!is.null(ref)) {
    spec$effect$ref <- ref
  }
  if (!is.null(conf.level)) {
    spec$effect$conf.level <- conf.level
  }
  spec
}

#########
# ROLE AND COVARIATE SETTER VERBS
# Grammar verbs for describing variables, stratification, and multivariable adjustment.

#' Capture variables to describe
#'
#' @param spec A `simtab_spec`.
#' @param ... Tidyselect expressions for variables to describe.
#' @return A modified `simtab_spec`.
#' @examples
#' describe(simtab(epitabl), age, sex)
#' @export
describe <- function(spec, ...) {
  spec <- validate_simtab_spec(spec)
  quos <- rlang::enquos(..., .ignore_empty = "all")
  if (length(quos) == 0) {
    simtab_abort_binding(c(
      "{.fn describe} requires at least one variable selection.",
      "i" = "No tidyselect expression was supplied.",
      "v" = "Select one or more columns, e.g. {.code describe(simtab(data), age, sex)}."
    ))
  }

  .bind_role(spec, "describe", quos)
}

#' Capture a stratifying variable
#'
#' @param spec A `simtab_spec`.
#' @param by A single data-masked variable.
#' @return A modified `simtab_spec`.
#' @examples
#' stratify(describe(simtab(epitabl), age, sex), adjudicated_acs)
#' @export
stratify <- function(spec, by) {
  UseMethod("stratify")
}

#' @export
stratify.simtab_spec <- function(spec, by) {
  .stratify_update_spec(spec, rlang::enquo(by))
}

#' Binds stratifying variable to specification with validation
#' @keywords internal
#' @noRd
.stratify_update_spec <- function(spec, by_quo) {
  spec <- validate_simtab_spec(spec)
  if (rlang::quo_is_missing(by_quo)) {
    simtab_abort_binding(c(
      "{.fn stratify} requires one stratifying column.",
      "i" = "The {.arg by} argument was not supplied.",
      "v" = "Choose one column, e.g. {.code stratify(simtab(data), disease)}."
    ))
  }

  .bind_role(spec, "by", by_quo)
}

#' @export
stratify.simtab_result <- function(spec, by) {
  if (!.result_supports_verb(spec, "stratify")) {
    return(spec)
  }
  by_quo <- rlang::enquo(by)
  .reapply_to_result(
    spec,
    function(spec) .stratify_update_spec(spec, by_quo),
    call = match.call()
  )
}

#' Capture adjustment covariates
#'
#' @param spec A `simtab_spec`.
#' @param ... Tidyselect expressions for adjustment covariates.
#' @return A modified `simtab_spec`.
#' @examples
#' adjust(simtab(epitabl), age, sex)
#' @export
adjust <- function(spec, ...) {
  UseMethod("adjust")
}

#' @export
adjust.simtab_spec <- function(spec, ...) {
  .adjust_update_spec(spec, rlang::enquos(..., .ignore_empty = "all"))
}

#' Binds adjustment covariates to specification with validation
#' @keywords internal
#' @noRd
.adjust_update_spec <- function(spec, quos) {
  spec <- validate_simtab_spec(spec)
  if (length(quos) == 0) {
    simtab_abort_binding(c(
      "{.fn adjust} requires at least one adjustment column.",
      "i" = "No tidyselect expression was supplied.",
      "v" = "Select one or more columns, e.g. {.code adjust(simtab(data), age, sex)}."
    ))
  }

  .bind_role(spec, "adjust", quos)
}

#' @export
adjust.simtab_result <- function(spec, ...) {
  if (!.result_supports_verb(spec, "adjust")) {
    return(spec)
  }
  quos <- rlang::enquos(..., .ignore_empty = "all")
  .reapply_to_result(
    spec,
    function(spec) .adjust_update_spec(spec, quos),
    call = match.call()
  )
}

#########
# STATISTICAL INFERENCE AND SUMMARY POLICIES
# Verbs for continuous summaries, effect measures, hypothesis tests, and missingness.

#' Set the default summary statistic
#'
#' @param spec A `simtab_spec`.
#' @param stat Summary statistic, one of `"auto"`, `"median"`, or `"mean"`.
#' @param .by_var Optional variable for a per-variable override.
#' @return A modified `simtab_spec`.
#' @examples
#' set_summary(describe(simtab(epitabl), age), "median")
#' @export
set_summary <- function(spec, stat = c("auto", "median", "mean"), .by_var = NULL) {
  UseMethod("set_summary")
}

#' @export
set_summary.simtab_spec <- function(spec, stat = c("auto", "median", "mean"), .by_var = NULL) {
  spec <- validate_simtab_spec(spec)
  stat <- match.arg(stat)
  by_var <- if (is.null(.by_var)) {
    NULL
  } else if (is.character(.by_var) && length(.by_var) == 1) {
    .by_var
  } else {
    as.character(rlang::ensym(.by_var))
  }
  if (is.null(by_var)) {
    spec$summary$default <- stat
    return(spec)
  }
  if (is.na(by_var) || !nzchar(by_var)) {
    simtab_abort_spec(c(
      "{.arg .by_var} must identify one non-missing, non-empty variable name.",
      "i" = "Per-variable summary overrides require a usable named key.",
      "v" = "Use a name such as {.code .by_var = \"age\"}, or {.code NULL} for the default."
    ))
  }
  spec$summary$overrides[[by_var]] <- stat
  spec
}

#' @export
set_summary.simtab_result <- function(spec, stat = c("auto", "median", "mean"), .by_var = NULL) {
  if (!.result_supports_verb(spec, "set_summary")) {
    return(spec)
  }
  stat <- match.arg(stat)
  by_var <- if (is.null(.by_var)) {
    NULL
  } else if (is.character(.by_var) && length(.by_var) == 1) {
    .by_var
  } else {
    as.character(rlang::ensym(.by_var))
  }
  .reapply_to_result(
    spec,
    function(spec) {
      spec <- set_summary(spec, stat, by_var)
      if (identical(spec$engine, "bivariate") && !is.null(spec$engine_opts$bivariate)) {
        spec <- .set_engine_opts(spec, "bivariate", list(stat.cont = spec$summary$default))
      }
      spec
    },
    call = match.call()
  )
}

#' Set an effect measure
#'
#' @param spec A `simtab_spec`.
#' @param m A single effect-measure name.
#' @param ref Optional reference level: one level shared by every variable, or
#'   a named list with one level per variable, e.g.
#'   `list(sex = "Female", smoking = "Never")`.
#' @param conf.level Confidence level between 0 and 1.
#' @param adjust Optional tidyselect adjustment covariates, captured as a
#'   convenience for the builder register.
#' @return A modified `simtab_spec`.
#' @examples
#' measure(simtab(epitabl), "OR", conf.level = 0.90)
#' @export
measure <- function(spec, m, ref = NULL, conf.level = 0.95, adjust = NULL) {
  UseMethod("measure")
}

#' @export
measure.simtab_spec <- function(spec, m, ref = NULL, conf.level = 0.95, adjust = NULL) {
  spec <- validate_simtab_spec(spec)
  adjust_quo <- rlang::enquo(adjust)
  has_adjust <- !missing(adjust) && !identical(rlang::quo_get_expr(adjust_quo), NULL)
  .measure_update_spec(spec, m, ref, conf.level, adjust_quo, has_adjust)
}

#' Validates and binds requested effect measure parameters
#' @keywords internal
#' @noRd
.measure_update_spec <- function(spec, m, ref, conf.level, adjust_quo, has_adjust) {
  if (missing(m) || !is.character(m) || length(m) != 1 || !nzchar(m)) {
    simtab_abort_spec(c(
      "{.arg m} must be a non-empty string.",
      "i" = "Effect measures are named with strings such as {.val OR}, {.val RR}, or {.val PR}.",
      "v" = "Use {.code measure(spec, \"OR\")}."
    ))
  }
  m <- toupper(m)
  tryCatch(
    .get_measure(m),
    error = function(e) simtab_abort_spec(c(
      sprintf("Unknown effect measure {.val %s}.", m),
      "i" = "SimtablR can only compute registered effect measures.",
      "v" = "Use one of {.val OR}, {.val RR}, or {.val PR}."
    ))
  )
  .check_conf_level(conf.level)
  if (!is.null(ref) && !.is_valid_effect_ref(ref)) {
    simtab_abort_spec(c(
      "{.arg ref} must be one non-missing reference value, or a named list with one per variable.",
      "i" = "Each variable's effect measure uses a single reference level.",
      "v" = "Supply {.code ref = \"No\"}, {.code ref = list(sex = \"Female\", smoking = \"Never\")}, or {.code NULL}."
    ))
  }

  spec$effect <- list(
    measure = m,
    estimator = NULL,
    ref = ref,
    conf.level = conf.level,
    resolved_from = "user"
  )

  if (isTRUE(has_adjust)) {
    spec <- .bind_role(spec, "adjust", rlang::new_quosures(list(adjust_quo)))
  }

  spec
}

#' Re-evaluates an existing simtab_result after applying specification edits
#' @keywords internal
#' @noRd
.reapply_to_result <- function(result, edit_fn, call = sys.call(-1)) {
  result <- validate_simtab_result(result)
  updated <- edit_fn(result$spec)
  updated <- validate_simtab_spec(updated)
  data_src <- updated$data_src
  if (!is.environment(data_src$ref) || !exists("data", envir = data_src$ref, inherits = FALSE)) {
    # One condition, not a warning followed by an error saying the same thing.
    simtab_abort_spec(c(
      "Cannot recompute because the captured data is unavailable.",
      "i" = "Editing a result re-runs the analysis, which needs the data the result
             was computed from.",
      "v" = "Rebuild the result from the original data frame."
    ))
  }

  data <- data_src$ref$data
  if (!identical(.hash_data(data), data_src$hash)) {
    warning(
      "Captured data for this result has changed since computation; recomputing with the current captured data.",
      call. = FALSE
    )
  }

  out <- evaluate(updated)
  if (!is.null(result$meta$patches)) {
    out$meta$patches <- result$meta$patches
  }
  out$call <- call
  out
}

#' @export
measure.simtab_result <- function(spec, m, ref = NULL, conf.level = 0.95, adjust = NULL) {
  if (!.result_supports_verb(spec, "measure")) {
    return(spec)
  }
  adjust_quo <- rlang::enquo(adjust)
  has_adjust <- !missing(adjust) && !identical(rlang::quo_get_expr(adjust_quo), NULL)
  # Changing only the measure of a computed result keeps its recorded
  # reference level and confidence level instead of silently resetting them.
  keep_ref <- missing(ref)
  keep_conf <- missing(conf.level)
  .reapply_to_result(
    spec,
    function(spec) {
      if (keep_ref) ref <- spec$effect$ref
      if (keep_conf) conf.level <- spec$effect$conf.level %||% 0.95
      .measure_update_spec(spec, m, ref, conf.level, adjust_quo, has_adjust)
    },
    call = match.call()
  )
}

#' Set a comparison test
#'
#' @param spec A `simtab_spec`.
#' @param method Test method. Defaults to `"auto"`. Categorical methods are
#'   `"chisq"`, `"fisher"`, `"mcnemar"`, and `"trend"`; continuous methods are
#'   `"t"`, `"wilcoxon"`, `"anova"`, and `"kruskal"`. Use `"none"` or `FALSE`
#'   to clear the comparison test. When `method` is omitted but `p.adjust`,
#'   `paired`, or `smd` is supplied, the existing test choice is kept, so
#'   `table1(...) |> test(smd = TRUE)` adds SMDs without adding p-values.
#' @param p.adjust Multiplicity adjustment method, or `NULL` to leave unchanged.
#' @param paired Logical; whether paired tests are requested.
#' @param smd Logical; whether standardized mean differences are requested.
#' @return A modified `simtab_spec`.
#' @examples
#' test(stratify(describe(simtab(epitabl), age), adjudicated_acs), "wilcoxon")
#' @export
test <- function(spec, method = "auto", p.adjust = NULL, paired = NULL, smd = NULL) {
  UseMethod("test")
}

#' @export
test.simtab_spec <- function(spec, method = "auto", p.adjust = NULL, paired = NULL, smd = NULL) {
  spec <- validate_simtab_spec(spec)
  if (missing(method) && !(is.null(p.adjust) && is.null(paired) && is.null(smd))) {
    method <- spec$comparison$test %||% FALSE
  }
  .check_optional_logical(paired, "paired")
  .check_optional_logical(smd, "smd")
  if (identical(method, "none") || isFALSE(method)) {
    spec$comparison$test <- NULL
  } else {
    if (isTRUE(method)) {
      method <- "auto"
    }
    if (!is.character(method) || length(method) != 1 || !nzchar(method)) {
      simtab_abort_spec(c(
        "{.arg method} must be a single string.",
        "i" = "Comparison tests are selected by method name.",
        "v" = "Use {.code test(spec, \"auto\")} or {.code test(spec, \"fisher\")}."
      ))
    }
    method <- tolower(method)
    valid_methods <- c("auto", "chisq", "fisher", "mcnemar", "trend", "wilcoxon", "t", "anova", "kruskal")
    if (!method %in% valid_methods) {
      simtab_abort_spec(c(
        sprintf("Unknown test method {.val %s}.", method),
        "i" = "SimtablR validates test names before computing the table.",
        "v" = sprintf("Use one of: %s.", paste(valid_methods, collapse = ", "))
      ))
    }
    spec$comparison$test <- method
  }
  if (!is.null(p.adjust)) {
    spec$comparison$p.adjust <- match.arg(
      p.adjust,
      c("none", "holm", "bonferroni", "BH", "BY", "hochberg")
    )
  }
  if (!is.null(paired)) {
    spec$comparison$paired <- paired
  }
  if (!is.null(smd)) {
    spec$comparison$smd <- smd
  }
  spec
}

#' @export
test.simtab_result <- function(spec, method = "auto", p.adjust = NULL, paired = NULL, smd = NULL) {
  if (!.result_supports_verb(spec, "test")) {
    return(spec)
  }
  keep_method <- missing(method)
  .reapply_to_result(
    spec,
    function(spec) {
      spec <- if (keep_method) {
        test(spec, p.adjust = p.adjust, paired = paired, smd = smd)
      } else {
        test(spec, method = method, p.adjust = p.adjust, paired = paired, smd = smd)
      }
      if (identical(spec$engine, "bivariate") && !is.null(spec$engine_opts$bivariate)) {
        bivariate_opts <- spec$engine_opts$bivariate
        spec <- .set_engine_opts(
          spec,
          "bivariate",
          list(
            test = .comparison_test_arg(spec$comparison$test),
            p.adjust = spec$comparison$p.adjust %||% bivariate_opts$p.adjust %||% "none",
            paired = spec$comparison$paired %||% bivariate_opts$paired %||% FALSE
          )
        )
      }
      spec
    },
    call = match.call()
  )
}

#' Set missing-data policy
#'
#' @param spec A `simtab_spec`.
#' @param display Whether to display missingness rows.
#' @param denominator Missing-data denominator policy, `"available"` or
#'   `"complete"`.
#' @param model Missing-data model policy, `"drop"`, `"fail"`, or
#'   `"explicit"`.
#' @return A modified `simtab_spec`.
#' @examples
#' missingness(simtab(epitabl), display = TRUE, denominator = "available")
#' @export
missingness <- function(spec, display = NULL, denominator = NULL, model = NULL) {
  UseMethod("missingness")
}

#' @export
missingness.simtab_spec <- function(spec, display = NULL, denominator = NULL, model = NULL) {
  spec <- validate_simtab_spec(spec)
  .check_optional_logical(display, "display")
  if (!is.null(display)) {
    spec$missing$display <- display
  }
  if (!is.null(denominator)) {
    valid_denominator <- c("available", "complete")
    if (!is.character(denominator) || length(denominator) != 1 || !denominator %in% valid_denominator) {
      simtab_abort_spec(c(
        sprintf("Unknown missingness denominator {.val %s}.", as.character(denominator)[1] %||% "<missing>"),
        "i" = "The denominator controls how missing rows are counted in displayed summaries.",
        "v" = "Use {.code missingness(spec, denominator = \"available\")} or {.code denominator = \"complete\"}."
      ))
    }
    spec$missing$denominator <- denominator
  }
  if (!is.null(model)) {
    valid_model <- c("drop", "fail", "explicit")
    if (!is.character(model) || length(model) != 1 || !model %in% valid_model) {
      simtab_abort_spec(c(
        sprintf("Unknown model missingness policy {.val %s}.", as.character(model)[1] %||% "<missing>"),
        "i" = "The model policy controls how model-fitting handles rows with missing covariates.",
        "v" = "Use {.code missingness(spec, model = \"drop\")}, {.code \"fail\"}, or {.code \"explicit\"}."
      ))
    }
    spec$missing$model_na <- model
  }
  spec
}

#' Validates that an optional argument is a single logical or NULL
#' @keywords internal
#' @noRd
.check_optional_logical <- function(value, arg) {
  if (!is.null(value) &&
      (!is.logical(value) || length(value) != 1 || is.na(value))) {
    simtab_abort_spec(c(
      sprintf("{.arg %s} must be TRUE or FALSE when supplied.", arg),
      "i" = "Missing, non-logical, and non-scalar values are not logical controls.",
      "v" = sprintf("Use {.code %s = TRUE}, {.code %s = FALSE}, or {.code NULL} to leave it unchanged.", arg, arg)
    ))
  }
  invisible(NULL)
}

#' @export
missingness.simtab_result <- function(spec, display = NULL, denominator = NULL, model = NULL) {
  if (!.result_supports_verb(spec, "missingness")) {
    return(spec)
  }
  .reapply_to_result(
    spec,
    function(spec) {
      spec <- missingness(spec, display = display, denominator = denominator, model = model)
      if (identical(spec$engine, "bivariate") && !is.null(spec$engine_opts$bivariate)) {
        bivariate_opts <- spec$engine_opts$bivariate
        spec <- .set_engine_opts(
          spec,
          "bivariate",
          list(flags = .tb_flags_from_state(
            spec,
            pct_requested = isTRUE(bivariate_opts$flags$percent)
          ))
        )
      }
      spec
    },
    call = match.call()
  )
}

#########
# FORMATTING, LABELS, AND JOURNAL STYLING
# Verbs for layout margins, variable labels, number formatting, and journal themes.

#' Toggle the overall column
#'
#' @param spec A `simtab_spec`.
#' @param value `TRUE` or `FALSE`.
#' @return A modified `simtab_spec`.
#' @examples
#' overall(simtab(epitabl), FALSE)
#' @export
overall <- function(spec, value = TRUE) {
  UseMethod("overall")
}

#' @export
overall.simtab_spec <- function(spec, value = TRUE) {
  spec <- validate_simtab_spec(spec)
  if (!is.logical(value) || length(value) != 1 || is.na(value)) {
    simtab_abort_input(c(
      "{.arg value} must be {.code TRUE} or {.code FALSE}.",
      "i" = "Received: {.val {value}}.",
      "v" = "Use {.code overall(spec, FALSE)} to drop the overall column."
    ))
  }

  spec$layout$overall <- value
  spec
}

#' @export
overall.simtab_result <- function(spec, value = TRUE) {
  .reapply_to_result(
    spec,
    function(spec) overall(spec, value = value),
    call = match.call()
  )
}

#' Set display labels
#'
#' @param spec A `simtab_spec`.
#' @param ... Named character labels.
#' @param labels Optional named character vector of labels.
#' @return A modified `simtab_spec`.
#' @details Label edits merge by variable name: supplied labels replace matching
#'   entries and all unspecified recorded labels remain unchanged. On computed
#'   results, this is a presentation-only edit; raw evidence is retained and
#'   the result call records the edit.
#' @examples
#' label(simtab(epitabl), age = "Age, years", sex = "Sex")
#' @export
label <- function(spec, ..., labels = NULL) {
  UseMethod("label")
}

#' @export
label.simtab_spec <- function(spec, ..., labels = NULL) {
  spec <- validate_simtab_spec(spec)
  dots <- list(...)
  if (!is.null(labels)) {
    if (!is.character(labels)) {
      simtab_abort_spec(c(
        "{.arg labels} must be a named character vector.",
        "i" = "Publication labels are recorded as one string per variable.",
        "v" = "Use {.code labels = c(age = \"Age, years\")} ."
      ))
    }
    dots <- c(dots, as.list(labels))
  }
  # `c(age = "Age")` is the form every other entry point takes (`table1(labels=)`,
  # `fmt(labels=)`), so accept it positionally too rather than reporting the
  # inner names as missing.
  unnamed <- if (is.null(names(dots))) seq_along(dots) else which(!nzchar(names(dots)))
  promoted <- unnamed[vapply(
    dots[unnamed],
    function(value) {
      is.character(value) && length(value) >= 1 &&
        !is.null(names(value)) && all(nzchar(names(value)))
    },
    logical(1)
  )]
  if (length(promoted) > 0) {
    dots <- c(dots[-promoted], unlist(lapply(dots[promoted], as.list), recursive = FALSE))
  }
  if (length(dots) == 0) {
    simtab_abort_spec(c(
      "{.fn label} requires at least one named label.",
      "v" = "Use {.code label(spec, age = \"Age, years\")} ."
    ))
  }
  label_names <- names(dots)
  if (is.null(label_names) || anyNA(label_names) || any(!nzchar(label_names))) {
    simtab_abort_spec(c(
      "Every publication label must have a non-missing, non-empty variable name.",
      "i" = "Names come either from the argument names or from a named character vector.",
      "v" = "Use {.code label(spec, age = \"Age, years\")} or {.code label(spec, labels = c(age = \"Age, years\"))}."
    ))
  }
  if (anyDuplicated(label_names)) {
    simtab_abort_spec(c(
      "Publication label names must be unique within one edit.",
      "i" = "A variable was labelled more than once in the same call.",
      "v" = "Supply one label per variable name."
    ))
  }
  valid_values <- vapply(
    dots,
    function(value) {
      is.character(value) && length(value) == 1 && !is.na(value) && nzchar(value)
    },
    logical(1)
  )
  if (!all(valid_values)) {
    simtab_abort_spec(c(
      "Every publication label must be one non-missing, non-empty character string.",
      "i" = "Vectors, missing values, empty strings, and non-character values are not labels.",
      "v" = "Use a scalar label such as {.code age = \"Age, years\"}."
    ))
  }
  values <- stats::setNames(
    vapply(dots, function(value) value[[1]], character(1)),
    label_names
  )

  merged <- spec$fmt$labels %||% setNames(character(), character())
  merged[names(values)] <- values
  spec$fmt$labels <- merged
  spec
}

#' @export
label.simtab_result <- function(spec, ..., labels = NULL) {
  result <- validate_simtab_result(spec)
  result$spec <- label(result$spec, ..., labels = labels)
  result <- .sync_result_labels(result)
  result$call <- match.call()
  result
}

#' Synchronises updated variable labels across result metadata slots
#' @keywords internal
#' @noRd
.sync_result_labels <- function(result) {
  labels <- result$spec$fmt$labels
  result$meta$labels[names(labels)] <- labels
  if (!is.null(result$meta$args)) {
    result$meta$args$labels <- labels
  }

  # Bivariate rendering stores the resolved row/column headers separately
  # from the general publication-label map. Keep those presentation fields in
  # step without touching the raw frequency/estimate evidence.
  row_var <- result$meta$row_var_name %||% NULL
  if (!is.null(row_var) && row_var %in% names(labels)) {
    result$meta$row_label <- unname(labels[[row_var]])
  }
  col_var <- result$meta$col_var_name %||% NULL
  # A stratified bivariate table records its column as "<var> (Stratified)";
  # match the label on the underlying variable and keep the suffix.
  suffix <- " (Stratified)"
  stratified <- isTRUE(result$meta$is_stratified) && !is.null(col_var) &&
    endsWith(col_var, suffix)
  if (stratified) {
    col_var <- substr(col_var, 1L, nchar(col_var) - nchar(suffix))
  }
  if (!is.null(col_var) && col_var %in% names(labels)) {
    result$meta$col_label <- paste0(unname(labels[[col_var]]), if (stratified) suffix)
  }
  result
}

#' Set render formatting options
#'
#' @param spec A `simtab_spec`.
#' @param d Decimal places for percentages/estimates, or `NULL` to leave
#'   unchanged.
#' @param conf_pct Confidence-interval percentage label, or `NULL` to leave
#'   unchanged.
#' @param labels Optional named character labels, merged by variable name
#'   through [label()].
#' @param percent Logical, or `NULL` to leave unchanged. When `TRUE`,
#'   diagnostic and ROC proportion metrics render as percentages. Other engines
#'   ignore this field.
#' @param big_mark Character inserted between every three digits of the integer
#'   part of rendered numbers (e.g. `","` yields `4,391.2`), or `NULL` to leave
#'   unchanged. Default `""` (no grouping). Currently honoured by [tb()].
#' @param decimal_mark Character used as the radix point in rendered numbers
#'   (e.g. `","` for many European locales), or `NULL` to leave unchanged.
#'   Default `"."`. Must differ from `big_mark`. Currently honoured by [tb()].
#' @return A modified `simtab_spec`.
#' @examples
#' fmt(simtab(epitabl), d = 1, conf_pct = 95)
#' fmt(simtab(epitabl), big_mark = ",", decimal_mark = ".")
#' @export
fmt <- function(spec, d = NULL, conf_pct = NULL, labels = NULL, percent = NULL,
                big_mark = NULL, decimal_mark = NULL) {
  UseMethod("fmt")
}

#' Validate a display separator argument (`big_mark` / `decimal_mark`).
#' @keywords internal
#' @noRd
.check_number_mark <- function(value, arg) {
  if (is.null(value)) {
    return(invisible(NULL))
  }
  if (!is.character(value) || length(value) != 1 || is.na(value)) {
    simtab_abort_spec(c(
      sprintf("{.arg %s} must be a single string.", arg),
      "i" = "It sets a display separator, e.g. {.val ,} or {.val .}.",
      "v" = sprintf("Use {.code fmt(spec, %s = \",\")}.", arg)
    ))
  }
  invisible(NULL)
}

#' @export
fmt.simtab_spec <- function(spec, d = NULL, conf_pct = NULL, labels = NULL, percent = NULL,
                            big_mark = NULL, decimal_mark = NULL) {
  spec <- validate_simtab_spec(spec)
  .check_number_mark(big_mark, "big_mark")
  .check_number_mark(decimal_mark, "decimal_mark")
  if (!is.null(big_mark)) {
    spec$fmt$big_mark <- big_mark
  }
  if (!is.null(decimal_mark)) {
    spec$fmt$decimal_mark <- decimal_mark
  }
  bm <- spec$fmt$big_mark %||% ""
  dm <- spec$fmt$decimal_mark %||% "."
  if (nzchar(bm) && identical(bm, dm)) {
    simtab_abort_spec(c(
      "{.arg big_mark} and {.arg decimal_mark} must differ.",
      "i" = sprintf("Both resolve to {.val %s}.", bm),
      "v" = "Use e.g. {.code fmt(spec, big_mark = \",\", decimal_mark = \".\")}."
    ))
  }
  if (!is.null(d)) {
    if (!is.numeric(d) || length(d) != 1 || is.na(d) || d < 0 || d > 10) {
      simtab_abort_spec(c(
        sprintf("{.arg d} must be a number between 0 and 10, not {.val %s}.", as.character(d)[1] %||% "<missing>"),
        "i" = "{.arg d} controls displayed decimal places.",
        "v" = "Use {.code fmt(spec, d = 1)}."
      ))
    }
    spec$fmt$d <- as.integer(d)
    # Recorded so renderers can tell a requested `d` from the constructor
    # default; table1() then applies it to continuous summaries as well.
    spec$fmt$d_explicit <- TRUE
  }
  if (!is.null(conf_pct)) {
    if (!is.numeric(conf_pct) || length(conf_pct) != 1 || is.na(conf_pct) || !is.finite(conf_pct)) {
      simtab_abort_spec(c(
        "{.arg conf_pct} must be one finite number.",
        "i" = "The value labels the confidence-interval percentage in rendered output.",
        "v" = "Use a finite value such as {.code conf_pct = 95}."
      ))
    }
    spec$fmt$conf_pct <- conf_pct
  }
  if (!is.null(percent)) {
    if (!is.logical(percent) || length(percent) != 1 || is.na(percent)) {
      simtab_abort_spec(c(
        "{.arg percent} must be a single {.code TRUE} or {.code FALSE}.",
        "i" = "It switches diagnostic and ROC proportion metrics to a percentage display.",
        "v" = "Use {.code fmt(spec, percent = TRUE)}."
      ))
    }
    spec$fmt$percent <- percent
  }
  if (!is.null(labels)) {
    spec <- label(spec, labels = labels)
  }
  spec
}

#' @export
fmt.simtab_result <- function(spec, d = NULL, conf_pct = NULL, labels = NULL, percent = NULL,
                              big_mark = NULL, decimal_mark = NULL) {
  result <- validate_simtab_result(spec)
  result$spec <- fmt(
    result$spec, d = d, conf_pct = conf_pct, labels = labels, percent = percent,
    big_mark = big_mark, decimal_mark = decimal_mark
  )
  if (!is.null(big_mark)) {
    result$meta$big_mark <- result$spec$fmt$big_mark
  }
  if (!is.null(decimal_mark)) {
    result$meta$decimal_mark <- result$spec$fmt$decimal_mark
  }
  if (!is.null(d)) {
    result$meta$d <- result$spec$fmt$d
    if (!is.null(result$meta$args)) {
      result$meta$args$d <- result$spec$fmt$d
    }
  }
  if (!is.null(conf_pct)) {
    result$meta$conf_pct <- result$spec$fmt$conf_pct
  }
  if (!is.null(percent)) {
    result$meta$percent <- result$spec$fmt$percent
  }
  if (!is.null(labels)) {
    result <- .sync_result_labels(result)
  }
  result$call <- match.call()
  result
}

#' Set or update a SimtablR style
#'
#' @param x A SimtablR object.
#' @param journal A single style or journal preset name.
#' @return A modified SimtablR object.
#' @examples
#' style(simtab(epitabl), "default")
#' @export
style <- function(x, journal) {
  UseMethod("style")
}

#' @export
style.simtab_spec <- function(x, journal) {
  x <- validate_simtab_spec(x)
  valid_style <- inherits(journal, "simtab_style") ||
    (is.character(journal) && length(journal) == 1 && nzchar(journal)) ||
    is.list(journal)
  if (!isTRUE(valid_style)) {
    simtab_abort_input(c(
      "{.arg journal} must be a single string, a {.cls simtab_style}, or a named list.",
      "i" = "Received an object of class {.cls {class(journal)[[1]]}}.",
      "v" = "Use {.code style(spec, \"lancet\")}, or build one with {.fn journal_style}."
    ))
  }

  x$style <- journal
  x
}

#########
# INSPECTION AND TIDYSELECT RESOLUTION HELPERS
# Console printing and lazy tidyselect evaluation for specification roles.

#' @export
print.simtab_spec <- function(x, ...) {
  x <- validate_simtab_spec(x)
  cat("<simtab_spec>\n")
  cat("  not yet computed\n")
  cat(
    "  data: ", x$data_src$nrow, " rows x ", x$data_src$ncol,
    " columns; hash ", substr(x$data_src$hash, 1, 12), "\n",
    sep = ""
  )
  cat("  describe: ", .format_quosures(x$roles$describe), "\n", sep = "")
  cat("  stratify/by: ", .format_quosure(x$roles$by), "\n", sep = "")
  cat("  adjust: ", .format_quosures(x$roles$adjust), "\n", sep = "")
  cat("  summary: ", x$summary$default, "\n", sep = "")
  cat(
    "  measure: ",
    if (is.null(x$effect$measure)) "<unset>" else x$effect$measure,
    " (ref: ",
    .format_effect_ref(x$effect$ref),
    ", conf.level: ",
    x$effect$conf.level,
    ")\n",
    sep = ""
  )
  cat("  test: ", if (is.null(x$comparison$test)) "<unset>" else x$comparison$test, "\n", sep = "")
  cat("  design: ", if (is.null(x$design)) "<unset>" else x$design, "\n", sep = "")
  cat("  style: ", x$style, "\n", sep = "")
  invisible(x)
}

#' Formats a single captured quosure for console display
#' @keywords internal
#' @noRd
.format_quosure <- function(quo) {
  if (is.null(quo)) {
    return("<unset>")
  }
  rlang::as_label(quo)
}

#' Formats multiple captured quosures for console display
#' @keywords internal
#' @noRd
.format_quosures <- function(quos) {
  if (is.null(quos) || length(quos) == 0) {
    return("<unset>")
  }
  paste(vapply(quos, rlang::quo_text, character(1)), collapse = ", ")
}

#' Normalise a captured quosure so plain character values keep working
#'
#' Every surface that binds variables (`describe()`/`stratify()`/`adjust()`,
#' `table1()`, `tb()`'s dots, `diag_test()`/`roc()`) captures a quosure and
#' resolves it through this shim before handing it to
#' `tidyselect::eval_select()`. A captured expression that evaluates to a
#' character vector (a string literal, or a bare symbol naming a character
#' vector in its own environment) is rewritten as `tidyselect::all_of(<value>)`
#' so it selects by name rather than erroring or warning as an "external
#' vector" selection.
#'
#' A bare symbol that already names a column in `data` is never promoted, even
#' if a same-named character vector also exists in the quosure's environment:
#' the column wins (this matches `tidyselect::eval_select()`'s own data-first
#' precedence, and the package's loud-not-silent rule — silently binding to an
#' unrelated environment variable when a column of that name exists would be a
#' surprise, not a convenience).
#'
#' @param quo An `rlang` quosure.
#' @param data The data.frame the quosure will be resolved against.
#' @return A quosure: either `quo` unchanged, or a new quosure wrapping
#'   `tidyselect::all_of(<value>)`.
#' @keywords internal
#' @noRd
.as_select_quo <- function(quo, data) {
  expr <- rlang::quo_get_expr(quo)
  if (rlang::is_symbol(expr) && as.character(expr) %in% names(data)) {
    return(quo)
  }

  val <- suppressWarnings(tryCatch(rlang::eval_tidy(quo), error = function(e) NULL))
  if (is.character(val)) {
    # Quosures of literal constants carry the empty environment (rlang docs:
    # "constants don't need any scope"), so referencing `all_of` by the
    # function value itself avoids a `tidyselect::` lookup that would fail
    # to resolve `::` in that environment.
    return(rlang::new_quosure(
      rlang::call2(tidyselect::all_of, val),
      rlang::quo_get_env(quo)
    ))
  }
  quo
}

#' Evaluates multiple tidyselect quosures against the source dataset
#' @keywords internal
#' @noRd
.eval_select_quos <- function(quos, data) {
  quos <- lapply(quos, .as_select_quo, data = data)
  expr <- if (length(quos) == 1) {
    rlang::quo_get_expr(quos[[1]])
  } else {
    # Keep the quosures nested in the combined selection. Tidyselect then
    # evaluates each branch in its own lexical environment instead of
    # flattening every expression into the first quosure's environment.
    rlang::call2("c", !!!quos)
  }
  env <- if (length(quos) == 1) rlang::quo_get_env(quos[[1]]) else rlang::empty_env()
  tryCatch(
    names(tidyselect::eval_select(expr, data = data, env = env)),
    error = function(e) simtab_abort_binding(c(
      "Could not resolve a tidyselect variable selection.",
      "i" = conditionMessage(e),
      "v" = "Check the column name, or pass a character vector with {.code all_of()}."
    ))
  )
}

#' Resolves column names for a tidyselect role slot
#' @keywords internal
#' @noRd
.resolve_tidyselect_role <- function(spec, role) {
  quos <- spec$roles[[role]]
  if (is.null(quos)) {
    return(character())
  }
  if (!inherits(quos, "quosures")) {
    simtab_abort_binding(c(
      "Role {.val {role}} is not a tidyselect role.",
      "i" = "It was bound as a single data-masked expression rather than a selection.",
      "v" = "Resolve it with {.fn .resolve_single_role} instead."
    ))
  }

  selected <- .eval_select_quos(quos, spec$data_src$ref$data)
  if (identical(role, "adjust") && length(selected) == 0) {
    simtab_abort_binding(c(
      "{.fn adjust} must select at least one adjustment column.",
      "i" = "The supplied tidyselect expression resolved to zero columns.",
      "v" = "Check the selector or omit {.fn adjust} for an unadjusted analysis."
    ))
  }
  selected
}

#' Append `adjust()` covariates to a model right-hand-side formula
#'
#' Model engines (GLM, Cox) carry their predictors as a formula; covariates
#' bound later through the `adjust()` verb live in `roles$adjust`. This adds
#' each selected column not already used on the right-hand side so the verb
#' reaches the fitted model rather than only the recorded specification.
#' @keywords internal
#' @noRd
.formula_with_adjust <- function(predictors, spec) {
  if (is.null(predictors) || is.null(spec$roles$adjust)) {
    return(predictors)
  }
  rhs <- predictors[[length(predictors)]]
  extra <- setdiff(.resolve_tidyselect_role(spec, "adjust"), all.vars(rhs))
  for (col in extra) {
    rhs <- call("+", rhs, as.name(col))
  }
  predictors[[length(predictors)]] <- rhs
  predictors
}

#' Resolves a single column name for a scalar data-masked role slot
#' @keywords internal
#' @noRd
.resolve_single_role <- function(spec, role) {
  quo <- spec$roles[[role]]
  if (is.null(quo)) {
    return(NULL)
  }
  if (!rlang::is_quosure(quo)) {
    simtab_abort_binding(c(
      "Role {.val {role}} is not a data-masked role.",
      "i" = "It was bound as a tidyselect selection rather than a single expression.",
      "v" = "Resolve it with {.fn .resolve_tidyselect_role} instead."
    ))
  }

  data <- spec$data_src$ref$data
  quo <- .as_select_quo(quo, data)
  sel <- tryCatch(
    tidyselect::eval_select(
      rlang::quo_get_expr(quo), data = data, env = rlang::quo_get_env(quo)
    ),
    error = function(e) simtab_abort_binding(c(
      sprintf("Could not resolve role {.val %s}.", role),
      "i" = conditionMessage(e),
      "v" = "Check the column name, e.g. {.code stratify(spec, disease)}."
    ))
  )
  if (length(sel) != 1) {
    simtab_abort_binding(c(
      sprintf("Role {.val %s} must select exactly one column, not %d.", role, length(sel)),
      "i" = "This role is used where SimtablR needs a single data column.",
      "v" = sprintf("Select one column for {.val %s}, e.g. {.code stratify(spec, disease)}.", role)
    ))
  }
  names(sel)
}
