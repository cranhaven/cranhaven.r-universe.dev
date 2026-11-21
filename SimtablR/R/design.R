# Study Design Resolution and Metadata
#
# Epidemiological study design specification, canonical name mapping,
# design-to-effect measure resolution rules (PR, RR, OR, IRR, HR), and metadata accessors.

.simtab_design_attr <- "simtablr.design"

#########
# CANONICAL STUDY DESIGN SPECIFICATION
# Normalization of study design identifiers and epidemiological measure mapping.

#' Normalize study design string to canonical epidemiological identifier
#' @keywords internal
#' @noRd
.normalise_design <- function(design, allow_null = TRUE) {
  if (is.null(design)) {
    if (isTRUE(allow_null)) {
      return(NULL)
    }
    simtab_abort_spec(c(
      "{.arg design} must be supplied.",
      "i" = "The study design determines which effect measure is appropriate.",
      "v" = "Use {.code design = \"cohort\"}, {.val case-control}, or {.val cross-sectional}."
    ))
  }
  if (!is.character(design) || length(design) != 1 || is.na(design) || !nzchar(trimws(design))) {
    simtab_abort_spec(c(
      "{.arg design} must be one non-missing, non-empty string.",
      "v" = "Use {.val cross_sectional}, {.val cohort}, {.val case_control}, {.val cohort_person_time}, or {.val cohort_time_to_event}."
    ))
  }

  key <- tolower(gsub("[ -]+", "_", trimws(design)))
  canonical <- c(
    "cross_sectional", "cohort", "case_control",
    "cohort_person_time", "cohort_time_to_event"
  )
  out <- if (key %in% canonical) key else NA_character_
  if (is.na(out)) {
    simtab_abort_spec(c(
      sprintf("Unknown study design {.val %s}.", design),
      "v" = "Use {.val cross_sectional}, {.val cohort}, {.val case_control}, {.val cohort_person_time}, or {.val cohort_time_to_event}."
    ))
  }
  out
}

#' Map canonical study design key to publication-ready prose label
#' @keywords internal
#' @noRd
.design_label <- function(design) {
  design <- .normalise_design(design)
  switch(
    design,
    cross_sectional = "cross-sectional",
    cohort = "cohort",
    case_control = "case-control",
    cohort_person_time = "cohort person-time",
    cohort_time_to_event = "cohort time-to-event",
    design
  )
}

#' Map canonical study design to default epidemiological effect measure
#' @keywords internal
#' @noRd
.design_measure_decision <- function(design) {
  design <- .normalise_design(design)
  switch(
    design,
    cross_sectional = "PR",
    cohort = "RR",
    case_control = "OR",
    cohort_person_time = "IRR",
    cohort_time_to_event = "HR",
    NULL
  )
}

#' Set study design metadata
#'
#' Records a study design on a data frame or a `simtab_spec`. The two forms
#' behave differently: on a `simtab_spec`, the design participates in
#' `evaluate()`'s effect-measure resolution, so a call such as
#' `set_design(spec, "cohort") |> evaluate()` can pick RR over an unset
#' measure. On a data frame, the design is recorded only as an attribute for
#' educator advice (methodological guidance rules can read it); it is *not*
#' consulted when resolving an effect measure, even for a `tb()`/`table1()`
#' call built directly from that data frame. To resolve a measure from
#' design, pass `design =` to the direct function or call `set_design()` on a
#' `simtab_spec` (see `vignette("study-design-effect-measures")`).
#'
#' @param data A data.frame or `simtab_spec`.
#' @param design Study design, e.g. `"cross_sectional"`, `"cohort"`, or
#'   `"case_control"`.
#' @return `data` with a SimtablR design attribute, or a modified
#'   `simtab_spec`.
#' @examples
#' set_design(simtab(epitabl), "cohort")
#' cohort_data <- set_design(epitabl, "cohort") # advisory only; see Details
#' @export
set_design <- function(data, design) {
  UseMethod("set_design")
}

#' @export
set_design.data.frame <- function(data, design) {
  .check_data_frame(data)
  attr(data, .simtab_design_attr) <- .normalise_design(design, allow_null = FALSE)
  data
}

#' @export
set_design.simtab_spec <- function(data, design) {
  spec <- validate_simtab_spec(data)
  spec["design"] <- list(.normalise_design(design, allow_null = FALSE))
  spec
}

#' Extract study design attribute from data frame
#' @keywords internal
#' @noRd
.data_design <- function(data) {
  .normalise_design(attr(data, .simtab_design_attr, exact = TRUE), allow_null = TRUE)
}

#' Read a recorded study design
#'
#' Returns the study design recorded by `set_design()`, without requiring
#' callers to read the internal `"simtablr.design"` attribute directly.
#' On a data frame this reads the advisory attribute; on a `simtab_spec` or
#' `simtab_result` it reads the specification's `design` field, falling back
#' to the source data frame's attribute for a `simtab_result` (matching what
#' `repro_manifest()` reports as `design`/`design_source`). It does not
#' indicate whether the design took part in effect-measure resolution; see
#' `resolved_from` on `spec$effect` or `design_used` in `repro_manifest()`
#' for that.
#'
#' @param x A data.frame, `simtab_spec`, or `simtab_result`.
#' @return A single design string, or `NULL` if none is recorded.
#' @examples
#' cohort <- set_design(epitabl, "cohort")
#' design(cohort)
#' @export
design <- function(x) {
  UseMethod("design")
}

#' @export
design.data.frame <- function(x) {
  .data_design(x)
}

#' @export
design.simtab_spec <- function(x) {
  x <- validate_simtab_spec(x)
  .normalise_design(x$design, allow_null = TRUE)
}

#' @export
design.simtab_result <- function(x) {
  .result_design(x)
}

#' Resolve recorded study design from result specification or underlying data
#' @keywords internal
#' @noRd
.result_design <- function(result) {
  result <- validate_simtab_result(result)
  spec_design <- .normalise_design(result$spec$design, allow_null = TRUE)
  if (!is.null(spec_design)) {
    return(spec_design)
  }
  .data_design(result$used$ref$data)
}

#' Identify provenance source of resolved study design
#' @keywords internal
#' @noRd
.result_design_source <- function(result) {
  if (!is.null(.normalise_design(result$spec$design, allow_null = TRUE))) {
    return("call")
  }
  if (!is.null(.data_design(result$used$ref$data))) {
    return("data")
  }
  NULL
}

#########
# EFFECT MEASURE DESIGN RESOLUTION
# Automatic resolution of association measures from declared study design.

#' Check whether vector has exactly two unique non-empty levels
#' @keywords internal
#' @noRd
.has_binary_values <- function(x) {
  length(levels(droplevels(factor(x)))) == 2
}

#' Check whether candidate effect measure is valid for engine and data roles
#' @keywords internal
#' @noRd
.design_effect_supported <- function(spec, data, measure) {
  engine <- spec$engine
  if (is.null(engine)) {
    engine <- tryCatch(.resolve_engine(spec), error = function(e) NULL)
  }
  if (is.null(engine)) {
    return(FALSE)
  }
  engine <- .normalise_registry_name(engine, "engine")

  if (identical(engine, "descriptive")) {
    by <- .resolve_single_role(spec, "by")
    return(measure %in% c("PR", "RR", "OR") &&
      !is.null(by) && by %in% names(data) && .has_binary_values(data[[by]]))
  }

  if (identical(engine, "bivariate")) {
    vars <- .resolve_tidyselect_role(spec, "describe")
    return(measure %in% c("PR", "RR", "OR") &&
      length(vars) >= 2 && vars[[2]] %in% names(data) && .has_binary_values(data[[vars[[2]]]]))
  }

  if (identical(engine, "cox")) {
    time <- .resolve_single_role(spec, "time")
    event <- .resolve_single_role(spec, "event")
    return(identical(measure, "HR") &&
      !is.null(time) && !is.null(event) && time %in% names(data) && event %in% names(data))
  }

  if (identical(engine, "glm")) {
    cfg <- spec$engine_opts$glm
    offset <- .resolve_single_role(spec, "offset")
    rate_ratio <- identical(measure, "IRR") &&
      !is.null(cfg) && length(cfg$outcomes) >= 1 &&
      identical(cfg$family$family, "poisson") && identical(cfg$family$link, "log") &&
      !is.null(offset) && offset %in% names(data) && is.numeric(data[[offset]])
    return(rate_ratio)
  }

  FALSE
}

#' Resolve default effect measure from study design onto analysis specification
#' @keywords internal
#' @noRd
.resolve_design <- function(spec, data) {
  spec <- validate_simtab_spec(spec)
  design <- .normalise_design(spec$design, allow_null = TRUE)
  spec["design"] <- list(design)

  if (is.null(design)) {
    return(spec)
  }
  if (!is.null(spec$effect$measure)) {
    if (is.null(spec$effect$resolved_from)) {
      spec$effect$resolved_from <- "user"
    }
    return(spec)
  }

  measure <- .design_measure_decision(design)
  if (identical(design, "cohort") &&
      !is.null(.resolve_single_role(spec, "time")) &&
      !is.null(.resolve_single_role(spec, "event"))) {
    measure <- "HR"
  }
  if (is.null(measure) || !.design_effect_supported(spec, data, measure)) {
    return(spec)
  }

  spec$effect$measure <- measure
  spec$effect$resolved_from <- "design"
  spec
}
