# COMPUTATION ENGINE REGISTRATION CONTRACT AND DISPATCH
# Validates engine definitions, resolves renderer verbs, and inspects
# S3 result classes across the analysis ecosystem.

#########
# ENGINE CONTRACT AND RENDERER DISPATCH
# Validates engine registration components and resolves registered renderer verbs.

#' Enumerate supported renderer verbs in the engine contract
#' @keywords internal
#' @noRd
.renderer_verbs <- function() {
  c("print", "as_data_frame", "as_flextable", "as_gt", "tidy", "glance", "autoplot", "as_methods")
}

#' Validate registration structure and renderer conformance of an engine
#' @keywords internal
#' @noRd
.validate_simtab_engine <- function(x) {
  if (!is.function(x$compute)) {
    simtab_abort_input(c(
      "{.arg compute} must be a function.",
      "i" = "Received an object of class {.cls {class(x$compute)[[1]]}}.",
      "v" = "Pass {.code function(spec, data) list(data = ..., meta = ...)}."
    ))
  }
  if (!is.character(x$subclass) || length(x$subclass) != 1 || !nzchar(x$subclass)) {
    simtab_abort_input(c(
      "{.arg subclass} must be a non-empty string.",
      "i" = "The subclass is the S3 class the computed result carries.",
      "v" = "Use a namespaced name, e.g. {.val simtab_myengine}."
    ))
  }
  if (!is.null(x$validate) && !is.function(x$validate)) {
    simtab_abort_input(c(
      "{.arg validate} must be a function or NULL.",
      "i" = "Received an object of class {.cls {class(x$validate)[[1]]}}.",
      "v" = "Pass {.code function(spec)}, or omit it."
    ))
  }
  if (!is.null(x$engine_opts) && !is.function(x$engine_opts)) {
    simtab_abort_input(c(
      "{.arg engine_opts} must be a function or NULL.",
      "i" = "Received an object of class {.cls {class(x$engine_opts)[[1]]}}.",
      "v" = "Build one with {.fn .engine_opts_validator}, or omit it."
    ))
  }
  if (!is.list(x$renderers) || (length(x$renderers) > 0 && is.null(names(x$renderers)))) {
    simtab_abort_input(c(
      "{.arg renderers} must be a named list of functions.",
      "i" = "Received an object of class {.cls {class(x$renderers)[[1]]}}.",
      "v" = "Use {.code renderers = list(print = function(x, ...) ...)}."
    ))
  }
  if (!is.null(x$verbs) &&
      (!is.character(x$verbs) || anyNA(x$verbs) || !all(x$verbs %in% .evidence_verbs()))) {
    allowed <- .evidence_verbs()
    simtab_abort_input(c(
      "{.arg verbs} must be NULL or a character vector of evidence verbs.",
      "i" = "Allowed: {.val {allowed}}.",
      "v" = "Use e.g. {.code verbs = c(\"stratify\", \"adjust\")}."
    ))
  }
  not_functions <- names(x$renderers)[!vapply(x$renderers, is.function, logical(1))]
  if (length(not_functions) > 0) {
    simtab_abort_input(c(
      "Every renderer must be a function.",
      "x" = "Not a function: {.val {not_functions}}.",
      "v" = "Pass {.code function(x, ...)} for each renderer verb."
    ))
  }
  if (length(x$renderers) > 0) {
    # Bound to a local first: cli reads a `{}` expression starting with a dot as
    # a style name, so `{.renderer_verbs()}` would not survive interpolation.
    known <- .renderer_verbs()
    unknown <- setdiff(names(x$renderers), known)
    if (length(unknown) > 0) {
      simtab_abort_input(c(
        "Unknown renderer verb(s): {.val {unknown}}.",
        "i" = "Known verbs: {.val {known}}.",
        "v" = "Rename the renderer, or drop it from the list."
      ))
    }
  }
  invisible(x)
}

#' Retrieve the registered renderer function for an engine and verb
#' @keywords internal
#' @noRd
.engine_renderer <- function(x, verb) {
  engine <- x$meta$engine %||% x$spec$engine
  if (is.null(engine)) {
    return(NULL)
  }
  entry <- tryCatch(.get_engine(engine), error = function(e) NULL)
  if (is.null(entry)) {
    return(NULL)
  }
  entry$renderers[[verb]]
}

#########
# RESULT CLASS IDENTIFICATION
# Tests whether an object inherits the simtab result class or specific presets.

#' Test whether an object is a SimtablR result, optionally of a given preset
#'
#' Namespaced subclasses (`simtab_tb`, `simtab_table1`, `simtab_regtab`,
#' `simtab_diag`, `simtab_roc`, ...) are the supported way to test result
#' identity; bare legacy tags (kept in the class vector for external,
#' backward-compatible `inherits()` callers) should not be tested directly in
#' new package code. Use `is_simtab(x, preset)` instead.
#'
#' @param x Any object.
#' @param preset Optional character engine/preset name (e.g. `"tb"`, `"roc"`).
#' @return `TRUE`/`FALSE`.
#' @examples
#' res <- tb(epitabl, sex, diabetes)
#' is_simtab(res)
#' is_simtab(res, preset = "tb")
#' is_simtab(iris)
#' @export
is_simtab <- function(x, preset = NULL) {
  if (!inherits(x, "simtab")) {
    return(FALSE)
  }
  if (is.null(preset)) {
    return(TRUE)
  }
  inherits(x, paste0("simtab_", preset))
}
