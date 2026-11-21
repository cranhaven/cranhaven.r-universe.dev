# SPECIFICATION EVALUATION ENGINE
# Evaluates inert analysis plans against registered engines and constructs evidence containers.

#########
# ENGINE DISPATCH AND VALIDATION
# Resolves execution engines, executes pre-flight checks, and attaches educator advice.

#' Evaluate a SimtablR analysis specification
#'
#' Turns an inert `simtab_spec` into a `simtab_result` by running the shared,
#' engine-agnostic validator, resolving one registered engine, running that
#' engine's own hard-error validator (if any), and wrapping the engine's raw
#' numeric output in the namespaced result subclass the engine registered.
#' Passing an existing `simtab_result` recomputes from its stored specification.
#'
#' @param spec A `simtab_spec`, or a `simtab_result` to recompute.
#' @return A `simtab_result`.
#' @examples
#' sp <- simtab(epitabl) |> describe(hypertension) |> stratify(adjudicated_acs)
#' res <- evaluate(sp)
#' res
#' @export
evaluate <- function(spec) {
  if (inherits(spec, "simtab_result")) {
    spec <- validate_simtab_result(spec)$spec
  } else {
    spec <- validate_simtab_spec(spec)
  }

  # Extract underlying data reference and resolve study design
  data <- spec$data_src$ref$data
  spec <- .resolve_design(spec, data)
  validate(spec)

  # Look up engine and execute validation and options checks
  engine_name <- .resolve_engine(spec)
  eng <- .get_engine(engine_name)
  if (!is.null(eng$validate)) {
    eng$validate(spec)
  }
  if (!is.null(eng$engine_opts)) {
    eng$engine_opts(spec$engine_opts[[engine_name]])
  }

  # Execute engine computation
  out <- eng$compute(spec, data)

  if (!is.list(out) || !all(c("data", "meta") %in% names(out))) {
    simtab_abort_engine(c(
      "Engine output must be a list with {.val data} and {.val meta}.",
      "i" = sprintf("Engine {.val %s} returned an object with names: %s.", engine_name, paste(names(out), collapse = ", ")),
      "v" = "Return {.code list(data = <raw results>, meta = <metadata>)} from the engine compute function."
    ))
  }
  if (is.null(out$meta$design)) {
    out$meta$design <- .normalise_design(spec$design, allow_null = TRUE) %||% .data_design(data)
  }
  out$meta$engine <- engine_name

  # Wrap raw numerical evidence into simtab_result and attach advice
  result <- new_simtab_result(
    spec,
    data = out$data,
    meta = out$meta,
    subclass = c(eng$subclass, eng$legacy_tag),
    call = spec$call
  )
  .attach_advice(result)
}

#' Validate a SimtablR specification before compute
#'
#' Keeps only engine-agnostic checks: spec shape and effect-measure existence.
#' Per-engine hard errors live in that engine's own `validate` hook, registered
#' via [register_engine()], and run by [evaluate()] after this shared check.
#'
#' @param spec A `simtab_spec`.
#' @return Invisibly, `spec`.
#' @examples
#' sp <- simtab(epitabl) |> describe(hypertension)
#' validate(sp)
#' @export
validate <- function(spec) {
  spec <- validate_simtab_spec(spec)
  if (!is.null(spec$effect$measure)) {
    .get_measure(spec$effect$measure)
  }
  invisible(spec)
}

#' Resolves computation engine from explicit specification or role heuristics
#' @keywords internal
#' @noRd
.resolve_engine <- function(spec) {
  if (!is.null(spec$engine)) {
    return(.normalise_registry_name(spec$engine, "engine"))
  }
  if (length(.resolve_tidyselect_role(spec, "describe")) > 0) {
    return("descriptive")
  }
  simtab_abort_engine(c(
    "No engine could be resolved for this spec.",
    "i" = "The spec has no explicit engine and no described variables.",
    "v" = "Add a role, e.g. {.code simtab(data) |> describe(age) |> evaluate()}."
  ))
}
