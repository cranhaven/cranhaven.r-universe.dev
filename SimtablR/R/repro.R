# COMPUTATIONAL REPRODUCIBILITY AND MANIFEST GENERATION
# Records runtime environments, session dependencies, data hash fingerprints,
# methodological choices, and design resolutions for transparent scientific auditing.

#########
# REPRODUCIBILITY MANIFEST GENERATOR
# S3 method dispatch and manifest compilation across specifications, results, and reports.

#' Build a reproducibility manifest
#'
#' Captures the runtime and recorded analysis decisions needed to reproduce a
#' SimtablR result. The data hash and ruleset version are read from the result;
#' the data are not re-hashed.
#'
#' @param result A `simtab_result` or `simtab_spec`.
#' @return A `simtab_manifest` list. `design` and `design_source` describe
#'   any study design recorded on the specification or the source data frame,
#'   whether or not it took part in resolving `measure`; `design_used`
#'   distinguishes the two, and is `TRUE` only when `resolved_from` is
#'   `"design"`. A design recorded only on the data frame (`set_design()` on
#'   a `data.frame`) is advisory and never sets `design_used`; see
#'   [set_design()].
#' @examples
#' res <- tb(epitabl, sex, diabetes)
#' repro_manifest(res)
#' @export
repro_manifest <- function(result) {
  UseMethod("repro_manifest")
}

#' @export
repro_manifest.simtab_spec <- function(result) {
  repro_manifest(evaluate(result))
}

#' @export
repro_manifest.simtab_result <- function(result) {
  result <- .compute_if_spec(result)
  result <- validate_simtab_result(result)
  structure(
    c(
      .manifest_header(result$used),
      list(
        engine = result$meta$engine %||% result$spec$engine,
        estimator = result$meta$estimator %||% result$meta$method %||% NULL,
        interval_method = result$meta$interval_method %||% NULL,
        engine_decisions = .manifest_engine_decisions(result),
        design = .result_design(result),
        design_source = .result_design_source(result),
        design_used = identical(result$spec$effect$resolved_from, "design"),
        measure = result$spec$effect$measure %||% result$meta$effect %||% NULL,
        resolved_from = result$spec$effect$resolved_from %||% NULL,
        measure_computation = result$meta$effect_measure_meta %||% NULL,
        tests = .manifest_tests(result),
        seed = .manifest_seed(),
        ruleset_version = result$meta$ruleset_version %||% .simtab_ruleset_version()
      )
    ),
    class = "simtab_manifest"
  )
}

#' @export
repro_manifest.simtab_report <- function(result) {
  result <- validate_simtab_report(result)
  item_manifests <- lapply(result$items, .manifest_report_item)
  structure(
    c(
      .manifest_header(result$used),
      list(
        items = item_manifests,
        methods = as_methods(result),
        advice_ids = vapply(result$advice, `[[`, character(1), "id"),
        seed = .manifest_seed(),
        ruleset_version = .simtab_ruleset_version()
      )
    ),
    class = "simtab_manifest"
  )
}

#########
# MANIFEST METADATA AND PROVENANCE EXTRACTORS
# Compiles session environments, data hashes, engine choices, and hypothesis tests.

#' Compile runtime environment and captured data hash header
#' @keywords internal
#' @noRd
.manifest_header <- function(used) {
  list(
    r = list(
      version.string = R.version.string,
      platform = R.version$platform
    ),
    packages = .manifest_packages(),
    data_hash = used$hash,
    data_hash_algorithm = "xxhash64",
    data = list(
      nrow = used$nrow,
      ncol = used$ncol,
      names = used$names
    )
  )
}

#' Extract reproducibility manifest sub-record for an individual report item
#' @keywords internal
#' @noRd
.manifest_report_item <- function(result) {
  manifest <- repro_manifest(result)
  list(
    class = class(result),
    engine = manifest$engine,
    estimator = manifest$estimator,
    interval_method = manifest$interval_method,
    engine_decisions = manifest$engine_decisions,
    design = manifest$design,
    design_source = manifest$design_source,
    design_used = manifest$design_used,
    measure = manifest$measure,
    resolved_from = manifest$resolved_from,
    measure_computation = manifest$measure_computation,
    tests = manifest$tests,
    ruleset_version = manifest$ruleset_version
  )
}

#' Extract recorded analytical choices and model metadata from result
#' @keywords internal
#' @noRd
.manifest_engine_decisions <- function(result) {
  fields <- c(
    "measures", "outcome", "method", "vcov", "exposure", "exposure_levels",
    "family", "link", "model_n"
  )
  decisions <- result$meta[intersect(fields, names(result$meta))]
  decisions[!vapply(decisions, is.null, logical(1))]
}

#' Enumerate loaded package versions in active session
#' @keywords internal
#' @noRd
.manifest_packages <- function() {
  info <- utils::sessionInfo()
  pkgs <- c(info$otherPkgs, info$loadedOnly)
  versions <- vapply(pkgs, function(pkg) as.character(pkg$Version), character(1))
  simtab_ver <- tryCatch(as.character(utils::packageVersion("SimtablR")), error = function(e) NA_character_)
  versions["SimtablR"] <- simtab_ver
  versions[order(names(versions))]
}

#' Enumerate statistical hypothesis testing methods used across variables
#' @keywords internal
#' @noRd
.manifest_tests <- function(result) {
  if (inherits(result, "simtab_table1")) {
    methods <- vapply(result$data, function(rec) {
      if (is.null(rec$test)) NA_character_ else rec$test$method %||% NA_character_
    }, character(1))
    return(unique(methods[!is.na(methods)]))
  }
  if (inherits(result, "simtab_tb")) {
    if (is.null(result$meta$stats)) {
      return(character())
    }
    return(result$meta$stats$method %||% character())
  }
  if (inherits(result, "simtab_diag")) {
    return(paste0(result$meta$ci, "_ci"))
  }
  if (inherits(result, "simtab_regtab")) {
    return(paste0(result$meta$family, "(", result$meta$link, ")"))
  }
  character()
}

#' Capture active random number generator seed if initialized
#' @keywords internal
#' @noRd
.manifest_seed <- function() {
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    return(get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)[1])
  }
  NULL
}

#########
# S3 PRESENTATION METHODS
# Formats reproducibility manifest for human-readable console inspection.

#' @export
print.simtab_manifest <- function(x, ...) {
  cat("SimtablR reproducibility manifest\n")
  cat("  R: ", x$r$version.string, "\n", sep = "")
  cat("  data hash (", x$data_hash_algorithm, "): ", x$data_hash, "\n", sep = "")
  if (!is.null(x$items)) {
    cat("  items: ", paste(names(x$items), collapse = ", "), "\n", sep = "")
  } else {
    cat("  design: ", x$design %||% "<unset>", "\n", sep = "")
    if (!is.null(x$design) && !isTRUE(x$design_used)) {
      cat("  design used: FALSE (advisory only; not part of measure resolution)\n")
    }
    cat("  measure: ", x$measure %||% "<unset>", "\n", sep = "")
  }
  cat("  ruleset: ", x$ruleset_version %||% "<unset>", "\n", sep = "")
  invisible(x)
}
