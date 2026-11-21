# SENSITIVITY ANALYSIS AND SPECIFICATION PERTURBATION
# Recomputes alternative analytical specifications (covariate sets, missingness policies,
# effect measures) and evaluates estimand stability for epidemiological peer review.

#########
# SENSITIVITY REPORT GENERATOR
# Evaluates named specification perturbations and compiles comparison metrics.

#' Recompute reviewer-loop sensitivity variations
#'
#' `sensitivity()` takes a computed result and recomputes named variations of
#' the recorded specification. Variations are either functions that take a
#' `simtab_spec` and return a modified `simtab_spec`, or supported shorthand
#' values such as `denominator = "complete"` and `measure = "OR"`.
#'
#' @param result A computed `simtab_result`.
#' @param ... Named sensitivity variations. Unnamed variations are rejected.
#' @return A `simtab_sensitivity` report.
#' @examples
#' res <- tb(epitabl, renal_impairment, adjudicated_acs, measure = "rr", ref = "No")
#' sensitivity(res, measure = "or")
#' @export
sensitivity <- function(result, ...) {
  result <- validate_simtab_result(result)
  dots <- list(...)
  if (length(dots) == 0) {
    simtab_abort_input(c(
      "{.fn sensitivity} requires at least one named variation.",
      "i" = "Each variation is named for a supported shorthand ({.val denominator}, {.val measure}, {.val adjust}) or is a function editing the specification.",
      "v" = "Call {.code sensitivity(result, denominator = \"complete\")}, or pass {.code function(spec)} for anything else."
    ))
  }
  dot_names <- names(dots)
  if (is.null(dot_names) || any(!nzchar(dot_names))) {
    simtab_abort_input(c(
      "All {.fn sensitivity} variations must be named.",
      "i" = "The names label each variation in the report.",
      "v" = "Give every variation a name, e.g. {.code \"Complete cases\" = ...}."
    ))
  }

  items <- list(primary = result)
  for (nm in dot_names) {
    edit_fn <- .sensitivity_variation_fn(nm, dots[[nm]])
    items[[nm]] <- .reapply_to_result(
      result,
      edit_fn,
      call = match.call()
    )
  }

  .new_sensitivity_report(items)
}

#' Resolve variation shorthand or custom modifier function
#' @keywords internal
#' @noRd
.sensitivity_variation_fn <- function(name, value) {
  if (is.function(value)) {
    return(value)
  }

  switch(
    name,
    denominator = {
      force(value)
      function(spec) missingness(spec, denominator = value)
    },
    measure = {
      force(value)
      function(spec) measure(spec, value, ref = spec$effect$ref)
    },
    adjust = {
      function(spec) {
        if (!is.null(value)) {
          simtab_abort_input(c(
            "The {.code adjust} shorthand currently supports only {.code NULL}.",
            "i" = "It exists to drop adjustment, not to change the covariate set.",
            "v" = "Use {.code adjust = NULL} to remove adjustment, or pass a function
                   that edits the specification."
          ))
        }
        spec <- validate_simtab_spec(spec)
        if (is.null(spec$roles$adjust)) {
          # Without this the variation silently equals the primary analysis
          # and the methods prose reports the result as "materially unchanged".
          warning(
            paste0(
              "Sensitivity variation `adjust = NULL` left the analysis unchanged: ",
              "the result records no adjustment covariates to drop. ",
              "Model-formula predictors (e.g. in regtab()) are not removed by this shorthand; ",
              "pass a function that edits the specification instead."
            ),
            call. = FALSE
          )
        }
        spec$roles$adjust <- NULL
        spec
      }
    },
    simtab_abort_input(c(
      "Variation {.val {name}} must be a function or a supported shorthand.",
      "i" = "Shorthands: {.val denominator}, {.val measure}, {.val adjust}.",
      "v" = "Pass {.code function(spec)} returning the edited specification."
    ))
  )
}

#' Construct a validated simtab_sensitivity report object
#' @keywords internal
#' @noRd
.new_sensitivity_report <- function(items) {
  rows <- .sensitivity_rows(items)
  advice <- .dedupe_advice(c(.report_advice_from_items(items), .sensitivity_estimand_advice(rows)))
  methods <- .sensitivity_methods(rows)
  report <- structure(
    list(
      items = items,
      methods = methods,
      used = items[[1]]$used,
      advice = advice
    ),
    class = c("simtab_sensitivity", "simtab_report", "simtab")
  )
  attr(report, "sensitivity_rows") <- rows
  attr(report, "notes") <- rows$note[nzchar(rows$note)]
  validate_simtab_report(report)
}

#########
# ESTIMAND COMPARISON AND METHOD SUMMARY
# Extracts primary effect metrics, calculates percentage deltas, and synthesizes methods.

#' Build comparative effect estimate data frame across variations
#' @keywords internal
#' @noRd
.sensitivity_rows <- function(items) {
  primary <- .sensitivity_headline(items[[1]])
  rows <- lapply(names(items), function(nm) {
    headline <- .sensitivity_headline(items[[nm]])
    same <- identical(headline$measure, primary$measure)
    delta <- if (same && !is.na(primary$estimate) && primary$estimate != 0) {
      (headline$estimate - primary$estimate) / primary$estimate * 100
    } else {
      NA_real_
    }
    note <- if (same) "" else sprintf("Estimand changed from %s to %s; delta not computed.", primary$measure, headline$measure)
    data.frame(
      variation = nm,
      measure = headline$measure,
      estimate = headline$estimate,
      conf.low = headline$conf.low,
      conf.high = headline$conf.high,
      delta_pct = delta,
      note = note
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Extract headline effect estimate and confidence interval from result
#' @keywords internal
#' @noRd
.sensitivity_headline <- function(result) {
  frame <- .effect_forest_frame(result)
  mh <- result$data$mh
  if (inherits(result, "simtab_tb") && is.data.frame(mh)) {
    # A stratified table's headline is the pooled estimate, not the first stratum.
    # The forest frame keeps the mh rows with an estimate, in order.
    row_type <- mh$row_type[!is.na(mh$estimate)]
    if (length(row_type) == nrow(frame)) {
      frame <- frame[row_type == "pooled", , drop = FALSE]
    }
  }
  # `is_reference` is a per-row logical column; use a vectorised test.
  # `isTRUE()` would collapse the whole column to a single FALSE, leaving the
  # reference row (estimate 1, NA CIs) in the frame and hijacking the headline.
  frame <- frame[!is.na(frame$estimate) & !(frame$is_reference %in% TRUE), , drop = FALSE]
  measure <- toupper(result$spec$effect$measure %||% result$meta$effect %||% "Estimate")
  if (nrow(frame) == 0) {
    return(list(measure = measure, estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_))
  }
  list(
    measure = measure,
    estimate = frame$estimate[[1]],
    conf.low = frame$conf.low[[1]],
    conf.high = frame$conf.high[[1]]
  )
}

#' Synthesize methodological summary text for sensitivity analyses
#' @keywords internal
#' @noRd
.sensitivity_methods <- function(rows) {
  if (nrow(rows) <= 1L) {
    return("Sensitivity analyses were recomputed from the recorded SimtablR specification.")
  }
  primary <- rows[rows$variation == "primary", , drop = FALSE]
  comparable <- rows[rows$variation != "primary" & !is.na(rows$delta_pct), , drop = FALSE]
  if (nrow(primary) == 1L && nrow(comparable) > 0) {
    max_abs_delta <- max(abs(comparable$delta_pct), na.rm = TRUE)
    material <- if (!is.na(max_abs_delta) && max_abs_delta <= 10) "materially unchanged" else "changed"
    return(sprintf(
      "Sensitivity analyses were recomputed from the recorded SimtablR specification; results were %s under same-estimand variations (%s %.3g in the primary analysis and %.3g to %.3g across variations).",
      material,
      primary$measure[[1]],
      primary$estimate[[1]],
      min(comparable$estimate, na.rm = TRUE),
      max(comparable$estimate, na.rm = TRUE)
    ))
  }
  "Sensitivity analyses were recomputed from the recorded SimtablR specification; estimand-changing variations were footnoted rather than deltaed."
}

#########
# S3 COERCION AND PRESENTATION METHODS
# Formats tabular sensitivity outputs for console print and flextable rendering.

#' @export
as.data.frame.simtab_sensitivity <- function(x, row.names = NULL, optional = FALSE, ...) {
  x <- validate_simtab_report(x)
  rows <- attr(x, "sensitivity_rows", exact = TRUE)
  data.frame(
    Variation = rows$variation,
    Measure = rows$measure,
    Estimate = rows$estimate,
    `95% CI low` = rows$conf.low,
    `95% CI high` = rows$conf.high,
    `Delta %` = rows$delta_pct,
    Note = rows$note,
    check.names = FALSE
  )
}

#' @export
print.simtab_sensitivity <- function(x, ...) {
  x <- validate_simtab_report(x)
  cat("<simtab_sensitivity>\n")
  print(as.data.frame(x), row.names = FALSE)
  notes <- attr(x, "notes", exact = TRUE) %||% character()
  if (length(notes) > 0) {
    cat("\nNotes:\n")
    for (note in unique(notes)) {
      cat("  - ", note, "\n", sep = "")
    }
  }
  .print_advice(x)
  invisible(x)
}

#' @rdname sensitivity
#' @param x A `simtab_sensitivity` report.
#' @exportS3Method flextable::as_flextable
as_flextable.simtab_sensitivity <- function(x, ...) {
  .require_pkg("flextable")
  ft <- flextable::flextable(as.data.frame(x), ...)
  notes <- attr(x, "notes", exact = TRUE) %||% character()
  if (length(notes) > 0) {
    ft <- flextable::add_footer_lines(ft, values = unique(notes))
    ft <- flextable::align(ft, part = "footer", align = "left")
  }
  simtab_theme(ft)
}

#########
# ESTIMAND INTEGRITY AND ADVISORY CHECKS
# Detects estimand transitions across variations and emits methodological guidance.

#' Detect whether any sensitivity variation altered the underlying effect estimand
#' @keywords internal
#' @noRd
.sensitivity_estimand_changed <- function(result) {
  rows <- attr(result, "sensitivity_rows", exact = TRUE)
  if (!is.data.frame(rows) || nrow(rows) == 0 || !"note" %in% names(rows)) {
    return(FALSE)
  }
  any(nzchar(rows$note %||% character()), na.rm = TRUE)
}

#' Generate advisory guidance when sensitivity variations alter the estimand
#' @keywords internal
#' @noRd
.sensitivity_estimand_advice <- function(result) {
  rows <- if (inherits(result, "simtab_sensitivity")) {
    attr(result, "sensitivity_rows", exact = TRUE)
  } else {
    result
  }
  if (!is.data.frame(rows) || nrow(rows) == 0) {
    return(list())
  }
  changed <- rows[nzchar(rows$note %||% character()), , drop = FALSE]
  if (nrow(changed) == 0) {
    return(list())
  }
  list(list(
    id = "sensitivity_estimand_change",
    severity = 1L,
    message = sprintf(
      "Sensitivity variation(s) %s changed the estimand; percent deltas are not comparable.",
      paste(sprintf("'%s'", changed$variation), collapse = ", ")
    ),
    citation = "STROBE item 17",
    fix = "Interpret estimand-changing sensitivity analyses qualitatively; do not report a percent delta across different estimands.",
    version = .simtab_ruleset_version(),
    why = "Odds ratios, prevalence ratios, and risk ratios answer different questions, so a numeric percent change between them is not a stability check."
  ))
}
