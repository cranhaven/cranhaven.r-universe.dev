# R/methods-print.R
#
# Print and summary S7 methods for survey design objects.
#
# All methods use S7::method() registration — NOT S3 syntax.
# S3 method naming (print.survey_taylor <- ...) is silently ignored
# for S7 objects and must never be used here.
#
# Classes defined in R/core-classes.R:
#   survey_taylor   — Taylor series linearization design
#   survey_replicate — Replicate weights design
#   survey_twophase — Two-phase sampling design
#
# See plans/surveycore-phase0-formal-specification.md Section VIII for
# the full output format specification.

# ── Internal helpers ────────────────────────────────────────────────────────

# Return the subset of @data to display, converted to tibble for printing.
# Respects @variables$visible_vars when set by surveytidy's select().
# NULL (the surveycore default) → all columns shown.
#' @noRd
.data_for_print <- function(x) {
  visible <- x@variables[["visible_vars"]]
  data <- if (!is.null(visible)) {
    x@data[, visible, drop = FALSE] # nocov — only set by surveytidy
  } else {
    x@data
  }
  tibble::as_tibble(data)
}

# Print a note about design variables that are not in data_shown.
# Only fires when surveytidy's select() has hidden design variables
# (i.e., visible_vars is set). Never fires in standalone surveycore use.
#' @noRd
.print_hidden_note <- function(design_vars, data_shown) {
  hidden <- setdiff(design_vars, names(data_shown))
  if (length(hidden) > 0L) {
    # nocov start — only set by surveytidy
    cli::cli_text("")
    cli::cli_bullets(c(
      "i" = "Design variables preserved but hidden: {.field {hidden}}.",
      "i" = "Use {.code print(x, full = TRUE)} to show all variables."
    ))
  } # nocov end
  invisible(NULL)
}

# Format weight distribution as bulleted list lines.
#' @noRd
.print_weight_distribution <- function(wts) {
  wt_min <- round(min(wts, na.rm = TRUE), 2L)
  wt_max <- round(max(wts, na.rm = TRUE), 2L)
  wt_mean <- round(mean(wts, na.rm = TRUE), 2L)
  wt_cv <- if (length(wts[!is.na(wts)]) > 1L) {
    round(stats::sd(wts, na.rm = TRUE) / mean(wts, na.rm = TRUE), 3L)
  } else {
    NA_real_
  }
  cli::cli_bullets(c(
    "*" = "Range: {.val {wt_min}} \u2013 {.val {wt_max}}",
    "*" = "Mean: {.val {wt_mean}}",
    "*" = "CV: {.val {wt_cv}}"
  ))
}

# Print a domain membership line when SURVEYCORE_DOMAIN_COL is present in @data.
# For survey_twophase, uses Phase 2 row counts to match what analysis computes.
# Does nothing when the column is absent (no filter applied via surveytidy).
# @param x Any survey_base subclass.
# @return invisible(NULL)
# @noRd
.print_domain_info <- function(x) {
  if (!SURVEYCORE_DOMAIN_COL %in% names(x@data)) {
    return(invisible(NULL))
  }

  if (S7::S7_inherits(x, survey_twophase)) {
    ph2_mask <- x@data[[x@variables$subset]]
    n_domain <- sum(x@data[[SURVEYCORE_DOMAIN_COL]][ph2_mask], na.rm = TRUE)
    n_total <- sum(ph2_mask, na.rm = TRUE)
    cli::cli_text("Domain: {.val {n_domain}} of {.val {n_total}} Phase 2 rows")
  } else {
    n_domain <- sum(x@data[[SURVEYCORE_DOMAIN_COL]], na.rm = TRUE)
    n_total <- nrow(x@data)
    cli::cli_text("Domain: {.val {n_domain}} of {.val {n_total}} row{?s}")
  }
  invisible(NULL)
}


# ── print.survey_taylor ────────────────────────────────────────────────────

# Print a Taylor Series Survey Design
#
# @param x A survey_taylor object.
# @param n Maximum number of data rows to print. Default 10L.
# @param design_info Show design specification section?
# @param weights_info Show weight distribution statistics?
# @param strata_info Currently bundled into design_info.
# @param cluster_info Currently bundled into design_info.
# @param metadata_info Show metadata summary?
# @param full If TRUE, show all sections.
# @param ... Passed to tibble print.
# @return x, invisibly.
# Class defined in R/core-classes.R
S7::method(print, survey_taylor) <- function(
  x,
  n = 10L,
  design_info = FALSE,
  weights_info = FALSE,
  strata_info = FALSE,
  cluster_info = FALSE,
  metadata_info = FALSE,
  full = FALSE,
  ...
) {
  if (full) {
    design_info <- weights_info <- strata_info <-
      cluster_info <- metadata_info <- TRUE
  }

  # ── Header ────────────────────────────────────────────────────────────────
  cli::cli_h1("Survey Design")
  cli::cli_text("{.cls survey_taylor} (Taylor series linearization)")
  cli::cli_text("Sample size: {.val {nrow(x@data)}}")
  .print_domain_info(x)

  if (length(x@groups) > 0L) {
    cli::cli_text("Groups: {.field {x@groups}}")
  }

  if (weights_info) {
    wts <- x@data[[x@variables$weights]]
    weighted_n <- round(sum(wts, na.rm = TRUE))
    cli::cli_text("Weighted N: {.val {weighted_n}}")
  }

  # ── Design specification ──────────────────────────────────────────────────
  if (design_info) {
    cli::cli_text("")
    cli::cli_h2("Design specification")

    ids_vars <- x@variables$ids
    if (!is.null(ids_vars) && length(ids_vars) > 0L) {
      n_psus <- length(unique(x@data[[ids_vars[[1L]]]]))
      cli::cli_bullets(c(
        "*" = "IDs: {.field {ids_vars}} ({n_psus} PSUs)"
      ))
    }

    strata_var <- x@variables$strata
    if (!is.null(strata_var)) {
      n_strata <- length(unique(x@data[[strata_var]]))
      cli::cli_bullets(c(
        "*" = "Strata: {.field {strata_var}} ({n_strata} strata)"
      ))
    }

    wts_var <- x@variables$weights
    cli::cli_bullets(c("*" = "Weights: {.field {wts_var}}"))

    # Show "Weights provided as:" unless this is a pure SRS auto-weight design
    # (..surveycore_wt.. AND probs_provided = FALSE).
    # When probs are provided and converted to ..surveycore_wt.., still show
    # the "sampling probabilities (converted)" indicator.
    is_srs_auto <- identical(wts_var, "..surveycore_wt..") &&
      !isTRUE(x@variables$probs_provided)
    if (!is_srs_auto) {
      wt_source <- if (isTRUE(x@variables$probs_provided)) {
        "sampling probabilities (converted)"
      } else {
        "sampling weights"
      }
      cli::cli_bullets(c("*" = "Weights provided as: {wt_source}"))
    }

    fpc_var <- x@variables$fpc
    if (!is.null(fpc_var)) {
      if (length(fpc_var) == 1L) {
        cli::cli_bullets(c("*" = "FPC: {.field {fpc_var}}"))
      } else {
        for (j in seq_along(fpc_var)) {
          cli::cli_bullets(c(
            "*" = "FPC (stage {j}): {.field {fpc_var[[j]]}}"
          ))
        }
      }
    } else {
      cli::cli_bullets(c("*" = "FPC: not specified"))
    }

    cli::cli_bullets(c("*" = "Nesting: {.val {isTRUE(x@variables$nest)}}"))

    design_vars_spec <- unique(c(ids_vars, wts_var, strata_var, fpc_var))
    cli::cli_text("")
    cli::cli_text("Design variables: {.field {design_vars_spec}}")
  }

  # ── Weight distribution ───────────────────────────────────────────────────
  if (weights_info) {
    wts <- x@data[[x@variables$weights]]
    cli::cli_text("")
    cli::cli_h2("Weight distribution")
    .print_weight_distribution(wts)
  }

  # ── Metadata ──────────────────────────────────────────────────────────────
  if (metadata_info) {
    n_labeled <- length(x@metadata@variable_labels)
    cli::cli_text("")
    cli::cli_h2("Metadata")
    cli::cli_text("{n_labeled} variable(s) labeled")
  }

  # ── Data ──────────────────────────────────────────────────────────────────
  cli::cli_text("")
  data_shown <- .data_for_print(x)
  print(data_shown, n = n, ...)

  design_vars_all <- unique(c(
    x@variables$ids,
    x@variables$weights,
    x@variables$strata,
    x@variables$fpc
  ))
  .print_hidden_note(design_vars_all, data_shown)

  invisible(x)
}


# ── print.survey_replicate ─────────────────────────────────────────────────

# Print a Replicate Weights Survey Design
#
# @param x A survey_replicate object.
# @param n Maximum number of data rows to print. Default 10L.
# @param design_info Show design specification section?
# @param weights_info Show weight distribution statistics?
# @param strata_info Reserved.
# @param cluster_info Reserved.
# @param metadata_info Show metadata summary?
# @param full If TRUE, show all sections.
# @param ... Passed to tibble print.
# @return x, invisibly.
# Class defined in R/core-classes.R
S7::method(print, survey_replicate) <- function(
  x,
  n = 10L,
  design_info = FALSE,
  weights_info = FALSE,
  strata_info = FALSE,
  cluster_info = FALSE,
  metadata_info = FALSE,
  full = FALSE,
  ...
) {
  if (full) {
    design_info <- weights_info <- strata_info <-
      cluster_info <- metadata_info <- TRUE
  }

  n_reps <- length(x@variables$repweights)

  # ── Header ────────────────────────────────────────────────────────────────
  cli::cli_h1("Survey Design")
  cli::cli_text(
    "{.cls survey_replicate} ({toupper(x@variables$type)}, {n_reps} replicates)"
  )
  cli::cli_text("Sample size: {.val {nrow(x@data)}}")
  .print_domain_info(x)

  if (length(x@groups) > 0L) {
    cli::cli_text("Groups: {.field {x@groups}}")
  }

  if (weights_info) {
    wts <- x@data[[x@variables$weights]]
    weighted_n <- round(sum(wts, na.rm = TRUE))
    cli::cli_text("Weighted N: {.val {weighted_n}}")
  }

  # ── Design specification ──────────────────────────────────────────────────
  if (design_info) {
    cli::cli_text("")
    cli::cli_h2("Design specification")

    wts_var <- x@variables$weights
    cli::cli_bullets(c("*" = "Weights: {.field {wts_var}}"))
    cli::cli_bullets(c(
      "*" = paste0(
        "Replicates: {n_reps} {toupper(x@variables$type)} ",
        "replicate weight column(s)"
      )
    ))
    cli::cli_bullets(c("*" = "Scale: {.val {x@variables$scale}}"))
    cli::cli_bullets(c("*" = "MSE: {.val {isTRUE(x@variables$mse)}}"))

    fpc_var <- x@variables$fpc
    if (!is.null(fpc_var)) {
      cli::cli_bullets(c(
        "*" = "FPC: {.field {fpc_var}} ({x@variables$fpctype})"
      ))
    } else {
      cli::cli_bullets(c("*" = "FPC: not specified"))
    }
  }

  # ── Weight distribution ───────────────────────────────────────────────────
  if (weights_info) {
    wts <- x@data[[x@variables$weights]]
    cli::cli_text("")
    cli::cli_h2("Weight distribution")
    .print_weight_distribution(wts)
  }

  # ── Metadata ──────────────────────────────────────────────────────────────
  if (metadata_info) {
    n_labeled <- length(x@metadata@variable_labels)
    cli::cli_text("")
    cli::cli_h2("Metadata")
    cli::cli_text("{n_labeled} variable(s) labeled")
  }

  # ── Data ──────────────────────────────────────────────────────────────────
  cli::cli_text("")
  data_shown <- .data_for_print(x)
  print(data_shown, n = n, ...)

  design_vars_all <- unique(c(x@variables$weights, x@variables$repweights))
  .print_hidden_note(design_vars_all, data_shown)

  invisible(x)
}


# ── print.survey_twophase ─────────────────────────────────────────────────

# Print a Two-Phase Survey Design
#
# @param x A survey_twophase object.
# @param n Maximum number of data rows to print. Default 10L.
# @param design_info Show phase 1 and phase 2 specification sections?
# @param weights_info Reserved (phase1 weight shown in design_info).
# @param strata_info Reserved.
# @param cluster_info Reserved.
# @param metadata_info Show metadata summary?
# @param full If TRUE, show all sections.
# @param ... Passed to tibble print.
# @return x, invisibly.
# Class defined in R/core-classes.R
S7::method(print, survey_twophase) <- function(
  x,
  n = 10L,
  design_info = FALSE,
  weights_info = FALSE,
  strata_info = FALSE,
  cluster_info = FALSE,
  metadata_info = FALSE,
  full = FALSE,
  ...
) {
  if (full) {
    design_info <- weights_info <- strata_info <-
      cluster_info <- metadata_info <- TRUE
  }

  subset_var <- x@variables$subset
  n_total <- nrow(x@data)
  n_phase2 <- if (!is.null(subset_var) && subset_var %in% names(x@data)) {
    sum(x@data[[subset_var]], na.rm = TRUE)
  } else {
    NA_integer_
  }

  # ── Header ────────────────────────────────────────────────────────────────
  cli::cli_h1("Survey Design")
  cli::cli_text(
    "{.cls survey_twophase} (method: {x@variables$method})"
  )
  cli::cli_text("Phase 1 sample size: {.val {n_total}}")
  if (!is.na(n_phase2)) {
    cli::cli_text("Phase 2 sample size: {.val {n_phase2}}")
  }
  .print_domain_info(x)

  if (length(x@groups) > 0L) {
    cli::cli_text("Groups: {.field {x@groups}}")
  }

  # ── Design specification ──────────────────────────────────────────────────
  if (design_info) {
    cli::cli_text("")
    cli::cli_h2("Phase 1 design")

    p1 <- x@variables$phase1
    if (!is.null(p1$ids) && length(p1$ids) > 0L) {
      n_psus <- length(unique(x@data[[p1$ids[[1L]]]]))
      cli::cli_bullets(c("*" = "IDs: {.field {p1$ids}} ({n_psus} PSUs)"))
    }
    if (!is.null(p1$strata)) {
      n_strata <- length(unique(x@data[[p1$strata]]))
      cli::cli_bullets(c(
        "*" = "Strata: {.field {p1$strata}} ({n_strata} strata)"
      ))
    }
    if (!is.null(p1$weights)) {
      cli::cli_bullets(c("*" = "Weights: {.field {p1$weights}}"))
    }

    p2 <- x@variables$phase2
    cli::cli_text("")
    cli::cli_h2("Phase 2 design")
    cli::cli_bullets(c("*" = "Subset: {.field {subset_var}}"))
    if (!is.null(p2$ids) && length(p2$ids) > 0L) {
      cli::cli_bullets(c("*" = "IDs: {.field {p2$ids}}"))
    }
    if (!is.null(p2$strata)) {
      cli::cli_bullets(c("*" = "Strata: {.field {p2$strata}}"))
    }
    if (!is.null(p2$probs)) {
      cli::cli_bullets(c("*" = "Probs: {.field {p2$probs}}"))
    }
    if (!is.null(p2$fpc)) {
      cli::cli_bullets(c("*" = "FPC: {.field {p2$fpc}}"))
    }
  }

  # ── Metadata ──────────────────────────────────────────────────────────────
  if (metadata_info) {
    n_labeled <- length(x@metadata@variable_labels)
    cli::cli_text("")
    cli::cli_h2("Metadata")
    cli::cli_text("{n_labeled} variable(s) labeled")
  }

  # ── Data ──────────────────────────────────────────────────────────────────
  cli::cli_text("")
  data_shown <- .data_for_print(x)
  print(data_shown, n = n, ...)

  p1 <- x@variables$phase1
  p2 <- x@variables$phase2
  p2_cols <- if (!is.null(p2)) {
    unlist(p2[!vapply(p2, is.null, logical(1L))], use.names = FALSE)
  } else {
    character(0L)
  }
  design_vars_all <- unique(c(
    p1$ids,
    p1$weights,
    p1$strata,
    p1$fpc,
    p2_cols,
    subset_var
  ))
  .print_hidden_note(design_vars_all, data_shown)

  invisible(x)
}


# ── print.survey_nonprob ───────────────────────────────────────────────

# Print a Non-Probability Survey Design
#
# @param x A survey_nonprob object.
# @param n Maximum number of data rows to print. Default 10L.
# @param design_info Show design specification section?
# @param weights_info Show weight distribution statistics?
# @param metadata_info Show metadata summary?
# @param full If TRUE, show all sections.
# @param ... Passed to tibble print.
# @return x, invisibly.
# Class defined in R/core-classes.R
S7::method(print, survey_nonprob) <- function(
  x,
  n = 10L,
  design_info = FALSE,
  weights_info = FALSE,
  metadata_info = FALSE,
  full = FALSE,
  ...
) {
  if (full) {
    design_info <- weights_info <- metadata_info <- TRUE
  }

  # Hoist weight extraction once; used by header and weight-distribution
  # sections
  wts <- x@data[[x@variables$weights]]
  wts_var <- x@variables$weights

  # ── Header ────────────────────────────────────────────────────────────────
  cli::cli_h1("Survey Design")
  if (!is.null(x@variables$repweights)) {
    R_count <- length(x@variables$repweights)
    type_label <- toupper(x@variables$type)
    cli::cli_text(
      paste0(
        "{.cls survey_nonprob} (non-probability, {type_label}, ",
        "{R_count} replicates) [experimental]"
      )
    )
  } else {
    cli::cli_text(
      "{.cls survey_nonprob} (non-probability) [experimental]"
    )
    cli::cli_bullets(
      c("*" = "Variance: SRS approximation (no bootstrap replicate weights)")
    )
  }
  cli::cli_text("Sample size: {.val {nrow(x@data)}}")
  .print_domain_info(x)

  if (length(x@groups) > 0L) {
    cli::cli_text("Groups: {.field {x@groups}}")
  }

  if (weights_info) {
    weighted_n <- round(sum(wts, na.rm = TRUE))
    cli::cli_text("Weighted N: {.val {weighted_n}}")
  }

  # ── Design specification ──────────────────────────────────────────────────
  if (design_info) {
    cli::cli_text("")
    cli::cli_h2("Design specification")

    cli::cli_bullets(c("*" = "Weights: {.field {wts_var}}"))

    if (!is.null(x@variables$repweights)) {
      R_count <- length(x@variables$repweights)
      rep_cols <- paste(x@variables$repweights, collapse = ", ")
      cli::cli_bullets(c(
        "*" = "Replicate weights: {R_count} columns ({.field {rep_cols}})"
      ))
      cli::cli_bullets(c("*" = "Type: {.val {x@variables$type}}"))
      cli::cli_bullets(c("*" = "Scale: {.val {x@variables$scale}}"))
    }

    cal_label <- if (!is.null(x@calibration)) "stored" else "none stored"
    cli::cli_bullets(c("*" = "Calibration provenance: {cal_label}"))

    cli::cli_text("")
    cli::cli_text("Design variables: {.field {wts_var}}")
  }

  # ── Weight distribution ───────────────────────────────────────────────────
  if (weights_info) {
    cli::cli_text("")
    cli::cli_h2("Weight distribution")
    .print_weight_distribution(wts)
  }

  # ── Metadata ──────────────────────────────────────────────────────────────
  if (metadata_info) {
    n_labeled <- length(x@metadata@variable_labels)
    cli::cli_text("")
    cli::cli_h2("Metadata")
    cli::cli_text("{n_labeled} variable(s) labeled")
  }

  # ── Data ──────────────────────────────────────────────────────────────────
  cli::cli_text("")
  data_shown <- .data_for_print(x)
  print(data_shown, n = n, ...)

  .print_hidden_note(wts_var, data_shown)

  invisible(x)
}


# ── summary.survey_nonprob ─────────────────────────────────────────────

# Summarise a Non-Probability Survey Design
# @param object A survey_nonprob object.
# @return object, invisibly.
# Class defined in R/core-classes.R
S7::method(summary, survey_nonprob) <- function(object, ...) {
  x <- object

  cli::cli_h1("Survey Design Summary")
  if (!is.null(x@variables$repweights)) {
    cli::cli_text(
      "Type: non-probability, {x@variables$type} replicates [experimental]"
    )
  } else {
    cli::cli_text("Type: non-probability [experimental]")
  }
  cli::cli_text("Sample size: {.val {nrow(x@data)}}")

  wts_var <- x@variables$weights
  wts <- x@data[[wts_var]]
  weighted_n <- round(sum(wts, na.rm = TRUE))
  cli::cli_text("Weighted N: {.val {weighted_n}}")

  cli::cli_text("")
  cli::cli_h2("Design")
  cli::cli_text("Weights: {.field {wts_var}}")
  .print_weight_distribution(wts)

  cal_label <- if (!is.null(x@calibration)) "stored" else "none stored"
  cli::cli_text("Calibration provenance: {cal_label}")

  cli::cli_text("")
  n_labeled <- length(x@metadata@variable_labels)
  n_total <- ncol(x@data)
  cli::cli_text("Metadata: {n_labeled} of {n_total} variable(s) labeled")

  invisible(x)
}


# ── summary.survey_taylor ──────────────────────────────────────────────────

# Summarise a Taylor Series Survey Design
# @param object A survey_taylor object.
# @return object, invisibly.
# Class defined in R/core-classes.R
S7::method(summary, survey_taylor) <- function(object, ...) {
  x <- object

  cli::cli_h1("Survey Design Summary")
  cli::cli_text("Type: Taylor series linearization")
  cli::cli_text("Sample size: {.val {nrow(x@data)}}")

  wts_var <- x@variables$weights
  wts <- x@data[[wts_var]]
  weighted_n <- round(sum(wts, na.rm = TRUE))
  cli::cli_text("Weighted N: {.val {weighted_n}}")

  cli::cli_text("")
  cli::cli_h2("Design")

  ids_vars <- x@variables$ids
  if (!is.null(ids_vars) && length(ids_vars) > 0L) {
    n_psus <- length(unique(x@data[[ids_vars[[1L]]]]))
    cli::cli_text("IDs: {.field {ids_vars}} ({n_psus} PSUs)")
  } else {
    cli::cli_text("IDs: none (SRS)")
  }

  strata_var <- x@variables$strata
  if (!is.null(strata_var)) {
    n_strata <- length(unique(x@data[[strata_var]]))
    cli::cli_text("Strata: {.field {strata_var}} ({n_strata} strata)")
  } else {
    cli::cli_text("Strata: none")
  }

  cli::cli_text("Weights: {.field {wts_var}}")
  .print_weight_distribution(wts)

  fpc_var <- x@variables$fpc
  if (!is.null(fpc_var)) {
    cli::cli_text("FPC: {.field {fpc_var}}")
  }

  cli::cli_text("")
  n_labeled <- length(x@metadata@variable_labels)
  n_total <- ncol(x@data)
  cli::cli_text("Metadata: {n_labeled} of {n_total} variable(s) labeled")

  invisible(x)
}


# ── summary.survey_replicate ──────────────────────────────────────────────

# Summarise a Replicate Weights Survey Design
# @param object A survey_replicate object.
# @return object, invisibly.
# Class defined in R/core-classes.R
S7::method(summary, survey_replicate) <- function(object, ...) {
  x <- object

  n_reps <- length(x@variables$repweights)

  cli::cli_h1("Survey Design Summary")
  cli::cli_text(
    "Type: replicate weights ({toupper(x@variables$type)}, {n_reps} replicates)"
  )
  cli::cli_text("Sample size: {.val {nrow(x@data)}}")

  wts_var <- x@variables$weights
  wts <- x@data[[wts_var]]
  weighted_n <- round(sum(wts, na.rm = TRUE))
  cli::cli_text("Weighted N: {.val {weighted_n}}")

  cli::cli_text("")
  cli::cli_h2("Design")
  cli::cli_text("Weights: {.field {wts_var}}")
  .print_weight_distribution(wts)
  cli::cli_text("Scale: {.val {x@variables$scale}}")
  cli::cli_text("MSE: {.val {isTRUE(x@variables$mse)}}")

  fpc_var <- x@variables$fpc
  if (!is.null(fpc_var)) {
    cli::cli_text("FPC: {.field {fpc_var}} ({x@variables$fpctype})")
  }

  cli::cli_text("")
  n_labeled <- length(x@metadata@variable_labels)
  n_total <- ncol(x@data)
  cli::cli_text("Metadata: {n_labeled} of {n_total} variable(s) labeled")

  invisible(x)
}


# ── summary.survey_twophase ───────────────────────────────────────────────

# Summarise a Two-Phase Survey Design
# @param object A survey_twophase object.
# @return object, invisibly.
# Class defined in R/core-classes.R
S7::method(summary, survey_twophase) <- function(object, ...) {
  x <- object

  subset_var <- x@variables$subset
  n_total <- nrow(x@data)
  n_phase2 <- if (!is.null(subset_var) && subset_var %in% names(x@data)) {
    sum(x@data[[subset_var]], na.rm = TRUE)
  } else {
    NA_integer_
  }

  cli::cli_h1("Survey Design Summary")
  cli::cli_text("Type: two-phase sampling (method: {x@variables$method})")
  cli::cli_text("Phase 1 sample size: {.val {n_total}}")
  if (!is.na(n_phase2)) {
    cli::cli_text("Phase 2 sample size: {.val {n_phase2}}")
  }

  cli::cli_text("")
  cli::cli_h2("Phase 1 design")
  p1 <- x@variables$phase1
  if (!is.null(p1$ids) && length(p1$ids) > 0L) {
    n_psus <- length(unique(x@data[[p1$ids[[1L]]]]))
    cli::cli_text("IDs: {.field {p1$ids}} ({n_psus} PSUs)")
  } else {
    cli::cli_text("IDs: none (SRS)")
  }
  if (!is.null(p1$strata)) {
    n_strata <- length(unique(x@data[[p1$strata]]))
    cli::cli_text("Strata: {.field {p1$strata}} ({n_strata} strata)")
  } else {
    cli::cli_text("Strata: none")
  }
  if (!is.null(p1$weights)) {
    p1_wts <- x@data[[p1$weights]]
    weighted_n <- round(sum(p1_wts, na.rm = TRUE))
    cli::cli_text("Weights: {.field {p1$weights}}")
    cli::cli_text("Weighted N: {.val {weighted_n}}")
  }

  cli::cli_text("")
  cli::cli_h2("Phase 2 design")
  cli::cli_text("Subset: {.field {subset_var}}")
  p2 <- x@variables$phase2
  if (!is.null(p2$ids) && length(p2$ids) > 0L) {
    cli::cli_text("IDs: {.field {p2$ids}}")
  }
  if (!is.null(p2$strata)) {
    cli::cli_text("Strata: {.field {p2$strata}}")
  }

  cli::cli_text("")
  n_labeled <- length(x@metadata@variable_labels)
  n_total_vars <- ncol(x@data)
  cli::cli_text("Metadata: {n_labeled} of {n_total_vars} variable(s) labeled")

  invisible(x)
}


# ── print.survey_collection ────────────────────────────────────────────────
#
# Class defined in R/core-classes.R.
# Format (spec §3.5):
#   A survey_collection with {n} survey{?s}:
#     "{name}": {class_name}, {rows} rows, {vars} variables
# Abbreviation: when length(x) > 20, print first 10 + "  ... and {N} more"
# + last 3.

.fmt_collection_line <- function(nm, s) {
  rows <- format(nrow(s@data), big.mark = ",")
  vars <- format(ncol(s@data), big.mark = ",")
  cls <- sub("^[^:]+::", "", class(s)[[1L]])
  cli::cli_text(
    "  {.val {nm}}: {cls}, {rows} rows, {vars} variables"
  )
}

S7::method(print, survey_collection) <- function(x, ...) {
  surveys <- x@surveys
  n <- length(surveys)
  cli::cli_text("A {.cls survey_collection} with {n} survey{?s}:")

  if (length(x@groups) > 0L) {
    cli::cli_text("Groups: {.field {x@groups}}")
  }

  # Always render the per-call dispatch defaults — even when at their
  # built-in defaults — so users see the resolution rules in effect.
  cli::cli_text("id: {.val {x@id}}")
  cli::cli_text("if_missing_var: {.val {x@if_missing_var}}")

  nms <- names(surveys)
  if (n <= 20L) {
    for (i in seq_len(n)) {
      .fmt_collection_line(nms[[i]], surveys[[i]])
    }
  } else {
    for (i in seq_len(10L)) {
      .fmt_collection_line(nms[[i]], surveys[[i]])
    }
    cli::cli_text("  ... and {n - 13L} more")
    tail_idx <- seq.int(n - 2L, n)
    for (i in tail_idx) {
      .fmt_collection_line(nms[[i]], surveys[[i]])
    }
  }
  invisible(x)
}


# ── [[, length, names methods for survey_collection ──────────────────────────
# Class defined in R/core-classes.R.
#
# Note on `[[<-`: no `[[<-` method is registered. `S7::method("[[<-", ...)`
# registers the method in the S3methods metadata with the function object
# (rather than a character name) in the method column, which trips
# `R CMD check`'s `checkReplaceFuns` coercion path and surfaces as a
# spurious WARNING. Use `add_survey()` / `remove_survey()` to mutate the
# collection instead.

S7::method(`[[`, survey_collection) <- function(x, i) x@surveys[[i]]
S7::method(length, survey_collection) <- function(x) length(x@surveys)
S7::method(names, survey_collection) <- function(x) names(x@surveys)
