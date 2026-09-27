# R/core-validators.R
#
# Layer 2 validator helpers for the surveycore package.
#
# These functions are called by constructors (Layer 3) and update_design()
# to validate input BEFORE creating S7 objects. All return invisible(TRUE)
# on success; call cli_abort() or cli_warn() on failure.
#
# The 3-layer validator architecture (per phase-0-implementation-plan-v2.md):
#   Layer 1 — S7 class validators    (R/core-classes.R)
#   Layer 2 — these helpers          (R/core-validators.R)  <-- this file
#   Layer 3 — constructor validation (R/core-constructors.R)
#
# No logic from one layer is duplicated in another.
# Error classes match plans/error-messages.md exactly.
# All functions are internal (not exported), prefixed with `.`.

#' Internal Validator Helpers
#'
#' Layer 2 reusable validation helpers called by constructors and
#' `update_design()` to validate input before creating S7 objects.
#' Return `invisible(TRUE)` on success; call [cli::cli_abort()] or
#' [cli::cli_warn()] on failure.
#'
#' @srrstats {G2.0} Constructors assert that named design columns (ids,
#'   weights, strata, fpc) are present in the supplied data frame and that
#'   each resolves to a valid column via tidyselect before object construction.
#'
#' @srrstats {G2.1} Type assertions are implemented here: weights must be
#'   numeric and positive; FPC must be numeric and non-missing; strata and
#'   ids must refer to columns that exist in @data.
#'
#' @srrstats {G2.1a} @param documentation for all constructor arguments states
#'   accepted data types (e.g. numeric for weights, character or factor for
#'   strata).
#'
#' @srrstats {G2.13} Pre-processing checks for missing values in design
#'   variables: NA weights cause surveycore_error_weights_na; NA FPC values
#'   cause surveycore_error_fpc_na.
#'
#' @srrstats {G2.14a} Validators error on missing values in design variables
#'   (weights, FPC) that must be fully observed for variance estimation.
#'
#' @srrstats {G2.15} No function passes data to base routines with a default
#'   na.rm = FALSE. All calls to mean(), sum(), var(), and similar functions
#'   use explicit na.rm = TRUE or receive pre-filtered data.
#'
#' @name validators
#' @keywords internal
#' @noRd
NULL


# ── .validate_data_frame ──────────────────────────────────────────────────────

# Validates the `data` argument shared by as_survey(), as_survey_replicate(),
# and
# as_survey_nonprob() (Layer 3, errors 1–4 from error-messages.md).
# Returns invisible(TRUE) on success; calls cli_abort()/cli_warn() otherwise.
#' @noRd
.validate_data_frame <- function(data, call = rlang::caller_call()) {
  # Error 1: data must be a data frame
  if (!is.data.frame(data)) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg data} must be a data frame, not ",
          "{.cls {class(data)[[1L]]}}"
        )
      ),
      class = "surveycore_error_not_data_frame",
      call = call
    )
  }

  # Error 2: data must have at least one row
  if (nrow(data) == 0L) {
    cli::cli_abort(
      c("x" = "{.arg data} must have at least one row"),
      class = "surveycore_error_empty_data",
      call = call
    )
  }

  # Error 3: column names must be unique
  if (anyDuplicated(names(data)) > 0L) {
    dupes <- unique(names(data)[duplicated(names(data))])
    cli::cli_abort(
      c(
        "x" = paste0(
          "Column names in {.arg data} must be unique. ",
          "Duplicates: {.field {dupes}}"
        )
      ),
      class = "surveycore_error_duplicate_names",
      call = call
    )
  }

  # Error 4: single-row data cannot support variance estimation
  if (nrow(data) == 1L) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg data} has only 1 row. A survey design requires ",
          "at least 2 observations."
        )
      ),
      class = "surveycore_error_single_row",
      call = call
    )
  }

  invisible(TRUE)
}


# ── .validate_weights ─────────────────────────────────────────────────────────

# Validates that `weights_var` column exists in `data`, is numeric, and has
# all non-NA values strictly positive (with the all-zero/all-missing case
# caught first for a more actionable error).
#' @noRd
.validate_weights <- function(weights_var, data) {
  # Existence check
  if (!weights_var %in% names(data)) {
    cli::cli_abort(
      c(
        "x" = "Weight column {.field {weights_var}} not found in {.arg data}.",
        "i" = "Available columns: {.field {names(data)}}."
      ),
      class = "surveycore_error_design_var_missing"
    )
  }

  wt_col <- data[[weights_var]]

  # Numeric check
  if (!is.numeric(wt_col)) {
    cli::cli_abort(
      c(
        "x" = "Weight column {.field {weights_var}} must be numeric.",
        "i" = "Got {.cls {class(wt_col)}}.",
        "v" = "Convert with {.code as.numeric({.field {weights_var}})}."
      ),
      class = "surveycore_error_weights_not_numeric"
    )
  }

  # All zero or all missing → no valid weights at all
  non_na <- wt_col[!is.na(wt_col)]
  if (length(non_na) == 0L || all(non_na == 0)) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "All values in weight column {.field {weights_var}} ",
          "are zero or missing."
        ),
        "i" = "No valid (non-NA, positive) weights found.",
        "v" = paste0(
          "Check {.field {weights_var}} for data issues or ",
          "supply a different column."
        )
      ),
      class = "surveycore_error_weights_all_zero"
    )
  }

  # Non-positive check (some bad weights mixed with good ones)
  n_bad <- sum(non_na <= 0)
  if (n_bad > 0L) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "Weight column {.field {weights_var}} has ",
          "{n_bad} non-positive value(s)."
        ),
        "i" = "All non-NA weights must be strictly greater than 0.",
        "v" = paste0(
          "Remove or replace rows where {.field {weights_var}} ",
          "is 0 or negative."
        )
      ),
      class = "surveycore_error_weights_nonpositive"
    )
  }

  invisible(TRUE)
}


# ── .validate_design_vars ─────────────────────────────────────────────────────

# Validates that all column names in `vars` exist in `data` and are atomic
# (not list-columns). Pass `vars = NULL` or `vars = character(0)` to no-op.
#' @noRd
.validate_design_vars <- function(vars, data) {
  if (is.null(vars) || length(vars) == 0L) {
    return(invisible(TRUE))
  }

  # Existence check
  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0L) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "Design variable(s) not found in {.arg data}: ",
          "{.field {missing_vars}}."
        )
      ),
      class = "surveycore_error_design_var_missing"
    )
  }

  # Atomic check (not list-columns)
  for (v in vars) {
    if (!is.atomic(data[[v]])) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "Design variable {.field {v}} is a list-column. ",
            "Design variables must be atomic vectors."
          )
        ),
        class = "surveycore_error_design_var_list"
      )
    }
  }

  invisible(TRUE)
}


# ── .validate_fpc ─────────────────────────────────────────────────────────────

# Validates that `fpc_var` column exists in `data` and contains no NA values.
# Pass `fpc_var = NULL` to no-op.
#' @noRd
.validate_fpc <- function(fpc_var, data) {
  if (is.null(fpc_var)) {
    return(invisible(TRUE))
  }

  # Existence check
  if (!fpc_var %in% names(data)) {
    cli::cli_abort(
      c(
        "x" = "FPC column {.field {fpc_var}} not found in {.arg data}.",
        "i" = "Available columns: {.field {names(data)}}."
      ),
      class = "surveycore_error_design_var_missing"
    )
  }

  fpc_col <- data[[fpc_var]]
  n_na <- sum(is.na(fpc_col))

  if (n_na > 0L) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg fpc} column {.field {fpc_var}} contains {n_na} ",
          "NA value(s). FPC must be fully observed."
        ),
        "v" = paste0(
          "Remove rows with missing FPC or set {.arg fpc = NULL} to ",
          "omit the correction."
        )
      ),
      class = "surveycore_error_fpc_na"
    )
  }

  invisible(TRUE)
}


# ── .validate_repweights ──────────────────────────────────────────────────────

# Validates that all columns in `repweights_vars` exist in `data` and are
# numeric. Pass `repweights_vars = NULL` or `repweights_vars = character(0)`
# to no-op.
#' @noRd
.validate_repweights <- function(repweights_vars, data) {
  if (is.null(repweights_vars) || length(repweights_vars) == 0L) {
    return(invisible(TRUE))
  }

  # Existence check
  missing_vars <- setdiff(repweights_vars, names(data))
  if (length(missing_vars) > 0L) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "Replicate weight column(s) not found in {.arg data}: ",
          "{.field {missing_vars}}."
        )
      ),
      class = "surveycore_error_design_var_missing"
    )
  }

  # Numeric check — collect ALL non-numeric columns before aborting
  # (consistent with .validate_design_vars() which collects all missing vars)
  not_numeric <- repweights_vars[
    !vapply(
      repweights_vars,
      function(rw) is.numeric(data[[rw]]),
      logical(1L)
    )
  ]
  if (length(not_numeric) > 0L) {
    bad_classes <- vapply(
      not_numeric,
      function(rw) class(data[[rw]])[[1L]],
      character(1L)
    )
    cli::cli_abort(
      c(
        "x" = paste0(
          "{length(not_numeric)} replicate weight column(s) are not numeric: ",
          "{.field {not_numeric}}."
        ),
        "i" = "Classes: {.cls {bad_classes}}."
      ),
      class = "surveycore_error_repweights_not_numeric"
    )
  }

  invisible(TRUE)
}


# ── .validate_psu_strata ──────────────────────────────────────────────────────

# Issues a warning if any PSU (first stage of `ids`) appears in more than one
# stratum. Pass `ids = NULL` or `strata = NULL` to no-op. This check is skipped
# when `nest = TRUE` (caller responsibility — this function does not take nest).
#' @noRd
.validate_psu_strata <- function(ids, strata, data) {
  if (is.null(ids) || length(ids) == 0L || is.null(strata)) {
    return(invisible(TRUE))
  }

  psu_col <- as.character(data[[ids[[1L]]]])
  strata_col <- as.character(data[[strata]])

  psu_n_strata <- tapply(strata_col, psu_col, function(s) length(unique(s)))
  multi_strata_psus <- names(psu_n_strata)[psu_n_strata > 1L]

  if (length(multi_strata_psus) > 0L) {
    cli::cli_warn(
      c(
        "!" = paste0(
          "Some PSUs appear in more than one stratum: ",
          "{.val {head(multi_strata_psus, 5L)}}. ",
          "If PSUs are nested within strata, set {.code nest = TRUE}."
        )
      ),
      class = "surveycore_warning_psu_multi_strata"
    )
  }

  invisible(TRUE)
}


# ── .validate_rscales ─────────────────────────────────────────────────────────

# Validates that `rscales` has exactly `n_rep` elements, matching the number
# of replicate weight columns. Pass `rscales = NULL` to no-op.
#' @noRd
.validate_rscales <- function(rscales, n_rep) {
  if (is.null(rscales)) {
    return(invisible(TRUE))
  }

  if (length(rscales) != n_rep) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "Length of {.arg rscales} ({length(rscales)}) must equal ",
          "number of replicate weights ({n_rep})."
        )
      ),
      class = "surveycore_error_rscales_length"
    )
  }

  # Check for non-numeric, NA, or negative values.
  n_na <- sum(is.na(rscales))
  n_neg <- sum(rscales[!is.na(rscales)] < 0)
  if (!is.numeric(rscales) || n_na > 0L || n_neg > 0L) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg rscales} must be a non-negative numeric vector ",
          "with no NA values."
        ),
        "i" = paste0(
          "Got {n_na} NA value(s) and/or {n_neg} negative value(s)."
        ),
        "v" = paste0(
          "Supply a numeric vector of length {n_rep} with ",
          "all values >= 0."
        )
      ),
      class = "surveycore_error_rscales_na"
    )
  }

  invisible(TRUE)
}


# ── .update_design_var_names ──────────────────────────────────────────────────

# Updates design variable name references in a `@variables` list according to
# `rename_map`. Returns the updated list. Intended for use by rename methods,
# which reconstruct the survey object atomically (new data + new variables in
# a single constructor call) to avoid intermediate S7 validator failures.
#
# `variables`:  the `@variables` list from a survey design object.
# `rename_map`: named character vector where names are old column names and
#               values are the corresponding new names
#               (e.g., c(old_wt = "weight")).
#' @noRd
.update_design_var_names <- function(variables, rename_map) {
  vars <- variables

  # Update scalar design variables (weights, strata, fpc)
  for (slot in c("weights", "strata", "fpc")) {
    if (!is.null(vars[[slot]]) && vars[[slot]] %in% names(rename_map)) {
      vars[[slot]] <- rename_map[[vars[[slot]]]]
    }
  }

  # Update ids (character vector, may have multiple stages)
  if (!is.null(vars$ids)) {
    matched <- vars$ids %in% names(rename_map)
    vars$ids[matched] <- rename_map[vars$ids[matched]]
  }

  # Update replicate weight column names (survey_replicate only)
  if (!is.null(vars$repweights)) {
    matched <- vars$repweights %in% names(rename_map)
    vars$repweights[matched] <- rename_map[vars$repweights[matched]]
  }

  vars
}


# ── .rename_metadata_keys ─────────────────────────────────────────────────────

# Renames keys (variable names) in all metadata list properties to stay in
# sync after a column rename. Called alongside .update_design_var_names().
#
# `rename_map`: same named character vector as .update_design_var_names().
#' @noRd
.rename_metadata_keys <- function(metadata, rename_map) {
  # Renames matched keys in a named list; leaves others unchanged.
  .rename_list_keys <- function(lst, map) {
    if (length(lst) == 0L || is.null(names(lst))) {
      return(lst)
    }
    matched <- names(lst) %in% names(map)
    names(lst)[matched] <- map[names(lst)[matched]]
    lst
  }

  metadata@variable_labels <- .rename_list_keys(
    metadata@variable_labels,
    rename_map
  )
  metadata@value_labels <- .rename_list_keys(metadata@value_labels, rename_map)
  metadata@question_prefaces <- .rename_list_keys(
    metadata@question_prefaces,
    rename_map
  )
  metadata@notes <- .rename_list_keys(metadata@notes, rename_map)
  metadata@sata <- .rename_list_keys(metadata@sata, rename_map)
  metadata@transformations <- .rename_list_keys(
    metadata@transformations,
    rename_map
  )

  metadata
}


# ── .delete_metadata_col ──────────────────────────────────────────────────────

# Deletes per-column metadata for `col` from every @metadata slot.
# Used by surveytidy's select() when a column is dropped from a survey design.
# Leaves @metadata@weighting_history untouched — that slot is design-level, not
# per-column.
#
# `design`: a survey_base subclass object.
# `col`:    character(1) — the column name to purge from all metadata slots.
#' @noRd
.delete_metadata_col <- function(design, col) {
  md <- design@metadata
  md@variable_labels[[col]] <- NULL
  md@value_labels[[col]] <- NULL
  md@question_prefaces[[col]] <- NULL
  md@notes[[col]] <- NULL
  md@transformations[[col]] <- NULL
  design@metadata <- md
  design
}
