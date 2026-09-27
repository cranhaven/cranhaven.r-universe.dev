# R/utils.R
#
# Internal shared helpers used by 2+ verb files.
# Single-use helpers live at the top of their own source file.
#
# Functions defined here:
#   .protected_cols()               — columns that must never leave @data
#   .warn_physical_subset()         — standard warning for row-removal verbs
#   dplyr_reconstruct.survey_base() — class preservation in complex pipelines
#                                     (moved from R/filter.R on feature/select)
#   .validate_transform_args()      — validate .label/.description for transform fns
#   .set_recode_attrs()             — set label, labels, surveytidy_recode attrs

# ── surveycore internal wrappers ─────────────────────────────────────────────

# Wrappers around surveycore internal functions used by rename().
# Using get() + asNamespace() rather than surveycore::: avoids the
# "Unexported objects imported by ':::' calls" R CMD check NOTE.
#
# These wrappers are defined once here (R/utils.R) to avoid duplicating the
# get() calls in R/04-rename.R.

.sc_update_design_var_names <- function(variables, rename_map) {
  fn <- get(".update_design_var_names", envir = asNamespace("surveycore"))
  fn(variables, rename_map)
}

.sc_rename_metadata_keys <- function(metadata, rename_map) {
  fn <- get(".rename_metadata_keys", envir = asNamespace("surveycore"))
  fn(metadata, rename_map)
}


# ── survey_collection internal accessors ────────────────────────────────────

# Wrappers around surveycore internals used by the collection-layer
# group_by helper. Same get() + asNamespace() pattern as the rename wrappers
# above to avoid the ":::" R CMD check NOTE.
#
# .sc_propagate_or_match() and .sc_check_groups_match() are documented in
# surveycore as @keywords internal helpers shared with surveytidy's
# group_by.survey_collection (PR 2c).

.sc_propagate_or_match <- function(...) {
  fn <- get(".propagate_or_match", envir = asNamespace("surveycore"))
  fn(...)
}

.sc_check_groups_match <- function(...) {
  fn <- get(".check_groups_match", envir = asNamespace("surveycore"))
  fn(...)
}


# ── Column protection ─────────────────────────────────────────────────────────

# Returns the design-variable column names for any survey type.
#
# surveycore::.get_design_vars_flat() handles survey_taylor, survey_replicate,
# and survey_twophase but has no branch for survey_nonprob (returns
# character(0L) for that class). survey_nonprob carries only a weights
# column — no ids, strata, fpc, or repweights.
#
# This helper is the single authoritative source used by .protected_cols()
# and dplyr_reconstruct.survey_base().
.survey_design_var_names <- function(design) {
  if (S7::S7_inherits(design, surveycore::survey_nonprob)) {
    unique(c(design@variables$weights))
  } else {
    surveycore::.get_design_vars_flat(design)
  }
}

# Returns all column names that must never be removed from @data.
# Used by select(), rename(), mutate(), arrange(), and group_by() to enforce
# design variable protection.
#
# Protected columns are:
#   - All design variable columns (.survey_design_var_names())
#   - The domain indicator column (SURVEYCORE_DOMAIN_COL), if present
.protected_cols <- function(design) {
  c(
    .survey_design_var_names(design),
    surveycore::SURVEYCORE_DOMAIN_COL
  )
}


# ── dplyr_reconstruct() ───────────────────────────────────────────────────────

# dplyr 1.1.0+ calls dplyr_reconstruct(new_data, template) after many verbs
# (joins, across(), slice, etc.) to rebuild the output class. Without this,
# pipelines silently return a tibble instead of a survey object.
#
# Also cleans up visible_vars when dplyr internally removes non-design columns
# (e.g., via .keep = "none" mutations routed through dplyr's machinery).
# Registered in .onLoad() — see R/00-zzz.R.
#' @noRd
dplyr_reconstruct.survey_base <- function(data, template) {
  design_vars <- .survey_design_var_names(template)
  missing_vars <- setdiff(design_vars, names(data))
  if (length(missing_vars) > 0L) {
    cli::cli_abort(
      c(
        "x" = "Required design variable(s) removed: {.field {missing_vars}}.",
        "i" = "Design variables cannot be removed from a survey object.",
        "v" = "Use {.fn select} to hide columns without removing them."
      ),
      class = "surveycore_error_design_var_removed"
    )
  }
  # Clean up visible_vars if dplyr removed any referenced non-design columns
  if (!is.null(template@variables$visible_vars)) {
    vv <- intersect(template@variables$visible_vars, names(data))
    template@variables$visible_vars <- if (length(vv) == 0L) NULL else vv
  }
  template@data <- data
  template
}


# ── Physical subset warning ───────────────────────────────────────────────────

# Issues the standard warning for operations that physically remove rows.
# Used by subset.survey_base() (R/filter.R) and slice_*.survey_base()
# (R/slice.R) and drop_na.survey_base() (R/drop-na.R).
#
# fn_name: the function name shown in the warning message, e.g. "slice_head"
.warn_physical_subset <- function(fn_name) {
  cli::cli_warn(
    c(
      "!" = "{.fn {fn_name}} physically removes rows from the survey data.",
      "i" = paste0(
        "This is different from {.fn filter}, which preserves all rows ",
        "for correct variance estimation."
      ),
      "v" = "Use {.fn filter} for subpopulation analyses instead."
    ),
    class = "surveycore_warning_physical_subset"
  )
}


# ── mutate() metadata attribute helpers ──────────────────────────────────────

# Mapping from metadata property → column attribute name. Used by
# .attach_metadata_attrs(), .extract_metadata_attrs(), and .strip_metadata_attrs()
# to keep the mapping in one place.
.METADATA_ATTR_MAP <- list(
  variable_labels = "label",
  value_labels = "labels",
  question_prefaces = "question_preface",
  notes = "note",
  universe = "universe",
  missing_codes = "missing_codes"
)

# Pre-attachment: copy metadata into column attrs on the data.frame so
# recode functions called inside mutate() can read them via attr().
# Does NOT set the "haven_labelled" class — attrs only.
# Always-on: runs on every mutate() call; fast path returns early when
# @metadata has nothing to attach (negligible overhead for the common case).
.attach_metadata_attrs <- function(data, metadata) {
  data_cols <- names(data)
  for (prop in names(.METADATA_ATTR_MAP)) {
    entries <- S7::prop(metadata, prop)
    if (length(entries) == 0L) {
      next
    }
    attr_name <- .METADATA_ATTR_MAP[[prop]]
    for (col in names(entries)) {
      if (col %in% data_cols) {
        attr(data[[col]], attr_name) <- entries[[col]]
      }
    }
  }
  data
}

# Post-detection: inspect changed_cols in new_data for metadata attrs and sync
# them into @metadata. Three cases:
#   1. Column has "surveytidy_recode" attr → extract all attrs (recode path)
#   2. Column has any metadata attr without "surveytidy_recode" →
#      extract into metadata (haven::labelled / structure(, label =) path)
#   3. Column has no metadata attrs and previously had metadata → clear stale
#
# changed_cols : character vector — names of explicitly-named LHS expressions
#                from rlang::quos(...). Unnamed mutate expressions (e.g.,
#                across()) are not covered — accepted limitation for Phase 0.6.
# Returns updated metadata object (does NOT assign internally).
.extract_metadata_attrs <- function(data, metadata, changed_cols) {
  # Batch-read: pull all 6 metadata properties into a plain named list so we

  # can use `[[` freely (S7 objects don't support `[[`).
  meta_lists <- lapply(
    stats::setNames(names(.METADATA_ATTR_MAP), names(.METADATA_ATTR_MAP)),
    function(prop) S7::prop(metadata, prop)
  )

  for (col in changed_cols) {
    if (!col %in% names(data)) {
      next
    }

    recode_attr <- attr(data[[col]], "surveytidy_recode")

    # Read all metadata attrs from the column
    col_attrs <- lapply(.METADATA_ATTR_MAP, function(attr_name) {
      attr(data[[col]], attr_name, exact = TRUE)
    })
    has_any_attr <- any(!vapply(col_attrs, is.null, logical(1L)))

    if (!is.null(recode_attr)) {
      # Recode function output: extract all attrs. NULL values clear the entry,
      # which is correct — the old metadata is no longer valid.
      for (prop in names(.METADATA_ATTR_MAP)) {
        meta_lists[[prop]][[col]] <- col_attrs[[prop]]
      }
    } else if (has_any_attr) {
      # User-applied attrs (e.g., haven::labelled(), structure(, label =)):
      # sync non-NULL attrs into metadata; leave existing entries for attrs
      # that weren't set.
      for (prop in names(.METADATA_ATTR_MAP)) {
        if (!is.null(col_attrs[[prop]])) {
          meta_lists[[prop]][[col]] <- col_attrs[[prop]]
        }
      }
    } else {
      # No attrs and no recode: clear any stale metadata for this column.
      has_existing <- any(vapply(
        names(.METADATA_ATTR_MAP),
        function(prop) !is.null(meta_lists[[prop]][[col]]),
        logical(1L)
      ))
      if (has_existing) {
        for (prop in names(.METADATA_ATTR_MAP)) {
          meta_lists[[prop]][[col]] <- NULL
        }
      }
    }
  }

  # Batch-write: push all 6 properties back into the S7 metadata object.
  for (prop in names(.METADATA_ATTR_MAP)) {
    S7::prop(metadata, prop) <- meta_lists[[prop]]
  }
  metadata
}

# Strip: remove all metadata attrs and the surveytidy_recode attr from
# every column before storing new_data in @data. haven::zap_labels() removes
# "labels", "format.spss", "display_width", and the "haven_labelled"
# class. Additional attrs are removed explicitly.
.strip_metadata_attrs <- function(data) {
  all_metadata_attrs <- c(
    unname(unlist(.METADATA_ATTR_MAP)),
    "surveytidy_recode"
  )
  for (col in names(data)) {
    data[[col]] <- haven::zap_labels(data[[col]])
    for (a in all_metadata_attrs) {
      attr(data[[col]], a) <- NULL
    }
  }
  data
}


# ── recode helpers ────────────────────────────────────────────────────────────

# Validate .label, .value_labels, and .description arguments.
# Called by all six recode functions. Returns invisible(TRUE) on success.
.validate_label_args <- function(label, value_labels, description = NULL) {
  if (!is.null(label) && !(is.character(label) && length(label) == 1L)) {
    cli::cli_abort(
      c(
        "x" = "{.arg .label} must be a single character string.",
        "i" = "Got {.cls {class(label)}} of length {length(label)}."
      ),
      class = "surveytidy_error_recode_label_not_scalar"
    )
  }
  if (!is.null(value_labels) && is.null(names(value_labels))) {
    cli::cli_abort(
      c(
        "x" = "{.arg .value_labels} must be a named vector.",
        "i" = "Got an unnamed {.cls {class(value_labels)}}.",
        "v" = "Use {.code c(\"Label\" = value, ...)} to name the entries."
      ),
      class = "surveytidy_error_recode_value_labels_unnamed"
    )
  }
  if (
    !is.null(description) &&
      !(is.character(description) && length(description) == 1L)
  ) {
    cli::cli_abort(
      c(
        "x" = "{.arg .description} must be a single character string.",
        "i" = "Got {.cls {class(description)}} of length {length(description)}."
      ),
      class = "surveytidy_error_recode_description_not_scalar"
    )
  }
  invisible(TRUE)
}

# Wrap a result vector in haven::labelled() and set the surveytidy_recode attr.
# Called when at least one of .label or .value_labels is non-NULL.
# fn:  character(1) — the recode function name (e.g., "recode_values")
# var: character(1) or NULL — the column name; NULL for multi-input functions
#      (case_when, if_else) that may consume multiple source columns.
.wrap_labelled <- function(
  x,
  label,
  value_labels,
  description = NULL,
  fn = NULL,
  var = NULL
) {
  result <- haven::labelled(x, labels = value_labels, label = label)
  attr(result, "surveytidy_recode") <- list(
    fn = fn,
    var = var,
    description = description
  )
  result
}

# Extract unique right-hand-side values from `old ~ new` formulas passed
# through `...`. Used by recode_values() to derive factor levels when the
# formula interface is used instead of explicit `to`. Non-formula elements
# in `...` are ignored (dplyr::recode_values() will error on them anyway).
.formula_rhs_values <- function(...) {
  dots <- rlang::list2(...)
  rhs <- lapply(dots, function(e) {
    if (rlang::is_formula(e)) rlang::eval_tidy(rlang::f_rhs(e)) else NULL
  })
  rhs <- rhs[!vapply(rhs, is.null, logical(1L))]
  if (length(rhs) == 0L) {
    return(NULL)
  }
  unique(unlist(rhs, use.names = FALSE))
}

# Convert result to a factor with levels ordered by value_labels (if provided)
# or by formula_values / unique(to) (if value_labels is NULL).
.factor_from_result <- function(x, value_labels, formula_values) {
  if (!is.null(value_labels)) {
    levels <- names(value_labels)
  } else {
    levels <- formula_values
  }
  factor(x, levels = levels)
}

# Merge base_labels (from x's attr) with override_labels (.value_labels arg).
# override_labels entries replace matching base_labels entries by name; new
# override entries are appended. When two entries share the same numeric value
# (e.g. base has "Independent" = 3 and override adds "Independent/Other" = 3),
# the later (override) entry wins and the earlier (base) entry is dropped.
# Returns NULL when both inputs are NULL.
#
# result_values: optional vector of values that appear in the recoded result.
#   When non-NULL, base_labels entries whose value does NOT appear in
#   result_values are pruned before the merge. override_labels is never
#   pruned — user-supplied entries survive even if the value is absent.
#   Default NULL means no pruning (backward-compatible).
.merge_value_labels <- function(base_labels, override_labels,
                                result_values = NULL) {
  # Prune stale base entries before any other logic. override_labels is never
  # touched here — user-supplied labels survive even for absent values.
  if (!is.null(base_labels) && !is.null(result_values)) {
    base_labels <- base_labels[unname(base_labels) %in% result_values]
    if (length(base_labels) == 0L) base_labels <- NULL
  }

  if (is.null(base_labels) && is.null(override_labels)) {
    return(NULL)
  }
  if (is.null(base_labels)) {
    return(override_labels)
  }
  if (is.null(override_labels)) {
    return(base_labels)
  }
  merged <- base_labels
  for (nm in names(override_labels)) {
    merged[nm] <- override_labels[[nm]]
  }
  # haven::labelled() requires unique values. When override introduced a new
  # label name for an existing value, drop the earlier (base) entry so the
  # override takes precedence. fromLast = TRUE keeps the last occurrence.
  merged[!duplicated(unname(merged), fromLast = TRUE)]
}


# ── transform helpers ─────────────────────────────────────────────────────────

# Validate .label and .description for transform functions.
# error_class: the class to raise (different per function).
# Returns invisible(TRUE) on success.
.validate_transform_args <- function(label, description, error_class) {
  if (!is.null(label) && !rlang::is_string(label)) {
    cli::cli_abort(
      c(
        "x" = "{.arg .label} must be a single character string or {.code NULL}.",
        "i" = "Got {.cls {class(label)}} of length {length(label)}."
      ),
      class = error_class
    )
  }
  if (!is.null(description) && !rlang::is_string(description)) {
    cli::cli_abort(
      c(
        "x" = "{.arg .description} must be a single character string or {.code NULL}.",
        "i" = "Got {.cls {class(description)}} of length {length(description)}."
      ),
      class = error_class
    )
  }
  invisible(TRUE)
}

# Set label, labels, and surveytidy_recode attrs on a result vector.
# label:       character(1) or NULL - variable label
# labels:      named vector or NULL - value labels
# fn:          character(1) - function name (hardcoded per function)
# var:         character(1) or NULL - column name
# description: character(1) or NULL - user-supplied description
.set_recode_attrs <- function(result, label, labels, fn, var, description) {
  attr(result, "label") <- label
  attr(result, "labels") <- labels
  attr(result, "surveytidy_recode") <- list(
    fn = fn,
    var = var,
    description = description
  )
  result
}


# ── survey_result helpers ──────────────────────────────────────────────────────

# Restore class and .meta after NextMethod() strips them.
# Called by all survey_result passthrough verb methods.
.restore_survey_result <- function(result, old_class, old_meta) {
  attr(result, ".meta") <- old_meta
  class(result) <- old_class
  result
}

# Remove meta entries for columns that are no longer present in the result.
# Called by mutate.survey_result (.keep variants) and select.survey_result.
#
# meta : the .meta list
# kept_cols : character vector of column names remaining after the operation
#
# IMPORTANT: Only $group entries are pruned based on output column presence.
# $group keys are grouping variable names that ARE output columns
# (e.g., "group" for a result grouped by the "group" variable).
#
# $x keys are input focal variable names (e.g., "y1" for get_means()),
# NOT output column names (the estimate column is named "mean", not "y1").
# Pruning $x by output column presence would always null it out for
# get_means() results, which is wrong. $x is left unchanged.
#
# $numerator/$denominator are input variable names, same situation as $x.
# They are never pruned here.
.prune_result_meta <- function(meta, kept_cols) {
  # Prune group entries not in kept_cols (group keys ARE output column names)
  meta$group <- meta$group[names(meta$group) %in% kept_cols]
  meta
}

# Apply a rename map to both tibble column names and .meta key references.
#
# rename_map : named character vector, c(old_name = "new_name")
.apply_result_rename_map <- function(result, rename_map) {
  if (length(rename_map) == 0L) {
    return(result)
  }

  old_names <- names(rename_map)
  new_names <- unname(rename_map)

  # 1. Rename tibble columns
  col_pos <- match(old_names, names(result))
  names(result)[col_pos[!is.na(col_pos)]] <- new_names[!is.na(col_pos)]

  # 2. Update .meta
  m <- attr(result, ".meta")

  # group keys
  for (i in seq_along(old_names)) {
    idx <- match(old_names[i], names(m$group))
    if (!is.na(idx)) names(m$group)[idx] <- new_names[i]
  }

  # x keys
  if (!is.null(m$x)) {
    for (i in seq_along(old_names)) {
      idx <- match(old_names[i], names(m$x))
      if (!is.na(idx)) names(m$x)[idx] <- new_names[i]
    }
  }

  # numerator / denominator $name (get_ratios results only)
  if (!is.null(m$numerator$name) && m$numerator$name %in% old_names) {
    m$numerator$name <- new_names[match(m$numerator$name, old_names)]
  }
  if (!is.null(m$denominator$name) && m$denominator$name %in% old_names) {
    m$denominator$name <- new_names[match(m$denominator$name, old_names)]
  }

  attr(result, ".meta") <- m
  result
}
