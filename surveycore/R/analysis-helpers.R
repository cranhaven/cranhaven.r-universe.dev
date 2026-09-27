# R/analysis-helpers.R
#
# Shared internal helpers for Phase 1 analysis functions.
# All functions are unexported. Single-use helpers for individual analysis
# functions live at the top of their respective source files.
#
# Contents:
#   Meta-key constants (six character vectors)
#   .extract_var_meta()     — variable/value label lookup for one variable
#   .build_group_meta()     — metadata lookup for all group variables
#   .apply_group_labels()   — convert coded group columns to labelled factors
#   .build_group_combos()   — unique group value combinations from domain data
#   .match_group_combo()    — row mask matching one group combination
#   .degf_taylor()          — Taylor df formula (retained; not called by
#                             .degf())
#   .resolve_groups()       — combine @groups + group= arg
#   .apply_domain()         — extract domain membership mask
#   .build_meta()           — assemble .meta list
#   .make_result_tibble()   — assemble result tibble + attach metadata
#   .validate_shared_args() — validate variance/conf_level/name_style/decimals
#   .apply_decimals()       — round numeric output columns
#   .apply_name_style()     — rename columns for broom compatibility
#   .check_unsupported_class() — throw for non-survey-base objects
#   .add_variance_cols()    — compute requested uncertainty columns
#   .degf()                 — design degrees of freedom

#' Analysis Helper Standards
#'
#' @srrstats {EA2.4} surveycore uses an explicit S7 class system
#'   (survey_taylor, survey_replicate, survey_twophase) that encodes the design
#'   type and all associated parameters.
#'
#' @srrstats {EA2.6} Routines process vector columns regardless of additional
#'   attributes: haven-style "label" and "labels" attributes are automatically
#'   detected and preserved in @metadata during construction, and restored in
#'   analysis output when requested.
#'
#' @srrstats {EA3.0} The get_*() family (get_freqs(), get_means(),
#'   get_totals(), get_corr(), get_quantiles(), get_ratios(), get_diffs())
#'   provides automated, reproducible extraction of survey statistics without
#'   requiring manual formula-writing or post-processing.
#'
#' @srrstats {EA3.1} All get_*() functions return tidy tibbles with consistent
#'   column naming conventions, enabling direct comparison of estimates across
#'   variables, groups, and designs.
#'
#' @srrstats {EA4.0} All get_*() functions return tibble-based objects;
#'   survey_glm() returns a survey_glm_fit S7 object. Return types do not
#'   vary by input class within the accepted input space.
#'
#' @srrstats {EA4.1} All seven get_*() analysis functions implement a
#'   decimals parameter (integer or NULL) that rounds all numeric output
#'   columns to the specified number of decimal places. Numeric precision
#'   control is consistently available across the full analysis API.
#'
#' @srrstats {EA4.2} All survey design objects have a print method that
#'   provides a structured summary (design type, sample size, variable
#'   preview). survey_glm_fit has both print and summary methods.
#'
#' @srrstats {EA5.2} The print.survey_glm_fit() and print.survey_glm_summary()
#'   methods format numeric output via format(round(x, digits), nsmall =
#'   digits) rather than relying on default numeric printing.
#'
#' @srrstats {EA5.3} The tibble print method for get_*() output includes
#'   column type abbreviations (<dbl>, <int>, <fct>, etc.), indicating the
#'   class and storage mode of each column.
#'
#' @srrstats {EA6.0} Return values from all get_*() functions are tested for
#'   class, dimensions, column names, column types, and numeric values.
#'
#' @srrstats {EA6.0a} Tests verify the class of all return objects: get_*()
#'   functions return tibbles; constructors return the correct S7 subclass.
#'
#' @srrstats {EA6.0b} Tests verify the dimensions (number of rows and columns)
#'   of all tabular return objects for representative inputs.
#'
#' @srrstats {EA6.0c} Tests verify that returned tibbles carry the expected
#'   column names (e.g. mean, se, ci_low, ci_high, n for get_means()).
#'
#' @srrstats {EA6.0d} Tests verify column classes within returned tibbles
#'   (e.g. numeric estimates, integer counts, factor group columns).
#'
#' @srrstats {EA6.0e} Numeric return values are tested with
#'   expect_equal(..., tolerance = 1e-10) or equivalent against reference
#'   values from the survey package.
#'
#' @noRd
NULL


# ── Meta-key constants ────────────────────────────────────────────────────────
#
# Required meta_args keys for each get_*() function. Pass the appropriate
# constant to .make_result_tibble() as required_meta_keys. The nested
# structure uses "group" and "x" (or "numerator"/"denominator" for ratios)
# as top-level keys, replacing the old flat key sets.
# These are the single source of truth for each function's meta_args contract.
# Adding a new meta field requires updating only the constant here.
#
# FAMILY_META_KEYS is the shared base for the seven analysis functions that
# share the "group + x" nested shape. Per-function constants derive from it
# by concatenating additional keys. get_ratios() and get_anova() have
# distinct shapes and are defined independently.

FAMILY_META_KEYS <- c("group", "x")

FREQS_META_KEYS <- FAMILY_META_KEYS
MEANS_META_KEYS <- FAMILY_META_KEYS
TOTALS_META_KEYS <- FAMILY_META_KEYS
VARIANCE_META_KEYS <- FAMILY_META_KEYS
CORR_META_KEYS <- c(FAMILY_META_KEYS, "method")
COVARIANCE_META_KEYS <- c(FAMILY_META_KEYS, "method")
QUANTILES_META_KEYS <- c(FAMILY_META_KEYS, "probs")
T_TEST_META_KEYS <- c(FAMILY_META_KEYS, "by")
PAIRWISE_META_KEYS <- c(FAMILY_META_KEYS, "by", "pval_adj")
EFFECTIVE_N_META_KEYS <- c(FAMILY_META_KEYS, "method")
DIFFS_META_KEYS <- c(
  FAMILY_META_KEYS,
  "treats",
  "covariates",
  "family",
  "link",
  "pval_adj",
  "estimate_method",
  "mean_method",
  "estimate_scale"
)
RATIOS_META_KEYS <- c("group", "numerator", "denominator")
ANOVA_META_KEYS <- c("model", "method", "test", "terms")


#' Extract variable metadata from a survey design
#'
#' Returns a named list with `variable_label`, `question_preface`, and
#' `value_labels` for a single variable. Checks `@metadata` first; falls back
#' to haven-style attributes on the column when `@metadata` has no entry. For
#' factor columns with no haven labels, surfaces `levels()` as `value_labels`
#' in haven format: a named integer vector where names are level strings and
#' values are sequential integers starting at `1L`.
#'
#' @param design   A survey design object.
#' @param var_name Character(1). Column name in `design@data`.
#' @return Named list with keys `variable_label`, `question_preface`,
#'   `value_labels`, `sata`, and `higher_is`.
#' @noRd
.extract_var_meta <- function(design, var_name) {
  col <- design@data[[var_name]]

  variable_label <- design@metadata@variable_labels[[var_name]] %||%
    attr(col, "label", exact = TRUE)

  question_preface <- design@metadata@question_prefaces[[var_name]]

  value_labels <- design@metadata@value_labels[[var_name]] %||%
    attr(col, "labels", exact = TRUE)

  if (is.null(value_labels) && is.factor(col)) {
    lvls <- levels(col)
    value_labels <- stats::setNames(seq_along(lvls), lvls)
    storage.mode(value_labels) <- "integer"
  }

  sata <- isTRUE(design@metadata@sata[[var_name]])

  higher_is <- design@metadata@higher_is[[var_name]] %||% NULL

  list(
    variable_label = variable_label,
    question_preface = question_preface,
    value_labels = value_labels,
    sata = sata,
    higher_is = higher_is
  )
}


#' Build group metadata list for all group variables
#'
#' Returns a named list with one entry per group variable, each being the
#' output of `.extract_var_meta()`. Returns `list()` when `group_vars` is
#' empty or has length 0.
#'
#' @param design     A survey design object.
#' @param group_vars Character vector of group variable names.
#' @return Named list of per-variable metadata lists, named by variable name.
#' @noRd
.build_group_meta <- function(design, group_vars) {
  if (length(group_vars) == 0L) {
    return(list())
  }
  meta <- lapply(group_vars, function(gv) .extract_var_meta(design, gv))
  stats::setNames(meta, group_vars)
}


#' Apply value labels to group combination columns
#'
#' Converts coded group columns in a `group_combos` data frame to labelled
#' factors. When `label_values = FALSE`, returns `group_combos` unchanged.
#'
#' Haven-labelled columns produce a factor whose levels are the label strings
#' ordered by code value (ascending). Plain R factor columns are re-factored
#' using the original `levels()` order exactly. All other column types
#' (unlabelled integer, character, etc.) are returned unchanged.
#'
#' **Important:** must be called *after* `group_combos` is sorted on raw
#' codes. Sorting after label conversion would use factor level order instead
#' of raw numeric order.
#'
#' @param group_combos A data.frame of group variable columns.
#' @param group_vars   Character vector of group variable names. Must be column
#'   names in both `group_combos` and `design@data`.
#' @param design       A survey design object.
#' @param label_values Logical(1). When `FALSE`, returns `group_combos`
#'   unmodified. Default `TRUE`.
#' @return The (possibly modified) `group_combos` data frame, with coded
#'   columns replaced by labelled factors where applicable.
#' @noRd
.apply_group_labels <- function(
  group_combos,
  group_vars,
  design,
  label_values = TRUE
) {
  if (!label_values) {
    return(group_combos)
  }
  for (gv in group_vars) {
    col <- group_combos[[gv]]
    src_col <- design@data[[gv]]

    labels <- design@metadata@value_labels[[gv]] %||%
      attr(src_col, "labels", exact = TRUE)

    if (!is.null(labels)) {
      # labels: c("Male" = 1, "Female" = 2) — names=label strings, values=codes
      # Build a map from regular code (as character) to label string.
      label_map <- stats::setNames(names(labels), as.character(unname(labels)))

      # Build a map from haven tagged-NA tag character to label string.
      # Requires haven at runtime; falls back gracefully when not installed:
      # tagged NAs without a resolvable tag remain NA in the factor output.
      haven_ok <- requireNamespace("haven", quietly = TRUE)
      # Tagged NAs are always doubles (special NaN bit patterns). Build a map
      # from tag character to label string by checking double NA entries in
      # labels. Non-double NA entries (integer NA, etc.) are plain NAs.
      tagged_na_map <- list()
      if (haven_ok) {
        for (i in seq_along(labels)) {
          lv <- labels[[i]]
          if (is.na(lv) && is.double(lv)) {
            tag <- haven::na_tag(lv)
            if (!is.na(tag)) tagged_na_map[[tag]] <- names(labels)[[i]]
          }
        }
      }

      labeled_col <- vapply(
        col,
        function(val) {
          if (!is.na(val)) {
            lbl <- label_map[as.character(val)]
            if (is.na(lbl)) NA_character_ else lbl
          } else if (haven_ok && is.double(val)) {
            # Double NA: may be a haven tagged NA — try to resolve to label
            tag <- haven::na_tag(val)
            if (!is.na(tag) && !is.null(tagged_na_map[[tag]])) {
              tagged_na_map[[tag]]
            } else {
              NA_character_
            }
          } else {
            NA_character_
          }
        },
        character(1L)
      )

      # Build factor levels from label names, excluding plain-NA label entries.
      # Tagged-NA label entries (double NAs with a haven tag) ARE included.
      is_plain_na_label <- vapply(
        labels,
        function(x) {
          if (!is.na(x)) {
            return(FALSE)
          }
          if (!haven_ok || !is.double(x)) {
            return(TRUE)
          }
          is.na(haven::na_tag(x))
        },
        logical(1L)
      )
      group_combos[[gv]] <- factor(
        labeled_col,
        levels = names(labels)[!is_plain_na_label]
      )
    } else if (is.factor(src_col)) {
      # Re-factor preserving original level order
      group_combos[[gv]] <- factor(as.character(col), levels = levels(src_col))
    }
    # else: leave unchanged (stays integer/numeric/character as-is)
  }
  group_combos
}


#' Build unique group value combinations from domain data
#'
#' Builds the data frame of unique group value combinations from `domain_data`
#' (a data frame already filtered to the active domain rows, containing only
#' group variable columns). When `na.rm = TRUE`, rows with any `NA` are
#' excluded before `unique()`. Output is sorted: non-`NA` combos first
#' (ascending by leftmost group variable), then `NA`-containing combos.
#'
#' @param domain_data data.frame. Rows are the active domain; columns are group
#'   variables only — not the full design data frame.
#' @param na.rm Logical. If `TRUE`, rows with any `NA` in any group column are
#'   excluded before computing unique combinations.
#' @return data.frame of unique group combinations, sorted with non-`NA` rows
#'   first.
#' @noRd
# Sync note: spec §II is authoritative — keep this block in sync with the spec
# if either changes. Do not edit one without updating the other.
.build_group_combos <- function(domain_data, na.rm) {
  if (na.rm) {
    domain_data <- domain_data[
      stats::complete.cases(domain_data),
      ,
      drop = FALSE
    ]
  }
  combos <- unique(domain_data)
  if (nrow(combos) == 0L) {
    return(combos)
  }
  # Sort: non-NA combos first, NA combos last.
  # Leftmost group variable is the primary sort key.
  # unname() prevents column names from colliding with order()'s named
  # formals (decreasing, method, na.last) if a group var shares a name.
  # rownames reset AFTER subsetting — before would leave non-sequential names.
  sort_vecs <- unname(lapply(names(combos), function(v) combos[[v]]))
  ord <- do.call(order, c(sort_vecs, list(na.last = TRUE)))
  combos <- combos[ord, , drop = FALSE]
  rownames(combos) <- NULL
  combos
}


#' Match rows to a single group value combination
#'
#' Returns a logical vector indicating which rows in `data_cols` match the
#' single group combination `combo_row`. Handles `NA` correctly: when
#' `combo_row[[gv]]` is `NA`, matches rows where `data_cols[[gv]]` is also
#' `NA` (using `is.na()` rather than `==`, which would not match `NA`).
#'
#' `data_cols` must be full-design-length (not domain-filtered). Build it as
#' `as.list(design@data[group_vars])` and combine with `domain_mask` after:
#' `active_mask <- domain_mask & .match_group_combo(data_cols, combo_row)`.
#'
#' @param data_cols Named list. One element per group variable, each a vector
#'   of length `nrow(design@data)` — the **full** design, not domain-filtered.
#' @param combo_row Single-row data.frame whose column names match
#'   `names(data_cols)`.
#' @return Logical vector of length `length(data_cols[[1]])`. `TRUE` where the
#'   row matches every column of `combo_row`.
#' @noRd
# Sync note: spec §II is authoritative — keep this block in sync with the spec
# if either changes. Do not edit one without updating the other.
.match_group_combo <- function(data_cols, combo_row) {
  match_vec <- rep(TRUE, length(data_cols[[1L]]))
  for (gv in names(combo_row)) {
    gv_col <- data_cols[[gv]]
    cv <- combo_row[[gv]]
    if (is.na(cv)) {
      match_vec <- match_vec & is.na(gv_col)
    } else {
      match_vec <- match_vec & !is.na(gv_col) & (gv_col == cv)
    }
  }
  match_vec
}


#' Compute Taylor series degrees of freedom
#'
#' Computes design-based degrees of freedom from a data frame and variables
#' list using the Taylor series rules. Retained for potential future use (e.g.,
#' design effect computation, user-accessible `degf()`). Not called by
#' `.degf()` — Phase 1 analysis functions use `Inf` directly (normal
#' approximation, matching `survey::svymean()` defaults).
#'
#' Rules applied, in order:
#' 1. Stratified cluster: `Σ_h(n_h - 1)` = total PSUs − number of strata.
#' 2. Unstratified cluster: `n_psus - 1`.
#' 3. Stratified, no PSUs: `n_obs - n_strata`.
#' 4. No structure (SRS): `n - 1`.
#'
#' @param data A data.frame (the survey design's `@data`).
#' @param vars A named list with `ids`, `strata`, and `nest` keys, taken from
#'   the design's `@variables`.
#' @return Numeric(1). Degrees of freedom (non-negative integer).
#' @noRd
.degf_taylor <- function(data, vars) {
  ids_var <- vars$ids
  strata_var <- vars$strata
  nest <- isTRUE(vars$nest)

  if (!is.null(strata_var) && !is.null(ids_var)) {
    # Stratified cluster: Σ(n_h - 1) = total_psus - n_strata
    psu_col <- as.character(data[[ids_var[[1L]]]])
    strata_col <- as.character(data[[strata_var]])
    if (nest) {
      # Make PSU IDs globally unique across strata before counting
      psu_col <- paste(strata_col, psu_col, sep = ".")
    }
    n_psus_per_stratum <- tapply(
      psu_col,
      strata_col,
      function(x) length(unique(x))
    )
    sum(n_psus_per_stratum) - length(n_psus_per_stratum)
  } else if (!is.null(ids_var)) {
    # Unstratified cluster: n_psus - 1
    psu_col <- as.character(data[[ids_var[[1L]]]])
    length(unique(psu_col)) - 1L
  } else if (!is.null(strata_var)) {
    # Stratified but no PSUs: n_obs - n_strata
    strata_col <- as.character(data[[strata_var]])
    nrow(data) - length(unique(strata_col))
  } else {
    # No design structure (SRS): n - 1
    nrow(data) - 1L
  }
}


#' Resolve the active grouping variables for a get_*() call
#'
#' Combines grouping variables from `design@groups` (set by
#' `surveytidy::group_by()`) with the `group =` argument supplied directly to
#' the analysis function. Both sources are applied simultaneously (AND
#' semantics). Returns a deduplicated character vector of group variable names,
#' or `character(0)` when no groups are active.
#'
#' @param design     A survey design object.
#' @param group_expr A quosure from `rlang::enquo(group)`.
#' @return Character vector of group variable names, with duplicates removed.
#' @noRd
.resolve_groups <- function(design, group_expr) {
  from_groups_prop <- design@groups
  from_arg <- .resolve_tidy_select(group_expr, design@data)
  unique(c(from_groups_prop, from_arg))
}


#' Extract the active domain membership mask
#'
#' Returns a logical vector indicating which rows of `design@data` belong to
#' the active estimation domain. When no domain column is present, all rows are
#' considered in-domain (all `TRUE`).
#'
#' Domain rows are **not** physically removed from the data — the full design
#' is retained for correct variance estimation; only the estimation sum is
#' restricted to in-domain rows.
#'
#' @param design A survey design object.
#' @return Logical vector of length `nrow(design@data)`. `TRUE` for in-domain
#'   rows.
#' @noRd
.apply_domain <- function(design) {
  if (SURVEYCORE_DOMAIN_COL %in% names(design@data)) {
    design@data[[SURVEYCORE_DOMAIN_COL]]
  } else {
    rep(TRUE, nrow(design@data))
  }
}


#' Assemble the structured .meta list for a result tibble
#'
#' Assembles the `.meta` attribute that is attached to all `survey_result`
#' tibbles. Derives `design_type` and `n_respondents` automatically from the
#' design object, then prepends them to `meta_args`.
#'
#' Valid `design_type` strings: `"taylor"`, `"replicate"`, `"twophase"`,
#' `"nonprob"`. Downstream code that reads `.meta` branches on these exact
#' strings.
#'
#' @param design    A survey design object.
#' @param meta_args Named list of function-supplied metadata. Must include the
#'   common fields (`conf_level`, `call`, `group_names`, `group_labels`) plus
#'   all function-specific fields defined in the relevant `*_META_KEYS`
#'   constant.
#' @return Named list representing the full `.meta` structure, with
#'   `design_type` and `n_respondents` prepended.
#' @noRd
.build_meta <- function(design, meta_args) {
  design_type <-
    if (S7::S7_inherits(design, survey_taylor)) {
      "taylor"
    } else if (S7::S7_inherits(design, survey_replicate)) {
      "replicate"
    } else if (S7::S7_inherits(design, survey_twophase)) {
      "twophase"
    } else if (S7::S7_inherits(design, survey_nonprob)) {
      "nonprob"
    } else {
      cli::cli_abort(
        c("x" = "Unrecognized design class {.cls {class(design)[1L]}}."),
        class = "surveycore_error_unsupported_class"
      )
    }

  c(
    list(
      design_type = design_type,
      n_respondents = as.integer(nrow(design@data))
    ),
    meta_args
  )
}


#' Assemble a survey_result tibble with a .meta attribute
#'
#' Builds a `survey_result` tibble from pre-computed column vectors, attaches
#' the `.meta` attribute via `.build_meta()`, and assigns the S3 class. Uses
#' column-by-column list accumulation to avoid a `vctrs` or `dplyr` runtime
#' dependency.
#'
#' @param col_vecs Named list. One named vector per result column (e.g.,
#'   `mean`, `se`, `ci_low`, `ci_high`, `n`). Vectors must be the same length.
#' @param groups_df data.frame of group variable columns to prepend. Pass
#'   `data.frame()` when there are no grouping variables.
#' @param class_name Character(1). S3 sub-class name for the result object,
#'   e.g., `"survey_means"` or `"survey_freqs"`.
#' @param design A survey design object passed to `.build_meta()`.
#' @param meta_args Named list of metadata for the `.meta` attribute.
#' @param required_meta_keys Character vector of required function-specific
#'   keys that must be present in `meta_args`. Always pass the relevant
#'   `*_META_KEYS` constant; omitting this argument is a programmer error.
#' @return A tibble with class
#'   `c(class_name, "survey_result", "tbl_df", "tbl", "data.frame")` and a
#'   `.meta` attribute.
#' @noRd
.make_result_tibble <- function(
  col_vecs,
  groups_df,
  class_name,
  design,
  meta_args,
  required_meta_keys
) {
  stopifnot(all(required_meta_keys %in% names(meta_args)))
  result <- tibble::as_tibble(c(as.list(groups_df), col_vecs))
  attr(result, ".meta") <- .build_meta(design, meta_args)
  class(result) <- c(class_name, "survey_result", "tbl_df", "tbl", "data.frame")
  result
}


#' Validate shared arguments across all get_*() functions
#'
#' Validates the cross-cutting arguments that appear on every `get_*()`
#' function: `variance`, `conf_level`, `name_style`, `decimals`, and `na.rm`.
#' Must be called as the **first action** in every `get_*()` function, before
#' any tidy-select resolution or estimation logic. This is the single canonical
#' source for these validation errors — never duplicate the checks inside
#' individual `get_*()` functions.
#'
#' Error classes raised (see `plans/error-messages.md`):
#' - `surveycore_error_na_rm_not_logical`
#' - `surveycore_error_invalid_variance_arg`
#' - `surveycore_error_invalid_conf_level`
#' - `surveycore_error_invalid_name_style`
#' - `surveycore_error_invalid_decimals`
#'
#' @param variance `NULL` or a character vector of variance type names.
#' @param conf_level Numeric scalar strictly between 0 and 1.
#' @param name_style `"surveycore"` (default) or `"broom"`.
#' @param decimals `NULL` or a non-negative whole number.
#' @param na.rm Logical(1). Must be exactly `TRUE` or `FALSE`.
#' @param valid_variance Character vector of accepted variance type names.
#'   Default covers all supported types: `"se"`, `"ci"`, `"var"`, `"cv"`,
#'   `"moe"`, `"deff"`.
#' @param call Caller environment for error attribution, passed to
#'   `cli::cli_abort()`.
#' @return `invisible(TRUE)` on success; errors otherwise.
#' @noRd
.validate_shared_args <- function(
  variance,
  conf_level,
  name_style,
  decimals = NULL,
  na.rm = TRUE,
  valid_variance = c("se", "ci", "var", "cv", "moe", "deff"),
  call = rlang::caller_env()
) {
  # Sync note: spec §V is authoritative — keep in sync if validation logic
  # changes.
  if (!isTRUE(na.rm) && !isFALSE(na.rm)) {
    cli::cli_abort(
      c(
        "x" = "{.arg na.rm} must be {.code TRUE} or {.code FALSE}.",
        "i" = "Got {.obj_type_friendly {na.rm}}."
      ),
      class = "surveycore_error_na_rm_not_logical",
      call = call
    )
  }
  if (!is.null(variance)) {
    bad_vals <- setdiff(variance, valid_variance)
    if (length(bad_vals) > 0L) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "{.arg variance} values must be from ",
            "{.or {.val {valid_variance}}}."
          ),
          "i" = "Unknown value{?s}: {.val {bad_vals}}."
        ),
        class = "surveycore_error_invalid_variance_arg",
        call = call
      )
    }
  }
  if (
    !is.numeric(conf_level) ||
      length(conf_level) != 1L ||
      conf_level <= 0 ||
      conf_level >= 1
  ) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg conf_level} must be a single number ",
          "strictly between 0 and 1."
        ),
        "i" = "Got {.val {conf_level}}."
      ),
      class = "surveycore_error_invalid_conf_level",
      call = call
    )
  }
  .validate_decimals_namestyle(decimals, name_style, call = call)
  invisible(TRUE)
}

# Validate `decimals` and `name_style` arguments.
#
# Shared between `.validate_shared_args()` (for the `get_*()` family) and
# `get_anova()` (which does not use `variance`, `conf_level`, or `na.rm`).
# Error classes `surveycore_error_invalid_decimals` and
# `surveycore_error_invalid_name_style` are reused from rows 45b and 46 of
# `plans/error-messages.md`.
#
# @param decimals Integer(1) or `NULL`.
# @param name_style Character(1) — one of `"surveycore"`, `"broom"`.
# @param call Caller environment for error attribution.
# @return `invisible(TRUE)` on success; errors otherwise.
# @keywords internal
# @noRd
.validate_decimals_namestyle <- function(
  decimals,
  name_style,
  call = rlang::caller_env()
) {
  if (!name_style %in% c("surveycore", "broom")) {
    cli::cli_abort(
      c(
        "x" = paste0(
          '{.arg name_style} must be {.val "surveycore"} ',
          'or {.val "broom"}.'
        ),
        "i" = "Got {.val {name_style}}."
      ),
      class = "surveycore_error_invalid_name_style",
      call = call
    )
  }
  if (!is.null(decimals)) {
    if (
      !is.numeric(decimals) ||
        length(decimals) != 1L ||
        decimals < 0 ||
        decimals != round(decimals)
    ) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "{.arg decimals} must be a non-negative whole number or ",
            "{.code NULL}."
          ),
          "i" = "Got {.val {decimals}}."
        ),
        class = "surveycore_error_invalid_decimals",
        call = call
      )
    }
  }
  invisible(TRUE)
}


#' Round numeric columns in a survey_result tibble
#'
#' Rounds all `double`-typed columns in a `survey_result` tibble to
#' `decimals` decimal places. Integer columns (e.g., `n`) and non-numeric
#' columns (group variables, character columns) are left unchanged. The `.meta`
#' attribute and S3 class are preserved across the rounding operation.
#'
#' @param result   A `survey_result` tibble.
#' @param decimals A non-negative whole number specifying decimal places.
#' @return The result tibble with all `double` columns rounded to `decimals`
#'   places.
#' @noRd
.apply_decimals <- function(result, decimals) {
  saved_meta <- attr(result, ".meta")
  saved_class <- class(result)
  for (i in seq_along(result)) {
    if (is.double(result[[i]])) {
      result[[i]] <- round(result[[i]], decimals)
    }
  }
  attr(result, ".meta") <- saved_meta
  class(result) <- saved_class
  result
}


#' Rename result columns for broom compatibility
#'
#' Renames columns in a `survey_result` tibble according to `name_style`.
#' No-op when `name_style = "surveycore"` (the default). Preserves the `.meta`
#' attribute and S3 class across the rename.
#'
#' Broom rename mapping (only columns that are present are renamed):
#' - `se` → `std.error`
#' - `ci_low` → `conf.low`
#' - `ci_high` → `conf.high`
#' - `p_value` → `p.value`
#' - `mean`, `total`, `pct`, `r`, `ratio`, `estimate` → `estimate`
#' - `df` → `parameter`
#' - `statistic` is unchanged
#'
#' @param result     A `survey_result` tibble.
#' @param name_style `"surveycore"` (no-op) or `"broom"`.
#' @param exclude    Character vector of column names to exclude from renaming,
#'   or `NULL`.
#' @return The (possibly renamed) result tibble with S3 class and `.meta`
#'   attribute preserved.
#' @noRd
.apply_name_style <- function(result, name_style, exclude = NULL) {
  if (name_style == "surveycore") {
    return(result)
  }

  broom_map <- c(
    se = "std.error",
    ci_low = "conf.low",
    ci_high = "conf.high",
    p_value = "p.value",
    mean = "estimate",
    total = "estimate",
    pct = "estimate",
    r = "estimate",
    ratio = "estimate",
    variance = "estimate",
    covariance = "estimate",
    estimate = "estimate",
    df = "parameter"
  )

  cols_present <- names(result)
  to_rename <- intersect(names(broom_map), cols_present)
  if (!is.null(exclude)) {
    to_rename <- setdiff(to_rename, exclude)
  }

  if (length(to_rename) > 0L) {
    saved_meta <- attr(result, ".meta")
    saved_class <- class(result)
    names(result)[match(to_rename, names(result))] <- broom_map[to_rename]
    attr(result, ".meta") <- saved_meta
    class(result) <- saved_class
  }

  result
}


#' Check that a design object is a supported survey class
#'
#' Throws `surveycore_error_unsupported_class` when `design` does not inherit
#' from `survey_base`. Must be called at the very start of every `get_*()`
#' and `survey_glm()` function, before any other validation or tidy-select
#' resolution.
#'
#' All four concrete design classes (`survey_taylor`, `survey_replicate`,
#' `survey_twophase`, `survey_nonprob`) and the abstract `survey_base` are
#' accepted. `survey_collection` is not accepted and is rejected by
#' upstream dispatch logic before `.check_unsupported_class()` is called.
#' Only objects that do not inherit from `survey_base` at all trigger the error.
#'
#' @param design  The first argument passed to a `get_*()` or fitting function.
#' @param fn_name Character(1). Calling function name, used in error messages.
#' @return `invisible(NULL)` on success.
#' @noRd
.check_unsupported_class <- function(design, fn_name) {
  if (!S7::S7_inherits(design, survey_base)) {
    cli::cli_abort(
      c(
        "x" = "{.fn {fn_name}} requires a survey design object.",
        "i" = "Got {.cls {class(design)[[1L]]}}."
      ),
      class = "surveycore_error_unsupported_class"
    )
  }
  invisible(NULL)
}


#' Pre-check that requested variables are present in `design@data`
#'
#' Called in each `get_*()` after tidy-select resolution but before any
#' data-frame subsetting. Raises `surveycore_error_variable_not_found` (C10)
#' when one or more resolved variable names are absent from
#' `names(design@data)`. Gives `.dispatch_over_collection()` a stable,
#' surveycore-owned error class to `tryCatch` on, so per-survey missing-variable
#' behavior can be governed by `.if_missing_var`.
#'
#' The pre-check covers every declared NSE argument (spec §III.4), not only
#' the focal `x`. The full set of args resolved into `var_names` for each
#' function is enumerated in spec §4.2.
#'
#' @param design    A `survey_base` object.
#' @param var_names Character vector of column names that must be present.
#'   Typically built from the tidy-select resolutions of every NSE arg the
#'   function declares. `NULL` entries are the caller's responsibility —
#'   drop them before passing.
#' @return `invisible(TRUE)` on success. Otherwise raises
#'   `surveycore_error_variable_not_found` with `parent = NULL` (see
#'   `plans/error-messages.md` row C10).
#' @noRd
.precheck_vars_present <- function(design, var_names) {
  if (length(var_names) == 0L) {
    return(invisible(TRUE))
  }
  missing <- var_names[!var_names %in% names(design@data)]
  if (length(missing) == 0L) {
    return(invisible(TRUE))
  }
  have <- names(design@data)
  cli::cli_abort(
    c(
      "x" = "Variable{?s} {.val {missing}} not found in survey data.",
      "i" = "Available: {.val {have}}."
    ),
    class = "surveycore_error_variable_not_found"
  )
}


#' Compute and add requested uncertainty columns to a result list
#'
#' Computes uncertainty measures from a vector of standard errors and returns
#' only the columns requested in `variance`. Called after the point estimate
#' and SE are computed in each `get_*()` function. Columns are returned in
#' this order when requested: `se`, `var`, `cv`, `ci_low`, `ci_high`, `moe`,
#' `deff`.
#'
#' @param se_vec       Numeric vector of standard errors. Length equals the
#'   number of estimation cells.
#' @param estimate_vec Numeric vector of point estimates. Same length as
#'   `se_vec`.
#' @param se_srs_vec   Numeric vector of SRS-equivalent standard errors, used
#'   to compute the design effect as `(se / se_srs)^2`. Pass `NULL` when
#'   `"deff"` is not in `variance`. For SRS designs, the calling function
#'   always passes `se_srs_vec = se_vec`.
#' @param conf_level   Numeric scalar in (0, 1). Confidence level for CIs and
#'   margin of error.
#' @param degf         Degrees of freedom for `stats::qt()`. Scalar or vector.
#'   Pass `Inf` for a normal approximation (matching `survey::svymean()`
#'   default).
#' @param variance     `NULL` or a character vector of requested variance types.
#'   Supported values: `"se"`, `"var"`, `"cv"`, `"ci"`, `"moe"`, `"deff"`.
#'   Returns `list()` when `NULL`.
#' @return Named list of numeric vectors. Only the columns named in `variance`
#'   are included.
#' @noRd
.add_variance_cols <- function(
  se_vec,
  estimate_vec,
  se_srs_vec = NULL,
  conf_level,
  degf,
  variance
) {
  if (is.null(variance)) {
    return(list())
  }

  out <- list()

  if ("se" %in% variance) {
    out$se <- se_vec
  }

  if ("var" %in% variance) {
    out$var <- se_vec^2
  }

  if ("cv" %in% variance) {
    cv <- se_vec / estimate_vec
    is_undef <- !is.na(estimate_vec) & estimate_vec <= 0
    n_undef <- sum(is_undef)
    if (n_undef > 0L) {
      cv[is_undef] <- NA_real_
      cli::cli_warn(
        c(
          "!" = paste0(
            '{.arg variance = "cv"} is undefined for {n_undef} ',
            "cell{?s} where the estimate is 0 or negative."
          ),
          "i" = "{.code cv} set to {.code NA} for those cells."
        ),
        class = "surveycore_warning_cv_undefined"
      )
    }
    out$cv <- cv
  }

  # Compute t_crit once — reused for both ci and moe
  if ("ci" %in% variance || "moe" %in% variance) {
    t_crit <- stats::qt((1 + conf_level) / 2, df = degf)
    ci_low <- estimate_vec - t_crit * se_vec
    ci_high <- estimate_vec + t_crit * se_vec
    if ("ci" %in% variance) {
      out$ci_low <- ci_low
      out$ci_high <- ci_high
    }
    if ("moe" %in% variance) {
      out$moe <- (ci_high - ci_low) / 2
    }
  }

  if ("deff" %in% variance) {
    if (is.null(se_srs_vec) || all(se_srs_vec == 0, na.rm = TRUE)) {
      out$deff <- rep(NA_real_, length(se_vec))
    } else {
      out$deff <- (se_vec / se_srs_vec)^2
    }
  }

  out
}


#' Compute design-based degrees of freedom
#'
#' Returns the design-based degrees of freedom as a numeric scalar. Used by
#' `survey_glm()` for t-distribution critical values in confidence intervals
#' and Wald tests. Phase 1 analysis functions (`get_means()`, `get_totals()`,
#' etc.) use `Inf` directly (normal approximation, matching
#' `survey::svymean()` defaults) and do **not** call `.degf()`.
#'
#' Design-specific formulas (matching `survey::degf()`):
#' - Taylor: `Σ_h(n_h - 1)` = total PSUs − number of strata.
#' - Replicate: `R - 1` where `R` is the number of replicate weight columns.
#' - Twophase: Phase-1 Taylor degrees of freedom.
#' - Non-probability (`survey_nonprob`): `Inf` — no design-based df.
#'
#' @param design A survey design object inheriting from `survey_base`.
#' @return Numeric(1). Design-based degrees of freedom, always ≥ 1.
#' @noRd
.degf <- function(design) {
  if (!S7::S7_inherits(design, survey_base)) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "Cannot compute degrees of freedom for ",
          "{.cls {class(design)[1L]}}."
        )
      ),
      class = "surveycore_error_unsupported_class"
    )
  }
  if (S7::S7_inherits(design, survey_taylor)) {
    max(1, .degf_taylor(design@data, design@variables))
  } else if (S7::S7_inherits(design, survey_replicate)) {
    rep_mat <- as.matrix(
      design@data[, design@variables$repweights, drop = FALSE]
    )
    max(1L, ncol(rep_mat) - 1L)
  } else if (S7::S7_inherits(design, survey_twophase)) {
    subset <- design@data[[design@variables$subset]]
    ph1_data <- design@data[subset, , drop = FALSE]
    max(1, .degf_taylor(ph1_data, design@variables$phase1))
  } else if (S7::S7_inherits(design, survey_nonprob)) {
    Inf
  } else {
    max(1L, nrow(design@data) - 1L)
  }
}


# ── .nonprob_rep_na_warn() ────────────────────────────────────────────────────
#
# Issue a warning when >5% of bootstrap replicates produce NA estimates for a
# survey_nonprob domain cell. Returns TRUE (sentinel) when all replicates are
# NA (caller must return NA-filled result). Returns NULL otherwise.
#
# @param design     A survey design object.
# @param na_frac    Fraction of replicates that are NA (0 to 1).
# @param na_dropped Integer count of NA replicates.
# @param R          Total number of replicates.
# @param scale      The scale factor used in the variance formula.
# @return NULL (no action), or TRUE (all-NA sentinel).
# @noRd
.nonprob_rep_na_warn <- function(design, na_frac, na_dropped, R, scale) {
  if (!S7::S7_inherits(design, survey_nonprob)) {
    return(NULL)
  }
  if (na_frac == 0) {
    return(NULL)
  }
  if (na_frac > 0.05) {
    cli::cli_warn(
      c(
        "!" = paste0(
          "{na_dropped} of {R} replicates have no observations in ",
          "this domain ({round(100 * na_frac, 1)}% of R)."
        ),
        "i" = paste0(
          "Standard errors for this cell understate variance because the ",
          "scale factor {.code {round(scale, 4)}} was computed for {R} ",
          "replicates but only {R - na_dropped} contribute."
        ),
        "i" = paste0(
          "Consider collapsing small domain categories or increasing R in ",
          "{.fn surveywts::create_bootstrap_weights}."
        )
      ),
      class = "surveycore_warning_domain_replicates_na"
    )
  }
  if (na_frac == 1.0) {
    return(TRUE) # sentinel: caller must return its own NA-filled result shape
  }
  NULL
}
