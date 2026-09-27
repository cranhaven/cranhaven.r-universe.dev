# R/select.R
#
# Column-selection verbs for survey design objects.
#
# select() physically removes non-selected, non-design columns from @data and
# records the user's visible selection in @variables$visible_vars. Design
# variables are always preserved — they are required for variance estimation.
#
# relocate() reorders visible_vars when it is set; reorders @data when NULL.
#
# pull() and glimpse() are thin wrappers around dplyr internals.
#
# Dispatch wiring: registered in .onLoad() via registerS3method().
# See R/zzz.R for the registration calls.
#
# Functions defined here:
#   select.survey_base()   — column selection with design-var preservation
#   select.survey_result() — column selection with .meta pruning
#   relocate.survey_base() — column reordering (via visible_vars or @data)
#   pull.survey_base()     — extract a column as a vector
#   glimpse.survey_base()  — print a concise column summary

# ── select() ──────────────────────────────────────────────────────────────────

#' Keep or drop columns using their names and types
#'
#' @description
#' `select()` keeps the named columns and drops all others, using the
#' [tidyselect mini-language][tidyselect::language] to describe column sets.
#' Design variables (weights, strata, PSU, FPC, replicate weights) are
#' **always retained** even when not explicitly selected — they are required
#' for variance estimation. After `select()`, `print()` shows only the columns
#' you selected; design variables remain in the object but are hidden from
#' display.
#'
#' `select()` is irreversible: dropped columns are permanently removed from
#' the survey object and cannot be recovered within the same pipeline.
#'
#' @details
#' ## Design variable preservation
#' Regardless of what you select, the following are always kept in the
#' survey object: weights, strata, PSUs, FPC columns, replicate weights,
#' and the domain column (if set by [filter()]). They are hidden from
#' `print()` output but remain available for variance estimation.
#'
#' ## Metadata
#' Variable labels, value labels, and other metadata for dropped columns
#' are removed. Metadata for retained columns is preserved.
#'
#' @param .data A [`survey_base`][surveycore::survey_base] object, or a
#'   `survey_result` object returned by a surveycore estimation function.
#' @param ... <[`tidy-select`][tidyselect::language]> One or more unquoted
#'   column names or tidy-select expressions.
#'
#' @return
#' An object of the same type as `.data` with the following properties:
#'
#' * Rows are not modified.
#' * Non-selected, non-design columns are permanently removed.
#' * Design variables are always retained.
#' * Survey design attributes are preserved.
#'
#' @examples
#' library(surveytidy)
#' library(surveycore)
#'
#' # create a survey design from the pew_npors_2025 example dataset
#' d <- as_survey(pew_npors_2025, weights = weight, strata = stratum)
#'
#' # select by name
#' select(d, gender, agecat)
#'
#' # select by name pattern
#' select(d, tidyselect::starts_with("smuse_"))
#'
#' # select by type
#' select(d, tidyselect::where(is.numeric))
#'
#' # drop columns with !
#' select(d, !tidyselect::starts_with("smuse_"))
#'
#' @family selecting
#' @seealso [relocate()] to reorder columns, [rename()] to rename them,
#'   [mutate()] to add new ones
#' @name select
NULL

#' @rdname select
#' @method select survey_base
select.survey_base <- function(.data, ...) {
  # Step 1: resolve the user's column selection
  user_pos <- tidyselect::eval_select(rlang::expr(c(...)), .data@data)
  user_cols <- names(user_pos)

  # Step 2: protected columns that are already present in @data
  # (intersect guards against domain col not yet existing before first filter())
  protected <- intersect(.protected_cols(.data), names(.data@data))

  # Step 3: visible columns = the user's explicit selection only
  # (design variables are preserved in @data but are hidden from print unless
  # the user explicitly named them)
  visible <- user_cols

  # Step 4: final data columns — user's order first, then any protected cols
  # that were not in the user's selection appended at the end
  final_cols <- union(user_cols, protected)

  # Step 5: columns to physically remove (never in final_cols)
  dropped <- setdiff(names(.data@data), final_cols)

  # Step 6: update @data
  .data@data <- .data@data[, final_cols, drop = FALSE]

  # Step 7: delete @metadata entries for physically removed columns
  # surveycore:::.delete_metadata_col() does not exist; delete manually.
  for (col in dropped) {
    .data@metadata@variable_labels[[col]] <- NULL
    .data@metadata@value_labels[[col]] <- NULL
    .data@metadata@question_prefaces[[col]] <- NULL
    .data@metadata@notes[[col]] <- NULL
    .data@metadata@transformations[[col]] <- NULL
  }

  # Step 8: normalise visible_vars
  # NULL means "show everything in @data" — use NULL when the selection
  # is empty OR when the user selected every column in @data (e.g. everything())
  .data@variables$visible_vars <- if (
    length(visible) == 0L || setequal(visible, final_cols)
  ) {
    NULL
  } else {
    visible
  }

  .data
}

#' @rdname select
#' @method select survey_result
select.survey_result <- function(.data, ...) {
  tbl <- tibble::as_tibble(.data)
  old_class <- class(.data)

  # Step 1: Resolve selection → named integer: output_name → column position
  selected_cols <- tidyselect::eval_select(rlang::expr(c(...)), tbl)

  # Step 2: Extract original and output column names
  original_names <- names(tbl)[unname(selected_cols)]
  output_names <- names(selected_cols)

  # Step 3: Detect and apply any inline renames (e.g., select(r, grp = group))
  rename_mask <- original_names != output_names
  if (any(rename_mask)) {
    rename_map <- stats::setNames(
      output_names[rename_mask],
      original_names[rename_mask]
    )
    .data <- .apply_result_rename_map(.data, rename_map)
  }

  # Step 4: Subset to selected columns
  result <- .data[, output_names, drop = FALSE]

  # Step 5: Prune meta for dropped columns ($group keys only — $x keys are
  # input variable names that don't correspond to output columns for most
  # result types, so pruning by column presence would be incorrect)
  new_meta <- .prune_result_meta(attr(.data, ".meta"), output_names)

  # Step 6: Assign and restore
  attr(result, ".meta") <- new_meta
  class(result) <- old_class
  result
}


# ── relocate() ────────────────────────────────────────────────────────────────

#' Change column order in a survey design object
#'
#' @description
#' `relocate()` moves columns to a new position using the same
#' [tidyselect mini-language][tidyselect::language] as [select()]. Design
#' variables (weights, strata, PSUs) are not moved — only analysis columns
#' change position.
#'
#' @details
#' ## Design variable positions
#' Design variables are always preserved at their current position in the
#' underlying data. When you call `relocate()`, only non-design columns are
#' affected by the reordering.
#'
#' ## After `select()`
#' When [select()] has been called, `relocate()` reorders the visible columns
#' (those shown when you print the object). This has no effect on the physical
#' column order in the underlying data.
#'
#' @param .data A [`survey_base`][surveycore::survey_base] object.
#' @param ... <[`tidy-select`][tidyselect::language]> Columns to move.
#' @param .before,.after <[`tidy-select`][tidyselect::language]> A destination
#'   column. Columns in `...` are placed immediately before or after it.
#'   Specify at most one of `.before` and `.after`.
#'
#' @return
#' An object of the same type as `.data` with the following properties:
#'
#' * Rows are not modified.
#' * All columns are present; only their order changes.
#' * Design variables are not moved.
#' * Groups and survey design attributes are preserved.
#'
#' @examples
#' library(surveytidy)
#' library(surveycore)
#'
#' # create a survey design from the pew_npors_2025 example dataset
#' d <- as_survey(pew_npors_2025, weights = weight, strata = stratum)
#'
#' # move agecat before gender
#' relocate(d, agecat, .before = gender)
#'
#' # move all social media columns to the front
#' relocate(d, tidyselect::starts_with("smuse_"))
#'
#' # after select(), relocate reorders the visible columns
#' d |>
#'   select(gender, agecat, partysum) |>
#'   relocate(partysum, .before = gender)
#'
#' @family selecting
#' @seealso [select()] to keep or drop columns, [rename()] to rename them
#' @name relocate
NULL

#' @rdname relocate
#' @method relocate survey_base
relocate.survey_base <- function(.data, ..., .before = NULL, .after = NULL) {
  # Capture .before and .after as quosures to avoid evaluating NSE expressions
  # (e.g. .before = y1) in the wrong environment. rlang::inject() then inlines
  # the expression into the dplyr::relocate() call where tidyselect can
  # evaluate it against the data frame.
  # Also: dplyr 1.2.0 errors when BOTH .before and .after are explicit (even
  # NULL), so only pass the ones that are actually provided.
  before_quo <- rlang::enquo(.before)
  after_quo <- rlang::enquo(.after)
  has_before <- !rlang::quo_is_null(before_quo)
  has_after <- !rlang::quo_is_null(after_quo)

  .do_relocate <- function(df) {
    if (has_before && has_after) {
      rlang::inject(
        dplyr::relocate(df, ..., .before = !!before_quo, .after = !!after_quo)
      )
    } else if (has_before) {
      rlang::inject(dplyr::relocate(df, ..., .before = !!before_quo))
    } else if (has_after) {
      rlang::inject(dplyr::relocate(df, ..., .after = !!after_quo))
    } else {
      dplyr::relocate(df, ...)
    }
  }

  if (!is.null(.data@variables$visible_vars)) {
    # When visible_vars is set, relocate applies to the visible columns only.
    # @data column order has no display meaning — only visible_vars does.
    vv_df <- .data@data[, .data@variables$visible_vars, drop = FALSE]
    ordered <- .do_relocate(vv_df)
    .data@variables$visible_vars <- names(ordered)
  } else {
    # No visible_vars: reorder @data directly
    .data@data <- .do_relocate(.data@data)
  }
  .data
}


# ── pull() ────────────────────────────────────────────────────────────────────

# pull() is a terminal operation — the result is a plain vector, not a survey
# object. No invariant checks, @groups, or @metadata considerations apply.

#' Extract a column from a survey design object
#'
#' @description
#' Pull a single column out of a survey design object as a plain vector.
#' This is a terminal operation — the result is not a survey object and cannot
#' be piped back into survey verbs.
#'
#' @param .data A [`survey_base`][surveycore::survey_base] object.
#' @param var <[`data-masking`][rlang::args_data_masking]> The column to
#'   extract. Accepts a bare name, a positive integer (counting from the
#'   left), or a negative integer (counting from the right). Defaults to the
#'   last column.
#' @param name <[`data-masking`][rlang::args_data_masking]> An optional column
#'   whose values are used as names for the returned vector.
#' @param ... Passed to `dplyr::pull()`.
#'
#' @return A vector the same length as the number of rows in `.data`.
#'
#' @examples
#' library(surveytidy)
#' library(surveycore)
#'
#' # create a survey design from the pew_npors_2025 example dataset
#' d <- as_survey(pew_npors_2025, weights = weight, strata = stratum)
#'
#' # extract a column by name
#' pull(d, agecat)
#'
#' # named vector — values of agecat named by respid
#' pull(d, agecat, name = respid)
#'
#' @family selecting
#' @seealso [select()] to keep columns in the survey object
#' @name pull
NULL

#' @rdname pull
#' @method pull survey_base
pull.survey_base <- function(.data, var = -1, name = NULL, ...) {
  dplyr::pull(.data@data, var = {{ var }}, name = {{ name }}, ...)
}


# ── glimpse() ─────────────────────────────────────────────────────────────────

#' Get a glimpse of a survey design object
#'
#' @description
#' Print a transposed summary of the survey object's columns — column names
#' run down the left, data types and values run across. Respects [select()]:
#' if columns have been selected, only those columns are shown; design
#' variables are hidden from the display.
#'
#' @param x A [`survey_base`][surveycore::survey_base] object.
#' @param width Width of the output. Defaults to the console width.
#' @param ... Passed to `dplyr::glimpse()`.
#'
#' @return `x` invisibly.
#'
#' @examples
#' library(surveytidy)
#' library(surveycore)
#'
#' # create a survey design from the pew_npors_2025 example dataset
#' d <- as_survey(pew_npors_2025, weights = weight, strata = stratum)
#'
#' # glimpse all columns
#' glimpse(d)
#'
#' # after select(), shows only the selected columns
#' d |>
#'   select(gender, agecat, partysum) |>
#'   glimpse()
#'
#' @family selecting
#' @seealso [select()] to control which columns are visible
#' @name glimpse
NULL

#' @rdname glimpse
#' @method glimpse survey_base
glimpse.survey_base <- function(x, width = NULL, ...) {
  if (!is.null(x@variables$visible_vars)) {
    dplyr::glimpse(x@data[, x@variables$visible_vars, drop = FALSE], width, ...)
  } else {
    dplyr::glimpse(x@data, width, ...)
  }
  invisible(x)
}


# ── select.survey_collection / relocate.survey_collection (PR 2b) ─────────────

# Pre-flight helper: resolve the user's tidyselect against the first member's
# @data and raise typed surveytidy_error_collection_select_group_removed if
# the selection would remove any column listed in coll@groups.
#
# Spec §IV.3: per V2 each member resolves its own tidyselect; the first
# member's resolution is sufficient for the group-column safety check
# because G1b guarantees every group column exists in every member's @data.
# If tidyselect cannot resolve against the first member (e.g., all_of() of a
# column missing on m1), the pre-flight silently defers to per-member
# dispatch where class-catch + .if_missing_var govern.
.check_select_group_removal <- function(coll, ...) {
  if (length(coll@groups) == 0L) {
    return(invisible(NULL))
  }
  first_data <- coll@surveys[[1L]]@data
  resolved <- tryCatch(
    tidyselect::eval_select(rlang::expr(c(...)), first_data),
    error = function(e) NULL
  )
  if (is.null(resolved)) {
    return(invisible(NULL))
  }
  selected_cols <- names(resolved)
  removed <- setdiff(coll@groups, selected_cols)
  if (length(removed) > 0L) {
    n_removed <- length(removed)
    cli::cli_abort(
      c(
        "x" = "{cli::qty(n_removed)}{.fn select} would remove group column{?s} {.field {removed}} from the {.cls survey_collection}.",
        "i" = "Group columns must remain in every member's {.code @data}; the surveycore class validator (G1b) requires this.",
        "v" = "{cli::qty(n_removed)}Include the group column{?s} in the selection, or call {.fn ungroup} on the collection first."
      ),
      class = "surveytidy_error_collection_select_group_removed"
    )
  }
  invisible(NULL)
}

#' @rdname select
#' @method select survey_collection
#' @inheritParams survey_collection_args
#'
#' @section Survey collections:
#' When applied to a `survey_collection`, `select()` is dispatched to each
#' member independently. Each member resolves its own tidyselect expression
#' against its own `@data`, so members may end up with different visible
#' columns when the selection is partial (e.g., `any_of()` against a
#' heterogeneous collection). Per-member design variables are always retained.
#'
#' Before dispatching, `select.survey_collection` resolves the selection
#' against the first member's `@data` and raises
#' `surveytidy_error_collection_select_group_removed` if any column in
#' `coll@groups` would be removed. Group columns must remain in every member;
#' silently dropping them would violate the surveycore class validator (G1b).
#' Use [ungroup()] first if you intend to remove a group column.
#'
#' `relocate.survey_collection` is **not** subject to this pre-flight —
#' `dplyr::relocate` only reorders columns and never drops them.
select.survey_collection <- function(.data, ..., .if_missing_var = NULL) {
  .check_select_group_removal(.data, ...)
  .dispatch_verb_over_collection(
    fn = dplyr::select,
    verb_name = "select",
    collection = .data,
    ...,
    .if_missing_var = .if_missing_var,
    .detect_missing = "class_catch",
    .may_change_groups = FALSE
  )
}

#' @rdname relocate
#' @method relocate survey_collection
#' @inheritParams survey_collection_args
#'
#' @section Survey collections:
#' When applied to a `survey_collection`, `relocate()` is dispatched to each
#' member independently. Each member's `relocate.survey_base` reorders
#' columns according to the user's tidyselect (and `.before`/`.after`),
#' preserving design variables and `@groups`. Negative tidyselect like
#' `relocate(coll, -group, .before = wt)` is permitted because `relocate`
#' only reorders — it never removes columns. The `select` group-removal
#' pre-flight does not apply.
relocate.survey_collection <- function(
  .data,
  ...,
  .before = NULL,
  .after = NULL,
  .if_missing_var = NULL
) {
  # Capture .before / .after as quosures so the per-member dplyr::relocate
  # call can resolve them against each member's @data via tidyselect. The
  # dispatcher's .unwrap_scalar_dots eval_tidy's dotted-name args, which
  # would force a bare-name reference like `.before = wt` in the wrong
  # environment. Forwarding the quosures through a closure with rlang::inject
  # avoids that and matches the relocate.survey_base pattern.
  before_quo <- rlang::enquo(.before)
  after_quo <- rlang::enquo(.after)
  has_before <- !rlang::quo_is_null(before_quo)
  has_after <- !rlang::quo_is_null(after_quo)

  fn <- if (has_before && has_after) {
    function(survey, ...) {
      rlang::inject(dplyr::relocate(
        survey,
        ...,
        .before = !!before_quo,
        .after = !!after_quo
      ))
    }
  } else if (has_before) {
    function(survey, ...) {
      rlang::inject(dplyr::relocate(survey, ..., .before = !!before_quo))
    }
  } else if (has_after) {
    function(survey, ...) {
      rlang::inject(dplyr::relocate(survey, ..., .after = !!after_quo))
    }
  } else {
    function(survey, ...) dplyr::relocate(survey, ...)
  }

  .dispatch_verb_over_collection(
    fn = fn,
    verb_name = "relocate",
    collection = .data,
    ...,
    .if_missing_var = .if_missing_var,
    .detect_missing = "class_catch",
    .may_change_groups = FALSE
  )
}
