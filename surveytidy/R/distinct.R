# R/distinct.R
#
# distinct.survey_base() — physical row deduplication for survey design objects.
#
# distinct() physically removes duplicate rows while always retaining all
# columns in @data (including design variables). Unlike dplyr::distinct()
# which by default drops non-selected columns, this implementation always
# uses .keep_all = TRUE internally — design variables must never be lost.
#
# The .keep_all = FALSE user argument is silently ignored — this is
# intentional and documented in the spec (spec §III.3).
#
# Dispatch wiring: registered in .onLoad() via registerS3method().
# See R/zzz.R for the registration calls.
#
# Functions defined here:
#   distinct.survey_base() — row deduplication with physical-subset warning

#' Remove duplicate rows from a survey design object
#'
#' @description
#' `distinct()` **physically removes duplicate rows** from a survey design
#' object, always issuing `surveycore_warning_physical_subset`. Unlike
#' [dplyr::distinct()], all columns in `@data` are retained regardless of
#' which columns are specified in `...` — design variables must never be
#' lost from the survey object.
#'
#' For subpopulation analyses, use [filter()] instead — it marks rows
#' out-of-domain without removing them, preserving valid variance estimation.
#'
#' @details
#' ## Column retention
#' `distinct()` always behaves as if `.keep_all = TRUE`. Specifying columns
#' in `...` controls which columns determine uniqueness — it does **not**
#' control which columns appear in the result. This is a deliberate
#' divergence from `dplyr::distinct(df, x, y)` which by default drops all
#' columns except `x` and `y`.
#'
#' ## Default deduplication (empty `...`)
#' When `...` is empty, deduplication uses all non-design columns. Design
#' variables (strata, PSU, weights, FPC) are excluded from the uniqueness
#' check — deduplicating on them would produce meaningless or
#' survey-corrupting results.
#'
#' ## Design variable warning
#' If `...` includes a design variable, `surveytidy_warning_distinct_design_var`
#' is issued before the operation. The operation still proceeds after the
#' warning — the user is assumed to know what they are doing.
#'
#' @param .data A [`survey_base`][surveycore::survey_base] object.
#' @param ... <[`data-masking`][rlang::args_data_masking]> Optional columns
#'   used to determine uniqueness. If empty, all non-design columns are used.
#'   Note: `.keep_all` is always `TRUE` regardless of what is specified here.
#' @param .keep_all Accepted for interface compatibility; **has no effect**.
#'   The survey implementation always retains all columns in `@data`.
#'
#' @return
#' An object of the same class as `.data` with the following properties:
#'
#' * Rows physically reduced to distinct subset (fewer rows possible).
#' * All columns in `@data` are retained (`.keep_all = TRUE` always).
#' * `@variables$visible_vars` is unchanged — distinct is a pure row operation.
#' * `@metadata` is unchanged.
#' * `@groups` is unchanged.
#' * Always issues `surveycore_warning_physical_subset`.
#'
#' @examples
#' library(surveytidy)
#' library(surveycore)
#'
#' # create a survey design from the pew_npors_2025 example dataset
#' d <- as_survey(pew_npors_2025, weights = weight, strata = stratum)
#'
#' # deduplicate on all non-design columns (issues physical-subset warning)
#' distinct(d)
#'
#' # deduplicate by one column (all other columns still retained)
#' distinct(d, cregion)
#'
#' @family row operations
#' @seealso [filter()] for domain-aware row marking (preferred for
#'   subpopulation analyses)
#' @name distinct
NULL

#' @rdname distinct
#' @method distinct survey_base
distinct.survey_base <- function(.data, ..., .keep_all = FALSE) {
  # Step 1: Always warn — distinct() physically removes rows.
  .warn_physical_subset("distinct")

  has_dots <- ...length() > 0L

  if (has_dots) {
    # Step 2: Resolve ... to column names and check for design variables.
    selected <- tidyselect::eval_select(rlang::expr(c(...)), .data@data)
    selected_names <- names(selected)
    protected <- .protected_cols(.data)
    design_in_dots <- intersect(selected_names, protected)

    if (length(design_in_dots) > 0L) {
      cli::cli_warn(
        c(
          "!" = "Deduplicating by design variable{?s} {.field {design_in_dots}} may corrupt variance estimation.",
          "i" = "Design variables define the sampling structure; removing rows that share design variable values can invalidate standard error calculations.",
          "i" = "Use {.code distinct(d)} without specifying design variables, or use {.fn subset} if physical row removal is intentional."
        ),
        class = "surveytidy_warning_distinct_design_var"
      )
    }

    # Step 3: Deduplicate by specified columns, retaining all columns.
    new_data <- dplyr::distinct(.data@data, ..., .keep_all = TRUE)
  } else {
    # Step 3 (empty ...): Deduplicate on non-design columns only.
    # This is the survey-safe default — design variables define sampling
    # structure and deduplicating on them would be meaningless or harmful.
    non_design <- setdiff(names(.data@data), .protected_cols(.data))
    new_data <- dplyr::distinct(
      .data@data,
      dplyr::across(dplyr::all_of(non_design)),
      .keep_all = TRUE
    )
  }

  # Step 4: Assign updated @data. @groups and @metadata propagate unchanged.
  .data@data <- new_data
  .data
}


# ── distinct.survey_collection (PR 2b) ────────────────────────────────────────

#' @rdname distinct
#' @method distinct survey_collection
#' @inheritParams survey_collection_args
#'
#' @section Survey collections:
#' When applied to a `survey_collection`, `distinct()` is dispatched to each
#' member independently — there is no cross-survey deduplication. Two
#' members that share a literally identical row will both retain that row
#' in their post-`distinct()` results. This is the V9 contract from the
#' survey-collection spec; collections deliberately avoid the
#' `bind_rows()` analogy here because cross-survey deduplication has no
#' coherent variance interpretation across designs.
#'
#' Each member's `distinct.survey_base` issues
#' `surveycore_warning_physical_subset` independently — N firings on an
#' N-member collection. Capture with `withCallingHandlers()`.
distinct.survey_collection <- function(
  .data,
  ...,
  .keep_all = FALSE,
  .if_missing_var = NULL
) {
  .dispatch_verb_over_collection(
    fn = dplyr::distinct,
    verb_name = "distinct",
    collection = .data,
    ...,
    .keep_all = .keep_all,
    .if_missing_var = .if_missing_var,
    .detect_missing = "class_catch",
    .may_change_groups = FALSE
  )
}
