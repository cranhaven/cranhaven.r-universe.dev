# R/rowwise.R
#
# rowwise() for survey design objects.
#
# rowwise() sets @variables$rowwise = TRUE and stores optional id columns in
# @variables$rowwise_id_cols. These two keys are the only change to the object
# — @data, @groups, and @metadata are all unchanged.
#
# mutate() checks @variables$rowwise and routes to dplyr::rowwise(@data) in
# the rowwise branch, enabling row-by-row computation. See R/mutate.R.
#
# Rowwise mode is exited by:
#   - ungroup() (full): clears both @variables$rowwise and $rowwise_id_cols
#   - group_by(.add = FALSE): clears rowwise keys (default group_by)
#   - group_by(.add = TRUE): promotes id_cols to @groups, clears rowwise keys
#
# Predicates is_rowwise() and is_grouped() are exported from this file.
# group_vars.survey_base() is in R/group-by.R (alongside group_by/ungroup).
#
# Dispatch wiring: registered in .onLoad() via registerS3method().
# See R/zzz.R for the registration calls.

# ── rowwise() ─────────────────────────────────────────────────────────────────

#' Compute row-wise on a survey design object
#'
#' @description
#' `rowwise()` enables row-by-row computation in [mutate()]. Each row is
#' treated as an independent group, so expressions like
#' `mutate(d, row_max = max(dplyr::c_across(tidyselect::starts_with("y"))))` compute the maximum
#' across columns for each row independently.
#'
#' Use [ungroup()] or [group_by()] to exit rowwise mode.
#'
#' @details
#' ## Storage
#' Rowwise mode is stored in `@variables$rowwise` (logical `TRUE`) and
#' `@variables$rowwise_id_cols` (character vector of id column names).
#' `@groups` is **not** modified — rowwise mode is independent of grouping.
#'
#' ## Exiting rowwise mode
#' * `ungroup(d)` — exits rowwise mode and removes all groups.
#' * `group_by(d, ...)` — exits rowwise mode and sets new groups.
#' * `group_by(d, ..., .add = TRUE)` — promotes id columns to groups, then
#'   appends the new groups, then exits rowwise mode.
#'
#' ## mutate() behaviour
#' [mutate()] detects rowwise mode and routes internally through
#' `dplyr::rowwise(@data)` before calling `dplyr::mutate()`. The `rowwise_df`
#' class is stripped from `@data` after mutation so subsequent operations
#' are not accidentally rowwise.
#'
#' @param data A [`survey_base`][surveycore::survey_base] object.
#' @param ... <[`tidy-select`][tidyselect::language]> Optional id columns that
#'   identify each row (used with [dplyr::c_across()]). Commonly omitted.
#'
#' @return `data` with `@variables$rowwise = TRUE` and
#'   `@variables$rowwise_id_cols` set. All other properties are unchanged.
#'
#' @examples
#' # create a survey object from the bundled NPORS dataset
#' d <- surveycore::as_survey(
#'   surveycore::pew_npors_2025,
#'   weights = weight,
#'   strata = stratum
#' )
#'
#' # row-wise max across several columns
#' d |>
#'   rowwise() |>
#'   mutate(
#'     row_max = max(dplyr::c_across(tidyselect::starts_with("econ")), na.rm = TRUE)
#'   )
#'
#' # exit rowwise mode
#' d |>
#'   rowwise() |>
#'   ungroup()
#'
#' @family grouping
#' @name rowwise
NULL

#' @rdname rowwise
#' @method rowwise survey_base
rowwise.survey_base <- function(data, ...) {
  if (...length() == 0L) {
    id_cols <- character(0)
  } else {
    pos <- tidyselect::eval_select(rlang::expr(c(...)), data@data)
    id_cols <- names(pos)
  }

  data@variables$rowwise <- TRUE
  data@variables$rowwise_id_cols <- id_cols
  data
}


# ── is_rowwise() ──────────────────────────────────────────────────────────────

#' Test whether a survey design is in rowwise mode
#'
#' @description
#' Returns `TRUE` if the design was created (or passed through) `rowwise()`.
#' Use this predicate in estimation functions to detect and handle (or
#' disallow) rowwise mode.
#'
#' @param design A [`survey_base`][surveycore::survey_base] object.
#'
#' @return A scalar logical.
#'
#' @examples
#' # create a survey object from the bundled NPORS dataset
#' d <- surveycore::as_survey(
#'   surveycore::pew_npors_2025,
#'   weights = weight,
#'   strata = stratum
#' )
#'
#' # FALSE for a freshly-built design; TRUE after rowwise()
#' is_rowwise(d)
#' is_rowwise(rowwise(d))
#'
#' @family grouping
#' @export
is_rowwise <- function(design) {
  if (S7::S7_inherits(design, surveycore::survey_collection)) {
    return(
      length(design@surveys) > 0L &&
        all(vapply(design@surveys, is_rowwise, logical(1L)))
    )
  }
  isTRUE(design@variables$rowwise)
}


# ── is_grouped() ──────────────────────────────────────────────────────────────

#' Test whether a survey design has active grouping
#'
#' @description
#' Returns `TRUE` if the design has one or more grouping columns set via
#' [group_by()]. Returns `FALSE` for ungrouped or rowwise (but not grouped)
#' designs.
#'
#' @param design A [`survey_base`][surveycore::survey_base] object.
#'
#' @return A scalar logical.
#'
#' @examples
#' # create a survey object from the bundled NPORS dataset
#' d <- surveycore::as_survey(
#'   surveycore::pew_npors_2025,
#'   weights = weight,
#'   strata = stratum
#' )
#'
#' # only group_by() makes is_grouped() TRUE; rowwise() does not count
#' is_grouped(d)
#' is_grouped(group_by(d, gender))
#' is_grouped(rowwise(d))
#'
#' @family grouping
#' @export
is_grouped <- function(design) {
  length(design@groups) > 0L
}

# ── rowwise.survey_collection (PR 2b) ────────────────────────────────────────

#' @rdname rowwise
#' @method rowwise survey_collection
#' @inheritParams survey_collection_args
#'
#' @section Survey collections:
#' When applied to a `survey_collection`, `rowwise()` is dispatched to each
#' member independently — every member receives `@variables$rowwise = TRUE`
#' and the same `@variables$rowwise_id_cols`. The collection has no rowwise
#' marker; rowwise state lives entirely per-member. `@groups`, `@id`, and
#' `@if_missing_var` on the collection are unchanged.
#'
#' Construction-time uniformity is by-construction: every member is rowwise
#' after the call. Mixed rowwise state across members is detected later by
#' [mutate()] (see §IV.5 of the survey-collection spec) and warned about
#' rather than blocked.
rowwise.survey_collection <- function(
  data,
  ...,
  .if_missing_var = NULL
) {
  .dispatch_verb_over_collection(
    fn = dplyr::rowwise,
    verb_name = "rowwise",
    collection = data,
    ...,
    .if_missing_var = .if_missing_var,
    .detect_missing = "class_catch",
    .may_change_groups = FALSE
  )
}

# ── is_rowwise.survey_collection (PR 2c) ──
#
# Implemented as a class-check branch inside `is_rowwise()` itself (above)
# rather than an S3 method, because S7 namespaced class names break S3
# dispatch for surveytidy-owned generics and registerS3method-only methods
# trip the R CMD check "Apparent methods for exported generics not
# registered" note. The single-function form is also what spec §IV.10
# describes as a one-liner predicate.
# ── end ──
