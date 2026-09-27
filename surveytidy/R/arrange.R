# R/arrange.R
#
# arrange() for survey design objects.
#
# arrange() sorts rows in @data. The domain column moves correctly with the
# rows — it is just another column. No update to @variables$domain quosures
# is needed (they are audit-only; the column is authoritative).
#
# Dispatch wiring: registered in .onLoad() via registerS3method().
# See R/zzz.R for the registration calls.
#
# Functions defined here:
#   arrange.survey_base()   — row sorting
#   arrange.survey_result() — class/meta preservation for survey_result

# ── arrange() ─────────────────────────────────────────────────────────────────

#' Order rows using column values
#'
#' @description
#' `arrange()` orders the rows of a [`survey_base`][surveycore::survey_base]
#' object by the values of selected columns.
#'
#' Unlike most other verbs, `arrange()` largely ignores grouping — use
#' `.by_group = TRUE` to sort by grouping variables first.
#'
#' @details
#' ## Missing values
#' Unlike base [sort()], `NA` values are always sorted to the end, even when
#' using [desc()].
#'
#' ## Domain column
#' The domain column moves with the rows — row reordering does not affect which
#' rows are in or out of the survey domain.
#'
#' @param .data A [`survey_base`][surveycore::survey_base] object, or a
#'   `survey_result` object returned by a surveycore estimation function.
#' @param ... <[`data-masking`][rlang::args_data_masking]> Variables, or
#'   functions of variables. Use [desc()] to sort a variable in descending
#'   order.
#' @param .by_group If `TRUE`, sorts first by the grouping variables set by
#'   [group_by()].
#' @param .locale The locale to use for ordering strings. If `NULL`, uses the
#'   `"C"` locale. See [stringi::locale()] for available locales.
#'
#' @return
#' An object of the same type as `.data` with the following properties:
#'
#' * All rows appear in the output, usually in a different position.
#' * Columns are not modified.
#' * Groups are not modified.
#' * Survey design attributes are preserved.
#'
#' @examples
#' library(surveytidy)
#' library(surveycore)
#'
#' # create a survey design from the pew_npors_2025 example dataset
#' d <- as_survey(pew_npors_2025, weights = weight, strata = stratum)
#'
#' # sort by age category ascending
#' arrange(d, agecat)
#'
#' # sort by age category descending
#' arrange(d, dplyr::desc(agecat))
#'
#' # sort by multiple variables
#' arrange(d, gender, dplyr::desc(agecat))
#'
#' # sort by grouping variables first
#' d_grouped <- group_by(d, gender)
#' arrange(d_grouped, .by_group = TRUE, agecat)
#'
#' @family single table verbs
#' @seealso [filter()] for domain-aware row marking,
#'   [slice()] for physical row selection
#' @name arrange
NULL

#' @rdname arrange
#' @method arrange survey_base
arrange.survey_base <- function(.data, ..., .by_group = FALSE, .locale = NULL) {
  # When .by_group = TRUE and @groups is non-empty, prepend the group columns
  # to the sort order. dplyr's native .by_group = TRUE would silently do
  # nothing because @data has no grouped_df attribute — groups are stored in
  # @groups on the survey object, not as a data frame attribute.
  if (isTRUE(.by_group) && length(.data@groups) > 0L) {
    new_data <- dplyr::arrange(
      .data@data,
      dplyr::across(dplyr::all_of(.data@groups)),
      ...,
      .locale = .locale
    )
  } else {
    new_data <- dplyr::arrange(
      .data@data,
      ...,
      .by_group = .by_group,
      .locale = .locale
    )
  }
  .data@data <- new_data
  .data
}

#' @rdname arrange
#' @method arrange survey_result
arrange.survey_result <- function(.data, ..., .by_group = FALSE) {
  old_class <- class(.data)
  old_meta <- attr(.data, ".meta")
  NextMethod() |> .restore_survey_result(old_class, old_meta)
}

#' @rdname arrange
#' @method arrange survey_collection
#' @inheritParams survey_collection_args
#'
#' @section Survey collections:
#' When applied to a `survey_collection`, `arrange()` is dispatched to each
#' member independently. Each member's rows are sorted in place; per-member
#' domain columns travel with the sorted rows. The output `survey_collection`
#' preserves the input's `@id`, `@if_missing_var`, and `@groups`. Use
#' `.if_missing_var` to override the collection's stored missing-variable
#' behavior for this call.
arrange.survey_collection <- function(
  .data,
  ...,
  .by_group = FALSE,
  .locale = NULL,
  .if_missing_var = NULL
) {
  .dispatch_verb_over_collection(
    fn = dplyr::arrange,
    verb_name = "arrange",
    collection = .data,
    ...,
    .by_group = .by_group,
    .locale = .locale,
    .if_missing_var = .if_missing_var,
    .detect_missing = "pre_check",
    .may_change_groups = FALSE
  )
}
