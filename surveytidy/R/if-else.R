# R/if-else.R
#
# if_else() — survey-aware vectorised if-else.
# Shadows dplyr::if_else() and propagates .label, .value_labels, and
# .description into @metadata via mutate.survey_base().

#' Vectorised if-else
#'
#' @description
#' `if_else()` is a survey-aware version of [dplyr::if_else()] that applies a
#' binary condition element-wise: `true` values are used where `condition` is
#' `TRUE`, `false` values where it is `FALSE`, and `missing` values where it
#' is `NA`.
#'
#' Compared to base [ifelse()], this function is stricter about types:
#' `true`, `false`, and `missing` must be compatible and will be cast to
#' their common type.
#'
#' When any of `.label`, `.value_labels`, or `.description` are supplied,
#' output label metadata is written to `@metadata` after [mutate()]. When none
#' of these arguments are used, the output is identical to [dplyr::if_else()].
#'
#' For more than two conditions, see [case_when()].
#'
#' @param condition A logical vector.
#' @param true,false Vectors to use for `TRUE` and `FALSE` values of
#'   `condition`. Both are recycled to the size of `condition` and cast to
#'   their common type.
#' @param missing If not `NULL`, used as the value for `NA` values of
#'   `condition`. Follows the same size and type rules as `true` and `false`.
#' @param ... These dots are for future extensions and must be empty.
#' @param ptype An optional prototype declaring the desired output type.
#'   Overrides the common type of `true`, `false`, and `missing`.
#' @param .label `character(1)` or `NULL`. Variable label stored in
#'   `@metadata@variable_labels` after [mutate()].
#' @param .value_labels Named vector or `NULL`. Value labels stored in
#'   `@metadata@value_labels`. Names are the label strings; values are the
#'   data values.
#' @param .description `character(1)` or `NULL`. Plain-language description
#'   of how the variable was created. Stored in
#'   `@metadata@transformations[[col]]$description` after [mutate()].
#'
#' @return A vector the same size as `condition` and the common type of
#'   `true`, `false`, and `missing`. If `.label` or `.value_labels` are
#'   supplied, returns a `haven_labelled` vector; otherwise returns the same
#'   type as the common type of the inputs.
#'
#' @seealso
#' * [dplyr::if_else()] for the base implementation.
#' * [case_when()] for more than two conditions.
#' * [na_if()] to replace specific values with `NA`.
#'
#' @examples
#' library(surveycore)
#' library(surveytidy)
#'
#' # create the survey design
#' ns_wave1_svy <- as_survey_nonprob(ns_wave1, weights = weight)
#'
#' # basic if_else — identical to dplyr::if_else()
#' new <- ns_wave1_svy |>
#'   mutate(senior = if_else(age >= 65, "Senior (65+)", "Non-senior")) |>
#'   select(age, senior)
#'
#' # by default, no metadata is attached
#' new
#' new@metadata
#'
#' # use missing = to specify the output value when condition is NA
#' new <- ns_wave1_svy |>
#'   mutate(
#'     dem = if_else(
#'       pid3 == 1,
#'       "Democrat",
#'       "Non-Democrat",
#'       missing = "Unknown"
#'     )
#'   ) |>
#'   select(pid3, dem)
#'
#' new
#'
#' # attach a variable label via .label
#' new <- ns_wave1_svy |>
#'   mutate(
#'     senior = if_else(
#'       age >= 65,
#'       "Senior (65+)",
#'       "Non-senior",
#'       .label = "Senior citizen (age 65+)"
#'     )
#'   ) |>
#'   select(age, senior)
#'
#' new@metadata@variable_labels
#'
#' # use integer codes and document them with value labels
#' new <- ns_wave1_svy |>
#'   mutate(
#'     senior = if_else(
#'       age >= 65,
#'       true = 1L,
#'       false = 0L,
#'       .label = "Senior citizen (age 65+)",
#'       .value_labels = c("Senior (65+)" = 1, "Non-senior" = 0)
#'     )
#'   ) |>
#'   select(age, senior)
#'
#' new@metadata@value_labels
#'
#' # attach a plain-language description of the transformation
#' new <- ns_wave1_svy |>
#'   mutate(
#'     senior = if_else(
#'       age >= 65,
#'       "Senior (65+)",
#'       "Non-senior",
#'       .label = "Senior citizen (age 65+)",
#'       .description = paste(
#'         "age >= 65 coded as 'Senior (65+)';",
#'         "everyone else as 'Non-senior'."
#'       )
#'     )
#'   ) |>
#'   select(age, senior)
#'
#' new@metadata@transformations
#'
#' @family recoding
#' @export
if_else <- function(
  condition,
  true,
  false,
  missing = NULL,
  ...,
  ptype = NULL,
  .label = NULL,
  .value_labels = NULL,
  .description = NULL
) {
  .validate_label_args(.label, .value_labels, .description)

  result <- dplyr::if_else(
    condition,
    true,
    false,
    missing = missing,
    ...,
    ptype = ptype
  )

  if (!is.null(.label) || !is.null(.value_labels)) {
    return(.wrap_labelled(
      result,
      .label,
      .value_labels,
      .description,
      fn = "if_else",
      var = NULL
    ))
  }

  if (!is.null(.description)) {
    attr(result, "surveytidy_recode") <- list(
      fn = "if_else",
      var = NULL,
      description = .description
    )
  }

  result
}
