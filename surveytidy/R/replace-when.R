# R/replace-when.R
#
# replace_when() — survey-aware in-place conditional replacement.
# Wraps dplyr::replace_when() and propagates .label, .value_labels, and
# .description into @metadata via mutate.survey_base(). Also inherits
# existing value labels from the input vector automatically.

#' Partially update a vector using conditional formulas
#'
#' @description
#' `replace_when()` is a survey-aware version of [dplyr::replace_when()] that
#' evaluates each formula case sequentially and replaces matching elements of
#' `x` with the corresponding RHS value. Elements where no case matches retain
#' their original value from `x`.
#'
#' Use `replace_when()` when partially updating an existing vector. When
#' creating an entirely new vector from conditions, [case_when()] is a better
#' choice.
#'
#' `replace_when()` automatically inherits value labels and the variable label
#' from `x`. Supply `.label` or `.value_labels` to override the inherited
#' values.
#'
#' When any of `.label`, `.value_labels`, or `.description` are supplied, or
#' when `x` carries existing labels, output label metadata is written to
#' `@metadata` after [mutate()]. When none apply, the output is identical to
#' [dplyr::replace_when()].
#'
#' @param x A vector to partially update.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> A sequence of two-sided
#'   formulas (`condition ~ value`). The left-hand side must be a logical
#'   vector the same size as `x`. The right-hand side provides the replacement
#'   value, cast to the type of `x`. Cases are evaluated sequentially; the
#'   first matching case is used. `NULL` inputs are ignored.
#' @param .label `character(1)` or `NULL`. Variable label stored in
#'   `@metadata@variable_labels` after [mutate()]. Overrides the label
#'   inherited from `x`.
#' @param .value_labels Named vector or `NULL`. Value labels stored in
#'   `@metadata@value_labels`. Names are the label strings; values are the
#'   data values. Merged with any existing labels inherited from `x`; entries
#'   in `.value_labels` take precedence over inherited entries with the same
#'   name.
#' @param .description `character(1)` or `NULL`. Plain-language description
#'   of how the variable was created. Stored in
#'   `@metadata@transformations[[col]]$description` after [mutate()].
#'
#' @return An updated version of `x` with the same type and size. If `x`
#'   carries labels or any surveytidy args are supplied, returns a
#'   `haven_labelled` vector; otherwise returns the same type as `x`.
#'
#' @seealso
#' * [dplyr::replace_when()] for the base implementation.
#' * [case_when()] to create an entirely new vector from conditions.
#' * [replace_values()] for in-place replacement using an explicit `from`/`to`
#'   mapping rather than conditions.
#'
#' @examples
#' library(surveycore)
#' library(surveytidy)
#'
#' # create the survey design
#' ns_wave1_svy <- as_survey_nonprob(ns_wave1, weights = weight)
#'
#' # basic replace_when — replace pid3 == 4 ("Something else") with 3
#' new <- ns_wave1_svy |>
#'   mutate(pid3_clean = replace_when(pid3, pid3 == 4 ~ 3)) |>
#'   select(pid3, pid3_clean)
#'
#' new
#'
#' # value labels from pid3 carry over to pid3_clean automatically
#' new@metadata@value_labels
#'
#' # override the inherited variable label via .label
#' new <- ns_wave1_svy |>
#'   mutate(
#'     pid3_clean = replace_when(
#'       pid3,
#'       pid3 == 4 ~ 3,
#'       .label = "Party ID (3 categories)"
#'     )
#'   ) |>
#'   select(pid3, pid3_clean)
#'
#' new@metadata@variable_labels
#'
#' # provide updated value labels reflecting the collapsed categories
#' new <- ns_wave1_svy |>
#'   mutate(
#'     pid3_clean = replace_when(
#'       pid3,
#'       pid3 == 4 ~ 3,
#'       .label = "Party ID (3 categories)",
#'       .value_labels = c(
#'         "Democrat" = 1,
#'         "Republican" = 2,
#'         "Independent/Other" = 3
#'       )
#'     )
#'   ) |>
#'   select(pid3, pid3_clean)
#'
#' new@metadata@value_labels
#'
#' # attach a plain-language description of the transformation
#' new <- ns_wave1_svy |>
#'   mutate(
#'     pid3_clean = replace_when(
#'       pid3,
#'       pid3 == 4 ~ 3,
#'       .label = "Party ID (3 categories)",
#'       .description = paste(
#'         "Recoded pid3: 'Something else' (4) merged into",
#'         "Independent (3)."
#'       )
#'     )
#'   ) |>
#'   select(pid3, pid3_clean)
#'
#' new@metadata@transformations
#'
#' @family recoding
#' @export
replace_when <- function(
  x,
  ...,
  .label = NULL,
  .value_labels = NULL,
  .description = NULL
) {
  var_name <- tryCatch(
    dplyr::cur_column(),
    error = function(e) rlang::as_label(rlang::enquo(x))
  )
  .validate_label_args(.label, .value_labels, .description)

  result <- dplyr::replace_when(x, ...)

  merged_labels <- .merge_value_labels(
    attr(x, "labels", exact = TRUE),
    .value_labels,
    result_values = unique(result)
  )
  effective_label <- if (!is.null(.label)) {
    .label
  } else {
    attr(x, "label", exact = TRUE)
  }

  if (!is.null(merged_labels) || !is.null(effective_label)) {
    return(.wrap_labelled(
      result,
      effective_label,
      merged_labels,
      .description,
      fn = "replace_when",
      var = var_name
    ))
  }

  if (!is.null(.description)) {
    attr(result, "surveytidy_recode") <- list(
      fn = "replace_when",
      var = var_name,
      description = .description
    )
  }

  result
}
