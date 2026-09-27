# R/replace-values.R
#
# replace_values() — survey-aware partial value replacement.
# Own implementation matching the dplyr 1.2.0 API; propagates .label,
# .value_labels, and .description into @metadata via mutate.survey_base().
# Also inherits existing value labels from the input vector automatically.

#' Partially update values using an explicit mapping
#'
#' @description
#' `replace_values()` replaces each value of `x` found in `from` with the
#' corresponding value from `to`. Values not found in `from` retain their
#' original value unchanged.
#'
#' Use `replace_values()` when updating only specific values of an existing
#' variable. When remapping the full range of values in `x`, [recode_values()]
#' is a better choice.
#'
#' `replace_values()` automatically inherits value labels and the variable
#' label from `x`. Supply `.label` or `.value_labels` to override the inherited
#' values.
#'
#' When any of `.label`, `.value_labels`, or `.description` are supplied, or
#' when `x` carries existing labels, output label metadata is written to
#' `@metadata` after [mutate()]. When none apply, the output is the same type
#' as `x`.
#'
#' @param x Vector to partially update.
#' @param ... These dots are for future extensions and must be empty.
#' @param from Vector of old values to replace. Must be the same type as `x`.
#' @param to Vector of new values corresponding to `from`. Must be the same
#'   length as `from`.
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
#' * `dplyr::replace_values()` for the base implementation.
#' * [recode_values()] for full value remapping with explicit `from`/`to`
#'   vectors; does not inherit labels from `x` automatically.
#' * [replace_when()] for condition-based partial replacement.
#' * [na_if()] to replace specific values with `NA`.
#'
#' @examples
#' library(surveycore)
#' library(surveytidy)
#'
#' # create the survey design
#' ns_wave1_svy <- as_survey_nonprob(ns_wave1, weights = weight)
#'
#' # basic replace_values — replace pid3 == 4 ("Something else") with 3
#' new <- ns_wave1_svy |>
#'   mutate(pid3_clean = replace_values(pid3, from = 4, to = 3)) |>
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
#'     pid3_clean = replace_values(
#'       pid3,
#'       from = 4,
#'       to = 3,
#'       .label = "Party ID (3 categories)"
#'     )
#'   ) |>
#'   select(pid3, pid3_clean)
#'
#' new@metadata@variable_labels
#'
#' # provide updated value labels that reflect the recoded categories
#' new <- ns_wave1_svy |>
#'   mutate(
#'     pid3_clean = replace_values(
#'       pid3,
#'       from = 4,
#'       to = 3,
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
#'     pid3_clean = replace_values(
#'       pid3,
#'       from = 4,
#'       to = 3,
#'       .label = "Party ID (3 categories)",
#'       .description = paste(
#'         "'Something else' (pid3 == 4) replaced with",
#'         "value 3 (Independent)."
#'       )
#'     )
#'   ) |>
#'   select(pid3, pid3_clean)
#'
#' new@metadata@transformations
#'
#' @family recoding
#' @export
replace_values <- function(
  x,
  ...,
  from = NULL,
  to = NULL,
  .label = NULL,
  .value_labels = NULL,
  .description = NULL
) {
  var_name <- tryCatch(
    dplyr::cur_column(),
    error = function(e) rlang::as_label(rlang::enquo(x))
  )
  .validate_label_args(.label, .value_labels, .description)

  result <- dplyr::replace_values(x, from = from, to = to, ...)

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
      fn = "replace_values",
      var = var_name
    ))
  }

  if (!is.null(.description)) {
    attr(result, "surveytidy_recode") <- list(
      fn = "replace_values",
      var = var_name,
      description = .description
    )
  }

  result
}
