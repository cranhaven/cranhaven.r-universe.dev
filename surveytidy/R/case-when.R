# R/case-when.R
#
# case_when() — survey-aware vectorised if-else.
# Shadows dplyr::case_when() and propagates .label, .value_labels, .factor,
# and .description into @metadata via mutate.survey_base().

#' A generalised vectorised if-else
#'
#' @description
#' `case_when()` is a survey-aware version of [dplyr::case_when()] that
#' evaluates each formula case sequentially and uses the first match for each
#' element to determine the output value.
#'
#' Use `case_when()` when creating an entirely new vector. When partially
#' updating an existing vector, [replace_when()] is a better choice — it
#' retains the original value wherever no case matches and inherits existing
#' value labels from the input automatically.
#'
#' When any of `.label`, `.value_labels`, `.factor`, or `.description` are
#' supplied, output label metadata is written to `@metadata` after [mutate()].
#' When none of these arguments are used, the output is identical to
#' [dplyr::case_when()].
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> A sequence of two-sided
#'   formulas (`condition ~ value`). The left-hand side must be a logical
#'   vector. The right-hand side provides the replacement value. Cases are
#'   evaluated sequentially; the first matching case is used. `NULL` inputs
#'   are ignored.
#' @param .default The value used when all LHS conditions return `FALSE` or
#'   `NA`. If `NULL` (the default), unmatched rows receive `NA`.
#' @param .unmatched Handling of unmatched rows. `"default"` (the default)
#'   uses `.default`; `"error"` raises an error if any row is unmatched.
#' @param .ptype An optional prototype declaring the desired output type.
#'   Overrides the common type of the RHS inputs.
#' @param .size An optional size declaring the desired output length.
#'   Overrides the common size computed from the LHS inputs.
#' @param .label `character(1)` or `NULL`. Variable label stored in
#'   `@metadata@variable_labels` after [mutate()]. Cannot be combined with
#'   `.factor = TRUE`.
#' @param .value_labels Named vector or `NULL`. Value labels stored in
#'   `@metadata@value_labels`. Names are the label strings; values are the
#'   data values.
#' @param .factor `logical(1)`. If `TRUE`, returns a factor. Levels are
#'   ordered by the RHS values in formula order, or by `.value_labels` names
#'   if supplied. Cannot be combined with `.label`.
#' @param .description `character(1)` or `NULL`. Plain-language description
#'   of how the variable was created. Stored in
#'   `@metadata@transformations[[col]]$description` after [mutate()].
#'
#' @return A vector, factor, or `haven_labelled` vector:
#' * No surveytidy args — same output as [dplyr::case_when()].
#' * `.factor = TRUE` — a factor with levels in RHS formula order.
#' * `.label` or `.value_labels` supplied — a `haven_labelled` vector.
#'
#' @seealso
#' * [dplyr::case_when()] for the base implementation.
#' * [replace_when()] to partially update an existing vector; also inherits
#'   existing value labels from the input automatically.
#' * [if_else()] for the two-condition case.
#' * [recode_values()] for value-mapping with explicit `from`/`to` vectors.
#'
#' @examples
#' library(surveycore)
#' library(surveytidy)
#'
#' # create the survey design
#' ns_wave1_svy <- as_survey_nonprob(
#'   ns_wave1,
#'   weights = weight
#' )
#'
#' # basic case_when — identical to dplyr::case_when()
#' new <- ns_wave1_svy |>
#'   mutate(
#'     age_pid = case_when(
#'       age < 30 & pid3 == 1 ~ "18-29 Democrats",
#'       age < 30 & pid3 == 2 ~ "18-29 Republicans",
#'       age < 30 & pid3 %in% c(3:4) ~ "18-29 Independents",
#'       .default = "Everyone else"
#'     )
#'   ) |>
#'   select(age, pid3, age_pid)
#'
#' # by default, no metadata is attached
#' new
#' new@metadata
#'
#' # attach a variable label via .label
#' new <- ns_wave1_svy |>
#'   mutate(
#'     age_pid = case_when(
#'       age < 30 & pid3 == 1 ~ "18-29 Democrats",
#'       age < 30 & pid3 == 2 ~ "18-29 Republicans",
#'       age < 30 & pid3 %in% c(3:4) ~ "18-29 Independents",
#'       .default = "Everyone else",
#'       .label = "Age and Partisanship"
#'     )
#'   ) |>
#'   select(age, pid3, age_pid)
#'
#' new@metadata@variable_labels
#'
#' # attach a plain-language description of the transformation
#' new <- ns_wave1_svy |>
#'   mutate(
#'     age_pid = case_when(
#'       age < 30 & pid3 == 1 ~ "18-29 Democrats",
#'       age < 30 & pid3 == 2 ~ "18-29 Republicans",
#'       age < 30 & pid3 %in% c(3:4) ~ "18-29 Independents",
#'       .default = "Everyone else",
#'       .label = "Age and Partisanship",
#'       .description = paste(
#'         "Young (< 30) Democrats, Republicans, and Independents",
#'         "were grouped by partisanship; everyone else was set to",
#'         "'Everyone else'."
#'       )
#'     )
#'   ) |>
#'   select(age, pid3, age_pid)
#'
#' new@metadata@transformations
#'
#' # attach value labels alongside numeric codes
#' new <- ns_wave1_svy |>
#'   mutate(
#'     age_pid = case_when(
#'       age < 30 & pid3 == 1 ~ 1,
#'       age < 30 & pid3 == 2 ~ 2,
#'       age < 30 & pid3 %in% c(3:4) ~ 3,
#'       .default = 4,
#'       .label = "Age and Partisanship",
#'       .value_labels = c(
#'         "18-29 Democrats" = 1,
#'         "18-29 Republicans" = 2,
#'         "18-29 Independents" = 3,
#'         "Everyone else" = 4
#'       )
#'     )
#'   ) |>
#'   select(age, pid3, gender, age_pid)
#'
#' new@metadata@value_labels
#'
#' # return a factor with levels in formula order
#' new <- ns_wave1_svy |>
#'   mutate(
#'     age_pid = case_when(
#'       age < 30 & pid3 == 1 ~ "18-29 Democrats",
#'       age < 30 & pid3 == 2 ~ "18-29 Republicans",
#'       age < 30 & pid3 %in% c(3:4) ~ "18-29 Independents",
#'       .default = "Everyone else",
#'       .factor = TRUE
#'     )
#'   ) |>
#'   select(age, pid3, age_pid)
#'
#' new
#'
#' @family recoding
#' @export
case_when <- function(
  ...,
  .default = NULL,
  .unmatched = "default",
  .ptype = NULL,
  .size = NULL,
  .label = NULL,
  .value_labels = NULL,
  .factor = FALSE,
  .description = NULL
) {
  .validate_label_args(.label, .value_labels, .description)
  if (isTRUE(.factor) && !is.null(.label)) {
    cli::cli_abort(
      c(
        "x" = "{.arg .label} cannot be used with {.code .factor = TRUE}.",
        "i" = "Factor levels carry their own labels."
      ),
      class = "surveytidy_error_recode_factor_with_label"
    )
  }

  result <- dplyr::case_when(
    ...,
    .default = .default,
    .unmatched = .unmatched,
    .ptype = .ptype,
    .size = .size
  )

  if (isTRUE(.factor)) {
    formulas <- list(...)
    all_literal <- all(vapply(
      formulas,
      function(f) rlang::is_syntactic_literal(rlang::f_rhs(f)),
      logical(1L)
    ))
    if (all_literal) {
      formula_values <- character(length(formulas))
      for (i in seq_along(formulas)) {
        formula_values[[i]] <- as.character(rlang::f_rhs(formulas[[i]]))
      }
      if (!is.null(.default) && !is.na(.default)) {
        formula_values <- c(formula_values, as.character(.default))
      }
    } else {
      formula_values <- unique(as.character(result[!is.na(result)]))
    }
    result <- .factor_from_result(result, .value_labels, formula_values)
    attr(result, "surveytidy_recode") <- list(
      fn = "case_when",
      var = NULL,
      description = .description
    )
    return(result)
  }

  if (!is.null(.label) || !is.null(.value_labels)) {
    return(.wrap_labelled(
      result,
      .label,
      .value_labels,
      .description,
      fn = "case_when",
      var = NULL
    ))
  }

  if (!is.null(.description)) {
    attr(result, "surveytidy_recode") <- list(
      fn = "case_when",
      var = NULL,
      description = .description
    )
  }

  result
}
