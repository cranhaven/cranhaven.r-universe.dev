# R/recode-values.R
#
# recode_values() — survey-aware value-to-value remapping.
# Own implementation matching the dplyr 1.2.0 API; propagates .label,
# .value_labels, .factor, and .description into @metadata via
# mutate.survey_base(). Supports .use_labels to auto-build the from/to
# map from pre-attached value labels.

#' Recode values using an explicit mapping
#'
#' @description
#' `recode_values()` replaces each value of `x` with a corresponding new
#' value. The mapping can be supplied in any of three ways:
#'
#' * **Formula interface** — pass `old_value ~ new_value` formulas in `...`:
#'   `recode_values(score, 1 ~ "SD", 2 ~ "D", 3 ~ "N", 4 ~ "A", 5 ~ "SA")`.
#' * **Lookup-table interface** — pass parallel `from` and `to` vectors.
#' * **Label-driven interface** — set `.use_labels = TRUE` to build the map
#'   from `attr(x, "labels")` (values become `from`, label strings become `to`).
#'
#' Values not found in the map are either kept unchanged
#' (`.unmatched = "default"`, the default) or trigger an error
#' (`.unmatched = "error"`).
#'
#' Unlike [replace_values()], which updates only specific matching values and
#' retains everything else, `recode_values()` is intended for full remapping:
#' every possible value in `x` typically has a corresponding entry in the map.
#'
#' When any of `.label`, `.value_labels`, `.factor`, or `.description` are
#' supplied, output label metadata is written to `@metadata` after [mutate()].
#' When none of these arguments are used, the output is identical to
#' `dplyr::recode_values()`.
#'
#' @param x Vector to recode.
#' @param ... `old_value ~ new_value` formulas describing the recoding map.
#'   Equivalent to supplying parallel `from`/`to` vectors. When `...` is
#'   non-empty, `from` and `.use_labels = TRUE` must not be used.
#' @param from Vector (or list of vectors, for many-to-one mapping) of old
#'   values. Required unless formulas are supplied in `...` or
#'   `.use_labels = TRUE`. Must be the same type as `x`.
#' @param to Vector of new values corresponding to `from`. Must be the same
#'   length as `from`.
#' @param default Value for entries in `x` not found in `from`. `NULL` (the
#'   default) keeps unmatched values unchanged. Ignored when
#'   `.unmatched = "error"`.
#' @param .unmatched `"default"` (the default) or `"error"`. When `"error"`,
#'   any value in `x` not present in `from` triggers a
#'   `surveytidy_error_recode_unmatched_values` error.
#' @param ptype An optional prototype declaring the desired output type.
#' @param .label `character(1)` or `NULL`. Variable label stored in
#'   `@metadata@variable_labels` after [mutate()]. Cannot be combined with
#'   `.factor = TRUE`.
#' @param .value_labels Named vector or `NULL`. Value labels stored in
#'   `@metadata@value_labels`. Names are the label strings; values are the
#'   data values.
#' @param .factor `logical(1)`. If `TRUE`, returns a factor. Levels are taken
#'   from `.value_labels` names if supplied, otherwise from `to` in lookup
#'   mode or from the right-hand sides of the `...` formulas in formula mode.
#'   Cannot be combined with `.label`.
#' @param .use_labels `logical(1)`. If `TRUE`, reads `attr(x, "labels")` to
#'   build the `from`/`to` map automatically: values become `from`, label
#'   strings become `to`. `x` must carry value labels; errors if not. Cannot
#'   be combined with formulas in `...`.
#' @param .description `character(1)` or `NULL`. Plain-language description
#'   of how the variable was created. Stored in
#'   `@metadata@transformations[[col]]$description` after [mutate()].
#'
#' @return A vector, factor, or `haven_labelled` vector:
#' * No surveytidy args — same output as `dplyr::recode_values()`.
#' * `.factor = TRUE` — a factor with levels in `to` order.
#' * `.label` or `.value_labels` supplied — a `haven_labelled` vector.
#'
#' @seealso
#' * `dplyr::recode_values()` for the base implementation.
#' * [replace_values()] for partial replacement (updates only matching values,
#'   retains existing value labels from `x`).
#' * [case_when()] for condition-based remapping.
#'
#' @examples
#' library(surveycore)
#' library(surveytidy)
#'
#' # create the survey design
#' ns_wave1_svy <- as_survey_nonprob(ns_wave1, weights = weight)
#'
#' # formula interface — recode pid3 using `old ~ new` formulas in `...`
#' new <- ns_wave1_svy |>
#'   mutate(
#'     party = recode_values(
#'       pid3,
#'       1 ~ "Democrat",
#'       2 ~ "Republican",
#'       3 ~ "Independent",
#'       4 ~ "Other"
#'     )
#'   ) |>
#'   select(pid3, party)
#'
#' new
#'
#' # formula interface with default for unmatched values
#' new <- ns_wave1_svy |>
#'   mutate(
#'     dem = recode_values(pid3, 1 ~ "Democrat", default = "Non-Democrat")
#'   ) |>
#'   select(pid3, dem)
#'
#' new
#'
#' # explicit from/to mapping — recode numeric codes to character labels
#' new <- ns_wave1_svy |>
#'   mutate(
#'     party = recode_values(
#'       pid3,
#'       from = c(1, 2, 3, 4),
#'       to = c("Democrat", "Republican", "Independent", "Other")
#'     )
#'   ) |>
#'   select(pid3, party)
#'
#' new
#'
#' # use default to catch unmatched values
#' new <- ns_wave1_svy |>
#'   mutate(
#'     dem = recode_values(
#'       pid3,
#'       from = c(1),
#'       to = c("Democrat"),
#'       default = "Non-Democrat"
#'     )
#'   ) |>
#'   select(pid3, dem)
#'
#' new
#'
#' # .use_labels = TRUE builds the from/to map from existing value labels
#' new <- ns_wave1_svy |>
#'   mutate(party = recode_values(pid3, .use_labels = TRUE)) |>
#'   select(pid3, party)
#'
#' new
#'
#' # attach a variable label via .label
#' new <- ns_wave1_svy |>
#'   mutate(
#'     party = recode_values(
#'       pid3,
#'       from = c(1, 2, 3, 4),
#'       to = c("Democrat", "Republican", "Independent", "Other"),
#'       .label = "Party identification"
#'     )
#'   ) |>
#'   select(pid3, party)
#'
#' new@metadata@variable_labels
#'
#' # collapse 4 categories to 3 and document via .value_labels
#' new <- ns_wave1_svy |>
#'   mutate(
#'     party = recode_values(
#'       pid3,
#'       from = c(1, 2, 3, 4),
#'       to = c(1, 2, 3, 3),
#'       .label = "Party ID (3 categories)",
#'       .value_labels = c(
#'         "Democrat" = 1,
#'         "Republican" = 2,
#'         "Independent/Other" = 3
#'       )
#'     )
#'   ) |>
#'   select(pid3, party)
#'
#' new@metadata@value_labels
#'
#' # return a factor with levels in `to` order
#' new <- ns_wave1_svy |>
#'   mutate(
#'     party = recode_values(
#'       pid3,
#'       from = c(1, 2, 3, 4),
#'       to = c("Democrat", "Republican", "Independent", "Other"),
#'       .factor = TRUE
#'     )
#'   ) |>
#'   select(pid3, party)
#'
#' new
#'
#' # attach a plain-language description of the transformation
#' new <- ns_wave1_svy |>
#'   mutate(
#'     party = recode_values(
#'       pid3,
#'       from = c(1, 2, 3, 4),
#'       to = c("Democrat", "Republican", "Independent", "Other"),
#'       .label = "Party identification",
#'       .description = paste(
#'         "pid3 recoded: 1->Democrat, 2->Republican,",
#'         "3->Independent, 4->Other."
#'       )
#'     )
#'   ) |>
#'   select(pid3, party)
#'
#' new@metadata@transformations
#'
#' @family recoding
#' @export
recode_values <- function(
  x,
  ...,
  from = NULL,
  to = NULL,
  default = NULL,
  .unmatched = "default",
  ptype = NULL,
  .label = NULL,
  .value_labels = NULL,
  .factor = FALSE,
  .use_labels = FALSE,
  .description = NULL
) {
  var_name <- tryCatch(
    dplyr::cur_column(),
    error = function(e) rlang::as_label(rlang::enquo(x))
  )
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

  has_formulas <- ...length() > 0L

  if (isTRUE(.use_labels) && has_formulas) {
    cli::cli_abort(
      c(
        "x" = "{.code .use_labels = TRUE} cannot be combined with formulas in {.arg ...}.",
        "i" = "Both describe the {.arg from}/{.arg to} map.",
        "v" = paste0(
          "Remove the formulas, or set {.code .use_labels = FALSE} and let ",
          "the formulas define the map."
        )
      ),
      class = "surveytidy_error_recode_use_labels_with_formulas"
    )
  }

  if (isTRUE(.use_labels)) {
    labels_attr <- attr(x, "labels", exact = TRUE)
    if (is.null(labels_attr)) {
      cli::cli_abort(
        c(
          "x" = "{.arg x} has no value labels.",
          "i" = "{.code .use_labels = TRUE} requires {.arg x} to carry value labels.",
          "v" = "Provide {.arg from} and {.arg to} explicitly instead."
        ),
        class = "surveytidy_error_recode_use_labels_no_attrs"
      )
    }
    from <- unname(labels_attr)
    to <- names(labels_attr)
  } else if (!has_formulas && is.null(from)) {
    cli::cli_abort(
      c(
        "x" = "No recoding map supplied.",
        "v" = paste0(
          "Pass {.code old ~ new} formulas in {.arg ...}, supply ",
          "{.arg from} and {.arg to}, or set {.code .use_labels = TRUE}."
        )
      ),
      class = "surveytidy_error_recode_from_to_missing"
    )
  }

  result <- tryCatch(
    dplyr::recode_values(
      x,
      from = from,
      to = to,
      default = default,
      unmatched = .unmatched,
      ptype = ptype,
      ...
    ),
    error = function(e) {
      # vctrs_error_combine_unmatched is the class thrown by dplyr::recode_values()
      # when unmatched = "error" and some values are not found in `from`.
      if (
        .unmatched == "error" &&
          inherits(e, "vctrs_error_combine_unmatched")
      ) {
        cli::cli_abort(
          c(
            "x" = "Some values in {.arg x} were not found in {.arg from}.",
            "i" = "Set {.code .unmatched = \"default\"} to keep unmatched values."
          ),
          class = "surveytidy_error_recode_unmatched_values",
          parent = e
        )
      }
      stop(e)
    }
  )

  if (isTRUE(.factor)) {
    factor_source <- if (!is.null(to)) {
      unique(to)
    } else {
      .formula_rhs_values(...)
    }
    result <- .factor_from_result(result, .value_labels, factor_source)
    attr(result, "surveytidy_recode") <- list(
      fn = "recode_values",
      var = var_name,
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
      fn = "recode_values",
      var = var_name
    ))
  }

  if (!is.null(.description)) {
    attr(result, "surveytidy_recode") <- list(
      fn = "recode_values",
      var = var_name,
      description = .description
    )
  }

  result
}
