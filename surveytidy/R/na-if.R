# R/na-if.R
#
# na_if() — survey-aware value-to-NA replacement.
# Shadows dplyr::na_if() and propagates updated value labels and .description
# into @metadata via mutate.survey_base(). Supports a vector y (multiple
# values replaced in a single call) and .update_labels to control whether
# label entries for NA'd values are removed.

#' Convert values to `NA`
#'
#' @description
#' `na_if()` is a survey-aware version of [dplyr::na_if()] that converts
#' values equal to `y` to `NA`. It is useful for replacing sentinel values
#' (e.g., `999` for "don't know") with proper missing values.
#'
#' Unlike [dplyr::na_if()], which accepts only a scalar `y`, this version
#' accepts a vector `y` and replaces all matching values in a single call.
#'
#' When `x` carries value labels, `na_if()` automatically inherits those
#' labels. By default (`.update_labels = TRUE`), the label entries for the
#' NA'd values are removed from the output; set `.update_labels = FALSE` to
#' retain them (useful when you want to document what was set to missing).
#'
#' @param x Vector to modify.
#' @param y Value or vector of values to replace with `NA`. `y` is cast to
#'   the type of `x` before comparison. When `y` has more than one element,
#'   each value is replaced sequentially.
#' @param .update_labels `logical(1)`. If `TRUE` (the default) and `x`
#'   carries value labels, label entries for values in `y` are removed from
#'   the output's value labels. Set to `FALSE` to retain all inherited labels
#'   even for values that were set to `NA`.
#' @param .description `character(1)` or `NULL`. Plain-language description
#'   of how the variable was created. Stored in
#'   `@metadata@transformations[[col]]$description` after [mutate()].
#'
#' @return A modified version of `x` where values equal to `y` are replaced
#'   with `NA`. If `x` carries value labels, returns a `haven_labelled` vector
#'   with updated (or retained) labels; otherwise returns the same type as `x`.
#'
#' @seealso
#' * [dplyr::na_if()] for the base implementation.
#' * [dplyr::coalesce()] to replace `NA`s with the first non-missing value.
#' * [replace_values()] for replacing specific values with a new value rather
#'   than `NA`.
#' * [replace_when()] for condition-based in-place replacement.
#'
#' @examples
#' library(surveycore)
#' library(surveytidy)
#'
#' # create the survey design
#' ns_wave1_svy <- as_survey_nonprob(ns_wave1, weights = weight)
#'
#' # basic na_if — replace "Something else" (pid3 == 4) with NA
#' new <- ns_wave1_svy |>
#'   mutate(pid3_clean = na_if(pid3, 4)) |>
#'   select(pid3, pid3_clean)
#'
#' new
#'
#' # replace multiple values at once — Independent (3) and "Something else" (4)
#' new <- ns_wave1_svy |>
#'   mutate(pid3_2party = na_if(pid3, c(3, 4))) |>
#'   select(pid3, pid3_2party)
#'
#' new
#'
#' # .update_labels = TRUE (default) drops label entries for NA'd values
#' new <- ns_wave1_svy |>
#'   mutate(pid3_clean = na_if(pid3, 4, .update_labels = TRUE)) |>
#'   select(pid3, pid3_clean)
#'
#' # "Something else" (4) is removed from pid3_clean's value labels
#' new@metadata@value_labels$pid3_clean
#'
#' # .update_labels = FALSE retains label entries even for NA'd values
#' new <- ns_wave1_svy |>
#'   mutate(pid3_clean = na_if(pid3, 4, .update_labels = FALSE)) |>
#'   select(pid3, pid3_clean)
#'
#' # "Something else" (4) is still in pid3_clean's value labels
#' new@metadata@value_labels$pid3_clean
#'
#' # attach a plain-language description of the transformation
#' new <- ns_wave1_svy |>
#'   mutate(
#'     pid3_clean = na_if(
#'       pid3,
#'       4,
#'       .description = "Set 'Something else' (pid3 == 4) to NA."
#'     )
#'   ) |>
#'   select(pid3, pid3_clean)
#'
#' new@metadata@transformations
#'
#' @family recoding
#' @export
na_if <- function(x, y, .update_labels = TRUE, .description = NULL) {
  var_name <- tryCatch(
    dplyr::cur_column(),
    error = function(e) rlang::as_label(rlang::enquo(x))
  )
  if (
    !is.logical(.update_labels) ||
      length(.update_labels) != 1L ||
      is.na(.update_labels)
  ) {
    cli::cli_abort(
      c(
        "x" = "{.arg .update_labels} must be a single {.cls logical} value.",
        "i" = "Got {.cls {class(.update_labels)}} of length {length(.update_labels)}."
      ),
      class = "surveytidy_error_na_if_update_labels_not_scalar"
    )
  }
  .validate_label_args(
    label = NULL,
    value_labels = NULL,
    description = .description
  )

  # dplyr::na_if() requires y to be scalar or same length as x.
  # When y is a vector of values-to-NA (length > 1 and != length(x)),
  # apply sequentially so every value in y gets replaced.
  result <- x
  for (yval in y) {
    result <- dplyr::na_if(result, yval)
  }

  labels_attr <- attr(x, "labels", exact = TRUE)
  label_attr <- attr(x, "label", exact = TRUE)

  if (!is.null(labels_attr)) {
    if (isTRUE(.update_labels)) {
      keep <- !labels_attr %in% y
      labels_attr <- labels_attr[keep]
      if (length(labels_attr) == 0L) labels_attr <- NULL
    }
    return(.wrap_labelled(
      result,
      label_attr,
      labels_attr,
      .description,
      fn = "na_if",
      var = var_name
    ))
  }

  if (!is.null(.description)) {
    attr(result, "surveytidy_recode") <- list(
      fn = "na_if",
      var = var_name,
      description = .description
    )
  }

  result
}
