# R/rowstats.R
#
# Row-aggregate transformation functions for survey variables.
# These are NOT dplyr verbs - they operate inside mutate() using dplyr::pick()
# to resolve tidyselect column selections and integrate with
# mutate.survey_base() via the surveytidy_recode attribute protocol.
#
# Functions defined here:
#   row_means()   - row-wise mean across tidyselect-selected columns
#   row_sums()    - row-wise sum across tidyselect-selected columns
#
# Shared helpers (in R/utils.R):
#   .validate_transform_args()  - validate .label/.description for transform fns
#   .set_recode_attrs()         - set label, labels, surveytidy_recode attrs

#  row_means()

#' Compute row-wise means across selected columns
#'
#' @description
#' `row_means()` computes the mean of each row across a tidyselect-selected set
#' of numeric columns. It is designed for use inside [mutate()] on survey design
#' objects. When called inside `mutate()`, the transformation is recorded in
#' `@metadata@transformations[[col]]`.
#'
#' @param .cols <[`tidy-select`][tidyselect::language]> Columns to average
#'   across, evaluated via [dplyr::pick()]. Typical values:
#'   `c(a, b, c)`, `starts_with("y")`, `where(is.numeric)`. Must resolve to at
#'   least one column, and all selected columns must be numeric.
#' @param na.rm `logical(1)`. If `TRUE`, `NA` values are excluded before
#'   computing the mean. If all values in a row are `NA` and `na.rm = TRUE`,
#'   the result is `NaN` (matching base R `rowMeans()` behavior). Default
#'   `FALSE`.
#' @param .label `character(1)` or `NULL`. Variable label stored in
#'   `@metadata@variable_labels[[col]]` after `mutate()`. If `NULL`, falls back
#'   to the output column name from [dplyr::cur_column()].
#' @param .description `character(1)` or `NULL`. Plain-language description of
#'   the transformation stored in `@metadata@transformations[[col]]$description`
#'   after `mutate()`.
#'
#' @return A `double` vector of length equal to the number of rows in the
#'   current data context.
#'
#' @examples
#' # create a dummy survey object
#' d <- surveycore::as_survey(
#'   data.frame(
#'     y1 = c(1, 2, 3),
#'     y2 = c(4, 5, 6),
#'     wt = c(1, 1, 1)
#'   ),
#'   weights = wt
#' )
#'
#' # use a vector of columns to create the score
#' mutate(d, score = row_means(c(y1, y2)))
#'
#' # use tidy-select for columns and add a label
#' d |>
#'   mutate(
#'     score = row_means(
#'       tidyselect::starts_with("y"),
#'       na.rm = TRUE,
#'       .label = "Score"
#'     )
#'   )
#'
#' @family transformation
#' @export
row_means <- function(
  .cols,
  na.rm = FALSE,
  .label = NULL,
  .description = NULL
) {
  # 1. Validate na.rm inline
  if (!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm)) {
    cli::cli_abort(
      c(
        "x" = "{.arg na.rm} must be a single non-NA logical value.",
        "i" = "Got {.cls {class(na.rm)}} of length {length(na.rm)}."
      ),
      class = "surveytidy_error_rowstats_bad_arg"
    )
  }

  # 2. Validate .label and .description via shared helper
  .validate_transform_args(
    .label,
    .description,
    "surveytidy_error_rowstats_bad_arg"
  )

  # 3. Resolve columns
  df <- dplyr::pick({{ .cols }})

  # 4. Zero-column guard
  if (ncol(df) == 0L) {
    cli::cli_abort(
      c(
        "x" = "{.arg .cols} matched 0 columns.",
        "i" = "{.fn row_means} requires at least one numeric column."
      ),
      class = "surveytidy_error_row_means_zero_cols"
    )
  }

  # 5. Non-numeric guard
  not_numeric <- names(df)[!vapply(df, is.numeric, logical(1L))]
  if (length(not_numeric) > 0L) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{length(not_numeric)} selected column{?s} {?is/are} not numeric: ",
          "{.field {not_numeric}}."
        ),
        "i" = "{.fn row_means} requires all columns to be numeric."
      ),
      class = "surveytidy_error_row_means_non_numeric"
    )
  }

  # 6. Compute
  source_cols <- names(df)
  effective_label <- .label %||%
    tryCatch(
      dplyr::cur_column(),
      error = function(e) NULL
    )
  result <- rowMeans(df, na.rm = na.rm)

  # 7. Set attrs and return
  .set_recode_attrs(
    result,
    effective_label,
    NULL,
    "row_means",
    source_cols,
    .description
  )
}

#  row_sums()

#' Compute row-wise sums across selected columns
#'
#' @description
#' `row_sums()` computes the sum of each row across a tidyselect-selected set
#' of numeric columns. It is designed for use inside [mutate()] on survey design
#' objects. When called inside `mutate()`, the transformation is recorded in
#' `@metadata@transformations[[col]]`.
#'
#' @param .cols <[`tidy-select`][tidyselect::language]> Columns to sum across,
#'   evaluated via [dplyr::pick()]. Typical values: `c(a, b, c)`,
#'   `starts_with("y")`, `where(is.numeric)`. Must resolve to at least one
#'   column, and all selected columns must be numeric.
#' @param na.rm `logical(1)`. If `TRUE`, `NA` values are excluded before
#'   summing. If all values in a row are `NA` and `na.rm = TRUE`, the result is
#'   `0` (matching base R `rowSums()` behavior). Default `FALSE`.
#' @param .label `character(1)` or `NULL`. Variable label stored in
#'   `@metadata@variable_labels[[col]]` after `mutate()`. If `NULL`, falls back
#'   to the output column name from [dplyr::cur_column()].
#' @param .description `character(1)` or `NULL`. Plain-language description of
#'   the transformation stored in `@metadata@transformations[[col]]$description`
#'   after `mutate()`.
#'
#' @return A `double` vector of length equal to the number of rows in the
#'   current data context.
#'
#' @examples
#' # create a dummy survey object
#' d <- surveycore::as_survey(
#'   data.frame(
#'     y1 = c(1, 2, 3),
#'     y2 = c(4, 5, 6),
#'     wt = c(1, 1, 1)
#'   ),
#'   weights = wt
#' )
#'
#' # use a vector of columns to create the total
#' mutate(d, total = row_sums(c(y1, y2)))
#'
#' # use tidy-select for columns and add a label
#' d |>
#'   mutate(
#'     total = row_sums(
#'       tidyselect::starts_with("y"),
#'       na.rm = TRUE,
#'       .label = "Total"
#'     )
#'   )
#'
#' @family transformation
#' @export
row_sums <- function(.cols, na.rm = FALSE, .label = NULL, .description = NULL) {
  # 1. Validate na.rm inline
  if (!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm)) {
    cli::cli_abort(
      c(
        "x" = "{.arg na.rm} must be a single non-NA logical value.",
        "i" = "Got {.cls {class(na.rm)}} of length {length(na.rm)}."
      ),
      class = "surveytidy_error_rowstats_bad_arg"
    )
  }

  # 2. Validate .label and .description via shared helper
  .validate_transform_args(
    .label,
    .description,
    "surveytidy_error_rowstats_bad_arg"
  )

  # 3. Resolve columns
  df <- dplyr::pick({{ .cols }})

  # 4. Zero-column guard
  if (ncol(df) == 0L) {
    cli::cli_abort(
      c(
        "x" = "{.arg .cols} matched 0 columns.",
        "i" = "{.fn row_sums} requires at least one numeric column."
      ),
      class = "surveytidy_error_row_sums_zero_cols"
    )
  }

  # 5. Non-numeric guard
  not_numeric <- names(df)[!vapply(df, is.numeric, logical(1L))]
  if (length(not_numeric) > 0L) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{length(not_numeric)} selected column{?s} {?is/are} not numeric: ",
          "{.field {not_numeric}}."
        ),
        "i" = "{.fn row_sums} requires all columns to be numeric."
      ),
      class = "surveytidy_error_row_sums_non_numeric"
    )
  }

  # 6. Compute
  source_cols <- names(df)
  effective_label <- .label %||%
    tryCatch(
      dplyr::cur_column(),
      error = function(e) NULL
    )
  result <- rowSums(df, na.rm = na.rm)

  # 7. Set attrs and return
  .set_recode_attrs(
    result,
    effective_label,
    NULL,
    "row_sums",
    source_cols,
    .description
  )
}
