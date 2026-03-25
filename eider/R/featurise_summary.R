#' Groups by ID and computes a summary statistic of the values in all rows
#' which pass a given filter.
#'
#' @param mode The type of transformation to apply. This should be one
#' of: 'sum', 'nunique', 'mean', 'median', 'sd'.
#' @param all_tables List of all input tables (passed in from read_data).
#' @param spec A list containing the following elements:
#'  - source_table:         Filename of the source table to read from.
#'  - filter:      A filter object to apply to the source table.
#'  - aggregation_column:  Name of the column which provides the values to be
#'  -                      summarised over.
#'  - output_feature_name: Name of the output column.
#'  - grouping_column:     Name of the column to group the source table by
#'                         before summarisation
#'  - absent_default_value:The value to use for patients who have no matching
#'                         rows in the source table.
#' @param context A character vector to be used in logging or error messages.
#' Defaults to NULL.
#'
#' @return A list with the following elements:
#' - feature_table: A data frame with one row per patient ID and one column
#'                  containing a summary statistic of matching rows in the
#'                  source table. The column names are 'id' for the ID (this is
#'                  standardised across all feature tables), and the value of
#'                  output_column_name.
#' - missing_value: The value to use for patients who have no matching rows in
#'                  the source table. This value is passed downstream to the
#'                  function which joins all the feature tables together.
#' @noRd
featurise_summary <- function(mode,
                              all_tables,
                              spec,
                              context = NULL) {
  extra_ctx <- paste0("featurise_summary:", mode)
  context <- c(context, extra_ctx)
  trace_context(context)

  # Check mode and choose appropriate summary function
  mode <- tolower(mode)
  allowed_modes <- c(
    "sum", "nunique", "mean", "median", "sd",
    "first", "last", "min", "max"
  )
  if (!mode %in% allowed_modes) {
    stop_context(
      message = paste0(
        "Invalid summary mode: must be one of ",
        paste(
          sapply(allowed_modes, function(x) paste0("'", x, "'")),
          collapse = ", "
        )
      ),
      context = context
    )
  }
  summary_function <- switch(mode,
    sum = sum,
    nunique = n_distinct,
    mean = mean,
    median = stats::median,
    sd = stats::sd,
    first = first,
    last = last,
    min = min,
    max = max
  )

  # Validate spec
  source_table <- validate_source_table(spec, all_tables, context)
  source_table <- preprocess_table(source_table, spec, context)
  output_feature_name <- validate_output_feature_name(spec, context)
  aggregation_column <- validate_column_present(
    "aggregation_column", spec, source_table, context
  )
  grouping_column <- validate_column_present(
    "grouping_column", spec, source_table, context
  )
  missing_value <- validate_absent_default_value(spec, context)
  filter_obj <- spec$filter

  # Calculate feature
  feature_table <- source_table %>% filter_all(filter_obj, context)
  feature_table <- tryCatch(
    {
      feature_table %>%
        magrittr::extract2("passed") %>%
        rename(id = !!grouping_column) %>%
        group_by(id) %>%
        summarise(
          !!output_feature_name := summary_function(.data[[aggregation_column]])
        )
    },
    error = function(e) {
      stop_context(message = conditionMessage(e), context = context)
    }
  )

  feature_table <- pad_missing_values(
    source_table,
    grouping_column,
    missing_value,
    feature_table,
    context = c(context, "pad_missing_values")
  )

  list(
    feature_table = feature_table,
    missing_value = missing_value
  )
}
