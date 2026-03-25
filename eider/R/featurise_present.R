#' Generates a feature which, for each patient ID, contains a 1 if any row that
#' passes a specified filter exists, or 0 if not.
#'
#' @param all_tables List of all input tables (passed in from read_data).
#' @param spec A list containing the following elements:
#'  - source_table:         Filename of the source table to read from.
#'  - filter:      A filter object to apply to the source table.
#'  - output_feature_name: Name of the output column.
#'  - grouping_column:     Name of the column to group by.
#' @param context A character vector to be used in logging or error messages.
#' Defaults to NULL.
#'
#' @return A list with the following elements:
#' - feature_table: A data frame with a 1 or 0 in the output column for each
#'                  patient ID, depending on whether any rows in the source
#'                  table pass the filter.
#' - missing_value: The value to use for patients who have no matching rows in
#'                  the source table. This value is passed downstream to the
#'                  function which joins all the feature tables together. By
#'                  definition, this value is 0.
#' @noRd
featurise_present <- function(all_tables,
                              spec,
                              context = NULL) {
  context <- c(context, "featurise_present")
  trace_context(context)

  # Validate spec
  source_table <- validate_source_table(spec, all_tables, context)
  source_table <- preprocess_table(source_table, spec, context)
  output_feature_name <- validate_output_feature_name(spec, context)
  grouping_column <- validate_column_present(
    "grouping_column", spec, source_table, context
  )
  filter_obj <- spec$filter
  missing_value <- 0

  # Calculate feature
  feature_table <- source_table %>% filter_all(filter_obj, context)
  feature_table <- tryCatch(
    {
      feature_table %>%
        magrittr::extract2("passed") %>%
        rename(id = !!grouping_column) %>%
        group_by(id) %>%
        summarise(!!output_feature_name := 1)
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
