#' Computes the number of rows per patient ID which contain specific values in
#' specific columns.
#'
#' @param all_tables List of all input tables (passed in from read_data).
#' @param spec A list containing the following elements:
#'  - source_table:         Filename of the source table to read from.
#'  - filter:      A filter object to apply to the source table.
#'  - output_feature_name: Name of the output column.
#'  - grouping_column:     Name of the column to group by.
#'  - absent_default_value:The value to use for patients who have no matching
#'                         rows in the source table.
#' @param context A character vector to be used in logging or error messages.
#' Defaults to NULL.
#'
#' @return A list with the following elements:
#' - feature_table: A data frame with one row per patient ID and one column
#'                  containing the count of matching rows in the source table.
#'                  The column names are 'id' for the ID (this is standardised
#'                  across all feature tables), and the value of
#'                  output_column_name.
#' - missing_value: The value to use for patients who have no matching rows in
#'                  the source table. This value is passed downstream to the
#'                  function which joins all the feature tables together.
#' @noRd
featurise_count <- function(all_tables,
                            spec,
                            context = NULL) {
  context <- c(context, "featurise_count")
  trace_context(context)

  # Validate spec
  source_table <- validate_source_table(spec, all_tables, context)
  source_table <- preprocess_table(source_table, spec, context)
  output_feature_name <- validate_output_feature_name(spec, context)
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
        summarise(!!output_feature_name := n())
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
