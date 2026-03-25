#' Computes the combination of two or more features.
#'
#' @param mode 'linear' for linear combination of features, 'min' to take the
#'             minimum of the features, 'max' to take the maximum of the
#'             features.
#' @param all_tables List of all input tables (passed in from read_all_tables).
#' @param spec A list containing the following elements:
#'  - output_feature_name: Name of the output column.
#'  - grouping_column:     Name of the column to group by.
#'  - subfeature:        List of feature specs to combine.
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
featurise_combine <- function(mode,
                              all_tables,
                              spec,
                              context = NULL) {
  context <- c(context, paste0("featurise_combine:", mode))
  trace_context(context)

  mode <- tolower(mode)

  if (!mode %in% c("combine_linear", "combine_min", "combine_max")) {
    stop_context(
      message = paste0("Invalid combination mode: ", mode),
      context = context
    )
  }

  # Validate spec
  output_feature_name <- validate_output_feature_name(spec, context)

  # Choose starting missing value
  initial_missing_value <- switch(mode,
    combine_linear = 0,
    combine_min = Inf,
    combine_max = -Inf
  )
  missing_value <- initial_missing_value

  # Loop over subfeatures
  n <- length(spec$subfeature)
  subfeatures <- list()
  for (i in seq_along(spec$subfeature)) {
    subfeature_name <- names(spec$subfeature)[i]

    # Pass in the output feature name from the parent spec
    subfeature_spec <- spec$subfeature[[i]]
    subfeature_spec$output_feature_name <- subfeature_name # validated later
    # Read the absent_default_value from the subfeature spec and aggregate it
    this_absent_default_value <- validate_absent_default_value(
      subfeature_spec, context
    )
    # Issue 89: set `this_absent_default_value` to 0 if the subfeature is a
    # PRESENT
    if (subfeature_spec$transformation_type %>% tolower() == "present") {
      this_absent_default_value <- 0
    }

    # Update the missing value
    missing_value <- switch(mode,
      combine_linear = missing_value +
        (validate_weight(subfeature_spec, context) * this_absent_default_value),
      combine_min = min(missing_value, this_absent_default_value),
      combine_max = max(missing_value, this_absent_default_value)
    )

    # Calculate the feature
    extra_ctx <- c(context, "(feature", i, "of", n, ": ", subfeature_name, ")")
    # Note: no need to pass is_feature to featurise() because it won't be used
    # at this stage
    subfeatures[[i]] <- featurise(
      all_tables,
      subfeature_spec,
      context = c(context, extra_ctx)
    )
  }

  # Combine the subfeatures into a table
  joined_subfeatures <- join_feature_tables(subfeatures, context = context)
  joined_subfeatures <- joined_subfeatures$features

  # Then combine the subfeatures
  feature_table <- tibble::tibble(id = joined_subfeatures$id) %>%
    mutate(!!output_feature_name := initial_missing_value)
  for (i in seq_along(subfeatures)) {
    subfeature_name <- names(spec$subfeature)[i]
    weight <- spec$subfeature[[i]]$weight
    feature_table[[output_feature_name]] <- switch(mode,
      combine_linear = feature_table[[output_feature_name]] +
        (weight * joined_subfeatures[[subfeature_name]]),
      combine_min = pmin(
        feature_table[[output_feature_name]],
        joined_subfeatures[[subfeature_name]]
      ),
      combine_max = pmax(
        feature_table[[output_feature_name]],
        joined_subfeatures[[subfeature_name]]
      )
    )
  }

  list(
    feature_table = tibble::as_tibble(feature_table),
    missing_value = missing_value
  )
}
