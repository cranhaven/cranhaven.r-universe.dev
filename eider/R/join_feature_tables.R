#' Join feature tables together.
#'
#' @param calculated_features A list of calculated features, each of which is
#' produced by the featurise_... functions
#' @param all_ids A vector of all the unique identifiers that should be in the
#' final feature table. If not given, will generate a feature table containing
#' all unique identifiers found in input tables used by at least one feature.
#' @param context A string to be used in logging or error messages. Defaults to
#' NULL.
#'
#' @return A data frame with the feature tables joined together
#' @noRd
join_feature_tables <- function(
    calculated_features,
    all_ids = NULL,
    context = NULL) {
  context <- c(context, "join_feature_tables")
  trace_context(context)

  if (length(calculated_features) == 0) {
    stop_context(message = "No feature tables to join.", context = context)
  }

  # If not supplied, collect the set of IDs across all tables
  if (is.null(all_ids)) {
    debug_context(
      context = context,
      message = paste0(
        "List of IDs was not explicitly supplied. ",
        "Taking the union of all IDs from feature tables."
      )
    )
    get_ids <- function(feature) feature$feature_table$id
    all_ids <- lapply(calculated_features, get_ids) %>%
      unlist() %>%
      unique() %>%
      sort()
  } else {
    debug_context(
      context = context,
      message = paste0(
        "A list of IDs to include in the output was specified, containing ",
        length(all_ids),
        " IDs."
      )
    )
  }

  # Generate empty data frames for features and responses
  feature_df <- data.frame(id = all_ids)
  response_df <- data.frame(id = all_ids)

  for (i in seq_along(calculated_features)) {
    feature <- calculated_features[[i]]
    feature_table <- feature$feature_table
    missing_value <- feature$missing_value
    output_column_name <- setdiff(names(feature$feature_table), "id")

    # Join the feature table to the main table and replace any NAs with the
    # specified missing value
    feature <- calculated_features[[i]]
    if (feature$is_feature) {
      feature_df <- add_feature_column_to_df(feature_df, feature, context)
    } else {
      response_df <- add_feature_column_to_df(response_df, feature, context)
    }
  }

  list(features = feature_df, responses = response_df)
}

#' Helper function
#' @noRd
add_feature_column_to_df <- function(df, feature, context = NULL) {
  context <- c(context, "add_feature_column_to_df")

  output_column_name <- setdiff(names(feature$feature_table), "id")

  if (output_column_name %in% names(df)) {
    stop_context(
      message = paste0(
        "Feature column name '",
        output_column_name,
        "' already exists in the data frame."
      ),
      context = context
    )
  }

  df <- df %>%
    left_join(feature$feature_table, by = "id") %>%
    mutate(
      !!output_column_name :=
        coalesce(
          .data[[output_column_name]],
          feature$missing_value
        )
    )
}
