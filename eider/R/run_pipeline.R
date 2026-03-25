#' Perform the entire feature transformation process
#'
#' Reads in data and feature specifications and performs the requisite
#' transformations. Please see the package vignettes for more detailed
#' information on the JSON specification of features.
#'
#' @param data_sources A list, whose names are the unique identifiers of the
#' data sources, and whose values are either the data frame itself or the file
#' path from which they should be read from. Only CSV files are supported at
#' this point in time.
#' @param feature_filenames A vector of file paths to the feature JSON
#' specifications. Defaults to `NULL`.
#' @param response_filenames A vector of file paths to the response JSON
#' specifications. Defaults to `NULL`.
#' @param all_ids A vector of all the unique numeric identifiers that should be
#' in the final feature table. If not given, this will be determined by taking
#' the union of all unique identifiers found in input tables used by at least
#' one feature.
#'
#' @return
#' A list with the following elementss:
#' * `features`: A data frame with all the features. The first column is the ID
#' column, and always has the name `id`. Subsequent columns are the features,
#' with column names as specified in the `output_feature_name` field of the
#' JSON files.
#' * `responses`: A data frame with all the responses. The structure is the same
#' as the `features` data frame.
#'
#' @examples
#' run_pipeline(
#'   data_sources = list(ae = eider_example("random_ae_data.csv")),
#'   feature_filenames = eider_example("ae_total_attendances.json")
#' )
#'
#' @export
run_pipeline <- function(
    data_sources,
    feature_filenames = NULL,
    response_filenames = NULL,
    all_ids = NULL) {
  # Read all the tables
  all_tables <- read_data(data_sources)

  # Parse JSON filenames into a list of `(feature, context)`, where `context`
  # is either the filename (for JSON read from file) or "user defined string"
  # (for JSON provided directly as a string)
  feature_objs <- lapply(feature_filenames, json_to_feature_wrapper)
  response_objs <- lapply(response_filenames, json_to_feature_wrapper)

  # Check that no output feature name is duplicated
  check_duplicate_feature_names(feature_objs, is_feature = TRUE)
  check_duplicate_feature_names(response_objs, is_feature = FALSE)

  # Calculate the features
  features <- lapply(
    feature_objs,
    function(f) {
      featurise_wrapper(f, TRUE, all_tables)
    }
  )
  responses <- lapply(
    response_objs,
    function(f) {
      featurise_wrapper(f, FALSE, all_tables)
    }
  )

  # Join all of them together
  join_feature_tables(c(features, responses), all_ids = all_ids)
}

json_to_feature_wrapper <- function(json_or_fname) {
  json_context <- json_or_fname
  file_or_string <- read_spec_type(json_or_fname)
  if (file_or_string == "string") {
    json_context <- "user defined string"
  } else {
    json_context <- json_or_fname
  }
  list(
    feature = json_to_feature(json_or_fname),
    context = json_context
  )
}

featurise_wrapper <- function(feature_and_context, is_feature, all_tables) {
  featurise(
    all_tables,
    feature_and_context$feature,
    is_feature = is_feature,
    context = paste0("featurise: ", feature_and_context$context)
  )
}

check_duplicate_feature_names <- function(specs, is_feature) {
  feature_names <- sapply(
    specs, function(spec) spec$feature$output_feature_name
  )
  feature_contexts <- sapply(
    specs, function(spec) spec$context
  )
  # Get the duplicated feature names
  duplicates <- unique(feature_names[duplicated(feature_names)])

  if (length(duplicates) > 0) {
    # Construct error message and quit
    context_str <- if (is_feature) {
      "Duplicate feature names found: \n"
    } else {
      "Duplicate response names found: \n"
    }
    for (d in duplicates) {
      context_str <- paste0(context_str, "   - '", d, "' found in\n")
      for (c in feature_contexts[feature_names == d]) {
        context_str <- paste0(context_str, "     -- ", c, "\n")
      }
    }

    stop(context_str)
  }
}
