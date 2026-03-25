#' Traverse a feature with appropriate logging
#' TODO: filters for subfeatures of COMBINE feature are not debug-logged
#'
#' @param json_data The JSON data for a feature
#'
#' @return The same data
#' @noRd
parse_feature <- function(json_data) {
  debug_context(
    context = "parse_feature",
    message = paste0(
      "Parsing feature of type ",
      json_data$transformation_type,
      " with output name ",
      json_data$output_feature_name
    )
  )

  # Initialise empty list
  feature_object <- list()

  # Read in all keys, using the special filter parser for the "filter"
  # key
  for (key in names(json_data)) {
    if (key == "filter") {
      feature_object$filter <- parse_single_or_nested(
        json_data$filter
      )
    } else {
      feature_object[[key]] <- json_data[[key]]
    }
  }
  feature_object
}

#' Traverse a single filter with appropriate logging
#'
#' @param filter A filter as defined in the origin json file
#'
#' @return A filter object
#' @noRd
parse_single_filter <- function(filter) {
  debug_context(
    context = "parse_single_filter",
    message = paste0(
      "Parsing single filter of type ",
      filter$type,
      " for column ",
      filter$column
    )
  )
  filter
}

#' Traverse a nested filter with appropriate logging
#'
#' @param nested_filter JSON data for a nested filter
#'
#' @return The same data
#' @noRd
parse_nested_filter <- function(filter) {
  debug_context(
    context = "parse_nested_filter",
    message = paste0(
      "Parsing nested filter of type ",
      filter$type,
      " with ",
      length(filter$subfilter),
      " subfilters"
    )
  )

  op_nested_filter <- list()
  op_nested_filter$type <- filter$type
  op_nested_filter$subfilter <- list()
  for (nm in names(filter$subfilter)) {
    target <- filter$subfilter[[nm]]
    op_nested_filter$subfilter[[nm]] <- parse_single_or_nested(target)
  }
  op_nested_filter
}

#' Check if a filter is nested or single and parse accordingly
#' @noRd
parse_single_or_nested <- function(filter) {
  if (!is.null(filter$subfilter)) {
    parse_nested_filter(filter)
  } else {
    parse_single_filter(filter)
  }
}

#' Take the json input file and produce a formatted R object which can direct
#' the filters
#'
#' @param filename The relative filepath to the json file or a json string
#'
#' @return A feature object - the spec
#' @noRd
json_to_feature <- function(filename) {
  json_data <- jsonlite::fromJSON(filename)
  parse_feature(json_data)
}
