#' Pass table through any type of filter.
#'
#' @param table A table to filter.
#' @param filter_obj A filter to apply to the table.
#' @param context A string to be used in logging or error messages. Defaults to
#' NULL.
#'
#' @return A list with the following elements:
#'               - passed: data frame with the rows that passed the filter
#'               - rejected: all other rows
#' @noRd
filter_all <- function(table,
                       filter_obj,
                       context = NULL) {
  if (is.null(filter_obj$type)) {
    # Filter type was not found. Assuming no filtering desired
    # TODO: Log this
    return(list(passed = table, rejected = data.frame()))
  }

  t <- tolower(filter_obj$type)

  if (t %in% c("in", "lt", "lt_eq", "gt", "gt_eq")) {
    filter_results <- filter_basic(table, filter_obj, context)
  } else if (t %in% c(
    "date_in", "date_lt",
    "date_lt_eq", "date_gt", "date_gt_eq"
  )) {
    filter_results <- filter_basic_date(table, filter_obj, context)
  } else if (t == "or") {
    filter_results <- filter_or(table, filter_obj, context)
  } else if (t == "and") {
    filter_results <- filter_and(table, filter_obj, context)
  } else if (t == "not") {
    filter_results <- filter_not(table, filter_obj, context)
  } else {
    stop_context(
      message = paste0("Filter type '", filter_obj$type, "' not implemented."),
      context = context
    )
  }
  filter_results
}
