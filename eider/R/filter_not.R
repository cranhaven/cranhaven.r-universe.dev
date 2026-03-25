#' Filter table rows based on a logical negation of a filter (i.e. NOT filter1)
#'
#' @param table A data frame
#' @param filter_obj A list containing the following elements:
#'               - type: must be 'not' (case-insensitive)
#'               - subfilter: a single filter object
#' @param context A string to be used in logging or error messages. Defaults to
#' NULL.
#'
#' @return A list with the following elements:
#'               - passed: data frame with the rows that satisfies the NOT
#'                         filter
#'               - rejected: all other rows
#' @noRd
filter_not <- function(table,
                       filter_obj,
                       context = NULL) {
  context <- c(context, "filter_not")
  trace_context(context)

  if (tolower(filter_obj$type) != "not") {
    stop_context(
      message = paste0(
        "Expected filter type 'not', but got '", filter_obj$type, "'."
      ),
      context = context
    )
  }

  subfilter_results <- filter_all(table, filter_obj$subfilter)
  list(
    passed = subfilter_results$rejected,
    rejected = subfilter_results$passed
  )
}
