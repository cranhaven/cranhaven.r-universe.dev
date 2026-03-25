#' Filter table rows based on a logical disjunction of multiple filters (i.e.
#' filter1 OR filter2 OR ...)
#'
#' @param table A data frame
#' @param filter_obj A list containing the following elements:
#'               - type: must be 'or' (case-insensitive)
#'               - subfilter: a list of filter objects
#' @param context A string to be used in logging or error messages. Defaults to
#' NULL.
#'
#' @return A list with the following elements:
#'               - passed: data frame with the rows that passed the filter
#'               - rejected: all other rows
#' @noRd
filter_or <- function(table, filter_obj, context = NULL) {
  context <- c(context, "filter_or")
  trace_context(context)

  if (tolower(filter_obj$type) != "or") {
    stop_context(
      message = paste0(
        "Expected filter type 'or', but got '", filter_obj$type, "'."
      ),
      context = context
    )
  }

  # Move row names to a column if present
  private_sentinel_row_names <- "EIDER_PRIVATE_ROW_NAMES"
  has_row_names <- tibble::has_rownames(table)
  if (has_row_names) {
    table <- tibble::rownames_to_column(table, private_sentinel_row_names)
  }

  # Attach an index column to label the rows. This step gets rid of preexisting
  # row names hence the check above
  private_sentinel_index <- "EIDER_PRIVATE_INDEX"
  has_indices <- private_sentinel_index %in% names(table)
  if (!has_indices) {
    table <- tibble::rowid_to_column(table, private_sentinel_index)
  }

  # Pass the input table through each subfilter in turn. To avoid doing more
  # work than necessary, once a row passes any of the subfilters, it is added
  # to the 'passed' table and we don't need to check it against the remaining
  # subfilters.
  n <- length(filter_obj$subfilter)
  passed <- tibble::tibble()
  not_yet_passed <- table
  for (i in seq_along(filter_obj$subfilter)) {
    nm <- names(filter_obj$subfilter)[[i]]
    extra_ctx <- paste0("(", i, "/", n, ": ", nm, ")")
    subfilter_result <- filter_all(
      not_yet_passed,
      filter_obj$subfilter[[i]],
      c(context, extra_ctx)
    )
    passed <- bind_rows(passed, subfilter_result$passed)
    not_yet_passed <- subfilter_result$rejected
  }

  # Sort by the index column (to restore the input order) and remove it
  if (!has_indices) {
    passed <- passed %>%
      arrange(.data[[private_sentinel_index]]) %>%
      select(-all_of(private_sentinel_index))
    not_yet_passed <- not_yet_passed %>%
      arrange(.data[[private_sentinel_index]]) %>%
      select(-all_of(private_sentinel_index))
  }

  # Restore row names if present
  if (has_row_names) {
    passed <- tibble::column_to_rownames(
      passed,
      private_sentinel_row_names
    )
    not_yet_passed <- tibble::column_to_rownames(
      not_yet_passed,
      private_sentinel_row_names
    )
  }

  list(passed = passed, rejected = not_yet_passed)
}
