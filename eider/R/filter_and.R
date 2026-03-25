#' Filter table rows based on a logical conjunction of multiple filters (i.e.
#' filter1 AND filter2 AND ...)
#'
#' @param table A data frame
#' @param filter_obj A list containing the following elements:
#'               - type: must be 'and' (case-insensitive)
#'               - subfilter: a list of filter objects
#' @param context A string to be used in logging or error messages. Defaults to
#' NULL.
#'
#' @return A list with the following elements:
#'               - passed: data frame with the rows that passed the filter
#'               - rejected: all other rows
#' @noRd
filter_and <- function(table,
                       filter_obj,
                       context = NULL) {
  context <- c(context, "filter_and")
  trace_context(context)

  if (tolower(filter_obj$type) != "and") {
    stop_context(
      message = paste0(
        "Expected filter type 'and', but got '", filter_obj$type, "'."
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
  # work than necessary, once a row fails any of the subfilters, it is added to
  # the 'failed' table and we don't need to check it against the remaining
  # subfilters.
  n <- length(filter_obj$subfilter)
  not_yet_failed <- table
  failed <- tibble::tibble()
  for (i in seq_along(filter_obj$subfilter)) {
    nm <- names(filter_obj$subfilter)[[i]]
    extra_ctx <- paste0("(", i, "/", n, ": ", nm, ")")
    subfilter_result <- filter_all(
      not_yet_failed,
      filter_obj$subfilter[[i]],
      c(context, extra_ctx)
    )
    not_yet_failed <- subfilter_result$passed
    failed <- bind_rows(failed, subfilter_result$rejected)
  }

  # This line to silence R CMD check 'no visible binding for global variable'.
  # Sort by the index column (to restore the input order) and remove it
  if (!has_indices) {
    not_yet_failed <- not_yet_failed %>%
      arrange(.data[[private_sentinel_index]]) %>%
      select(-all_of(private_sentinel_index))
    failed <- failed %>%
      arrange(.data[[private_sentinel_index]]) %>%
      select(-all_of(private_sentinel_index))
  }

  # Restore row names if present
  if (has_row_names) {
    not_yet_failed <- tibble::column_to_rownames(
      not_yet_failed,
      private_sentinel_row_names
    )
    failed <- tibble::column_to_rownames(
      failed,
      private_sentinel_row_names
    )
  }

  list(passed = not_yet_failed, rejected = failed)
}
