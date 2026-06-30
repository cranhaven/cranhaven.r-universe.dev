#' @title Fetch All Records from Finna API
#'
#' @description
#' This function fetches records from the Finna API in chunks of 100,000,
#' automatically paginating through the results until the maximum number of
#' records is reached.
#'
#' @param base_query A string specifying the base query. Defaults to "*".
#' @param base_filters A character vector of filters to apply to the query.
#'                     Defaults to `c('collection:"FEN"')`.
#' @param sort A string defining the sort order of the results. Default is "main_date_str asc".
#' @param limit_per_query An integer specifying the number of records to fetch per query. Defaults to 100000.
#' @param total_limit An integer specifying the maximum number of records to fetch. Defaults to `Inf`.
#' @return A tibble containing all fetched records.
#'
#' @examples
#' \dontrun{
#'   results <- fetch_all_records(
#'     base_query = "*",
#'     base_filters = c('collection:"FEN"'),
#'     sort = "main_date_str asc",
#'     limit_per_query = 100000,
#'     total_limit = Inf
#'   )
#'   print(results)
#' }
#' @export
fetch_all_records <- function(base_query = "*",
                              base_filters = c('collection:"FEN"'),
                              sort = "main_date_str asc",
                              limit_per_query = 100000,
                              total_limit = Inf) {

  # Initialize storage for results
  all_results <- list()
  total_fetched <- 0
  continue_fetching <- TRUE
  last_fetched_id <- NULL

  while (continue_fetching) {
    # Construct filters for the current subquery
    filters <- base_filters
    if (!is.null(last_fetched_id)) {
      filters <- c(filters, paste0('id:[', last_fetched_id, ' TO *]'))
    }

    # Inform user about the current query
    message(sprintf("Fetching records starting from id: %s",
                    ifelse(is.null(last_fetched_id), "beginning", last_fetched_id)))

    # Retry logic for handling errors
    attempt <- 1
    max_attempts <- 3
    results <- NULL
    while (is.null(results) && attempt <= max_attempts) {
      tryCatch({
        # Fetch records using the subquery
        results <- search_finna(
          query = base_query,
          filters = filters,
          sort = sort,
          limit = limit_per_query
        )
      }, error = function(e) {
        warning(sprintf("Attempt %d failed: %s", attempt, e$message))
        attempt <- attempt + 1
        Sys.sleep(5)  # Delay before retrying
      })
    }

    # If results are still NULL after retries, stop fetching
    if (is.null(results)) {
      warning("Failed to fetch records after multiple attempts. Stopping.")
      break
    }

    # Check if results are empty
    if (nrow(results) == 0) {
      message("No more records found.")
      break
    }

    # Add the fetched records to the main storage
    all_results <- c(all_results, list(results))

    # Update the total fetched count
    num_fetched <- nrow(results)
    total_fetched <- total_fetched + num_fetched

    # Stop if we've hit the total limit
    if (total_fetched >= total_limit) {
      message("Total limit reached.")
      break
    }

    # Update the last fetched id for the next query
    last_fetched_id <- max(results$id, na.rm = TRUE)

    # Stop if fewer than limit_per_query records are fetched
    if (num_fetched < limit_per_query) {
      message("No more records to fetch.")
      break
    }
  }

  # Combine all results into a single tibble
  combined_results <- dplyr::bind_rows(all_results)
  return(combined_results)
}


























# fennica_all_records <- function(base_query = "*",
#                               base_filters = c('collection:"FEN"'),
#                               sort = "main_date_str asc",
#                               limit_per_query = 100000,
#                               total_limit = Inf) {
#
#   all_results <- list()  # List to store results
#   total_fetched <- 0     # Total records fetched so far
#   continue_fetching <- TRUE
#   last_fetched_date <- NULL
#
#   while (continue_fetching) {
#     # Construct filters for subquery
#     filters <- base_filters
#
#     # Add a date range filter if this isn't the first query
#     if (!is.null(last_fetched_date)) {
#       filters <- c(filters, paste0('search_daterange_mv:"[', last_fetched_date, ' TO 9999]"'))
#     }
#
#     # Fetch records using the subquery
#     message(sprintf("Fetching records starting from date: %s",
#                     ifelse(is.null(last_fetched_date), "beginning", last_fetched_date)))
#     results <- search_finna(query = base_query, filters = filters, sort = sort, limit = limit_per_query)
#
#     # Add fetched results to the main list
#     all_results <- c(all_results, list(results))
#
#     # Update total fetched count
#     num_fetched <- nrow(results)
#     total_fetched <- total_fetched + num_fetched
#
#     # Check if we've fetched all results or hit the total limit
#     if (num_fetched < limit_per_query || total_fetched >= total_limit) {
#       continue_fetching <- FALSE
#     } else {
#       # Update the last fetched date for the next query
#       last_fetched_date <- max(results$Year, na.rm = TRUE)  # Update to the latest year fetched
#     }
#   }
#
#   # Combine all results into a single tibble
#   combined_results <- dplyr::bind_rows(all_results)
#   return(combined_results)
# }
#
# # Example usage
# final_results <- fennica_all_records(
#   base_query = "*",
#   base_filters = c('collection:"FEN"'),
#   sort = "main_date_str asc",
#   limit_per_query = 100000,
#   total_limit = Inf
# )
#
# # View results
# print(final_results)
