#' @title Fetch Fennica Collection Records by Year Ranges from Finna API (Including NA Dates)
#'
#' @description
#' This function fetches records from the Finna API in chunks divided by year ranges, handling missing date values for the Fennica collection.
#'
#' @param base_query The base query string, defaults to "*".
#' @param base_filters A character vector of filters for the search, e.g., `c('collection:"FEN"')`.
#' @param year_ranges A list of numeric vectors specifying year ranges, e.g., `list(c(2000, 2005), c(2006, 2010))`.
#' @param include_na Whether to include records with missing `main_date_str`. Default is `TRUE`.
#' @param limit_per_query Maximum number of records to fetch per query. Default is 100000.
#' @param total_limit Maximum number of records to fetch overall. Default is `Inf`.
#' @param delay_after_query Delay in seconds between queries. Default is 5.
#' @return A tibble containing all fetched records.
#'
#' @export
fetch_fennica_records <- function(base_query = "*",
                                  base_filters = c('collection:"FEN"',"finna.include_hidden_parts:1"),
                                  year_ranges = list(c(0, as.numeric(format(Sys.Date(), "%Y")))),
                                  include_na = TRUE,
                                  limit_per_query = 100000,
                                  total_limit = Inf,
                                  delay_after_query = 5) {

  # Input validation
  if (!all(sapply(year_ranges, function(x) length(x) == 2 && is.numeric(x) && x[1] <= x[2]))) {
    stop("Each year range must be a numeric vector of length 2, where the first element <= the second.")
  }

  if (!is.numeric(limit_per_query) || limit_per_query <= 0) {
    stop("limit_per_query must be a positive integer.")
  }

  if (!is.numeric(total_limit) || total_limit <= 0) {
    stop("total_limit must be a positive number.")
  }

  message("Fetching records for Fennica collection started...")

  all_results <- list()
  total_fetched <- 0

  # Base filter for Fennica collection
  base_filters <- c('collection:"FEN"')

  # Iterate over year ranges
  for (range in year_ranges) {
    if (total_fetched >= total_limit) {
      message("Reached the total record limit.")
      break
    }

    # Construct the range-specific filter
    range_filter <- paste0('main_date_str:["', range[1], '" TO "', range[2], '"]')
    filters <- c(base_filters, range_filter)

    # Fetch results for the range
    results <- tryCatch({
      search_finna(
        query = base_query,
        filters = filters,
        limit = limit_per_query
      )
    }, error = function(e) {
      warning(sprintf("Error fetching data for range %s-%s: %s", range[1], range[2], e$message))
      return(NULL)
    })

    if (!is.null(results) && nrow(results) > 0) {
      if (total_fetched + nrow(results) > total_limit) {
        results <- results[1:(total_limit - total_fetched), ]
      }
      all_results <- c(all_results, list(results))
      total_fetched <- total_fetched + nrow(results)
    }

    message(sprintf("Range %d-%d: Fetched %d records (Total: %d)", range[1], range[2], ifelse(is.null(results), 0, nrow(results)), total_fetched))

    if (total_fetched >= total_limit) {
      message("Reached the total record limit.")
      break
    }

    Sys.sleep(delay_after_query)
  }

  # Optionally fetch NA values
  if (include_na && total_fetched < total_limit) {
    message("Fetching records with missing main_date_str...")
    na_results <- tryCatch({
      search_finna(
        query = base_query,
        filters = c(base_filters, '-main_date_str:*'),
        limit = limit_per_query
      )
    }, error = function(e) {
      warning("Error fetching NA values: ", e$message)
      return(NULL)
    })

    if (!is.null(na_results) && nrow(na_results) > 0) {
      all_results <- c(all_results, list(na_results))
      total_fetched <- total_fetched + nrow(na_results)
    }

    message(sprintf("Fetched %d records with missing dates.", ifelse(is.null(na_results), 0, nrow(na_results))))
  }

  # Combine all results into a single tibble
  combined_results <- bind_rows(all_results) %>%
    distinct()  # Remove duplicates

  message(sprintf("Fetching completed. Total records fetched: %d", total_fetched))
  return(combined_results)
}
