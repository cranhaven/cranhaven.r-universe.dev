#' @title Finna Publisher Search
#'
#' @description
#' This function retrieves only the publisher information from the Finna index based on the search query.
#'
#' @name search_publisher
#' @param query A string specifying the search query.
#' @param limit An integer specifying the total number of records to return.
#' @param lng A string for the language of returned translated strings. Defaults to "fi".
#' @param filters A vector of filter queries to refine the search. Defaults to NULL.
#' @param prettyPrint A logical value indicating whether to pretty-print the JSON response. Defaults to FALSE.
#' @return A tibble containing the record IDs and their respective publishers.
#'
#' @examples
#' \dontrun{
#' publishers <- search_publisher("sibelius", limit = 10)
#' print(publishers)
#' }
#' @export
search_publisher <- function(query = NULL,
                             limit = 100,
                             lng = "fi",
                             filters = NULL,
                             prettyPrint = FALSE) {

  # Handle empty search queries
  if (is.null(query) || query == "") {
    warning("Empty search query provided.")
    return(NULL)
  }

  # Define the base URL for the Finna search API
  base_url <- "https://api.finna.fi/v1/search"

  # Initialize list to store publisher records
  publisher_data <- list()

  # Pagination parameters
  total_fetched <- 0
  page <- 1
  records_per_page <- 100  # Max records per page

  # Fetch data page by page
  while (total_fetched < limit) {
    # Determine how many records to fetch in this request
    remaining_to_fetch <- min(records_per_page, limit - total_fetched)

    # Construct the query parameters for publisher search
    query_params <- list(
      lookfor = query,
      `field[]` = "publishers",  # Only return publishers
      page = page,
      limit = remaining_to_fetch,
      lng = lng,
      prettyPrint = prettyPrint
    )

    # Execute the GET request
    response <- httr::GET(base_url, query = query_params)

    # Check for a successful response
    if (httr::status_code(response) == 200) {
      # Parse the JSON content
      publisher_results <- httr::content(response, "parsed")
      #print(publisher_results)

      # Extract publisher records
      records <- publisher_results$records
      #print(records)
      if (is.null(records) || length(records) == 0) {
        break  # No more records, stop fetching
      }

      # Process publisher data
      data <- lapply(records, function(record) {
        list(
          id = record$id %||% NA,
          Publisher = if (!is.null(record$publishers) && length(record$publishers) > 0) {
            paste(record$publishers, collapse = ", ")
          } else {
            NA
          }
        )
      })

      # Add to the publisher data list
      publisher_data <- c(publisher_data, data)

      # Update the total fetched and page number
      total_fetched <- total_fetched + length(records)
      page <- page + 1

    } else {
      # Handle API errors with a warning
      error_message <- sprintf("Failed to fetch publisher records. Status code: %d", httr::status_code(response))
      warning(error_message)
      return(NULL)
    }
  }

  # Convert the list of publisher data to a tibble for easy manipulation
  publisher_tibble <- tibble::as_tibble(do.call(rbind, lapply(publisher_data, function(x) unlist(x, recursive = FALSE))))

  return(publisher_tibble)
}
