#' @title Fetch Finna Collection Data with Flexible Query
#'
#' @description
#' This function retrieves data from the Finna API and formats it as a tidy tibble.
#'
#' @param query The query string for filtering results. Defaults to NULL, which fetches data without a specific search term.
#' @param limit Maximum number of results to fetch. Defaults to 0.
#' @param facets Facet to retrieve, defaults to "building".
#' @param lng Language for results, defaults to "fi".
#' @param prettyPrint Logical, whether to pretty-print JSON responses.
#' @return A tibble containing the fetched data with relevant fields.
#'
#' @examples
#' \dontrun{
#'   fetch_finna(query = "record_format:ead", limit = 0)
#'   fetch_finna() # Fetches data with no specific query
#' }
#' @export
fetch_finna <- function(query = NULL, limit = 0, facets = "building", lng = "fi", prettyPrint = TRUE) {


  if (!is.null(query) && query == "") {
    stop("Invalid query: Query string cannot be empty.")
  }

  # Start the timer
  start_time <- Sys.time()

  base_url <- "https://api.finna.fi/v1/search"

  # Build query parameters
  query_params <- list(
    type = "AllFields",
    limit = limit,
    `facet[]` = facets,
    lng = lng,
    prettyPrint = prettyPrint
  )

  # Add query parameter only if it is not NULL
  if (!is.null(query)) {
    query_params$lookfor <- query
  }

  # Perform the GET request
  response <- httr::GET(base_url, query = query_params)

  # Check response status
  if (httr::status_code(response) != 200) {
    stop("API request failed. Status code: ", httr::status_code(response))
  }

  # Parse the response JSON
  json_data <- httr::content(response, as = "parsed")
  #print(json_data)

  # Extract the result count
  result_count <- json_data$resultCount

  # Extract and process the building facet data
  building_data <- json_data$facets$building
  buildings_df <- if (!is.null(building_data)) {
    tibble::tibble(
      value = sapply(building_data, `[[`, "value"),
      translated = sapply(building_data, `[[`, "translated"),
      count = sapply(building_data, `[[`, "count"),
      href = sapply(building_data, `[[`, "href")
    )
  } else {
    tibble::tibble()
  }

  # End the timer
  end_time <- Sys.time()
  time_taken <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # Print summary messages
  message(sprintf("Total results found: %d", result_count))
  message(sprintf("Data fetching completed in %.2f seconds.", time_taken))

  # Return the tibble
  buildings_df
}
