#' Validate query parameters
#'
#' @param params List of query parameters
#' @return Logical indicating if parameters are valid
#' @keywords internal
validate_params <- function(params) {
  required <- c("wt", "cursorMark", "sort")
  missing <- required[!required %in% names(params)]

  if (length(missing) > 0) {
    stop(sprintf("Missing required parameters: %s",
                 paste(missing, collapse = ", ")))
  }

  TRUE
}

#' Clean text for URL encoding
#'
#' @param text Text string to clean
#' @return URL-encoded string
#' @keywords internal
clean_query_text <- function(text) {
  if (is.null(text)) return(NULL)
  else {
    new_text <- gsub(" ", "%20", text)
    return(new_text)
  }
}

#' Parse API response
#'
#' @param response Raw API response
#' @return Parsed response object
#' @keywords internal
parse_response <- function(response) {
  if (httr::http_error(response)) {
    stop(
      sprintf(
        "API request failed [%s]\n%s",
        httr::status_code(response),
        httr::content(response, "text")
      )
    )
  }

  httr::content(response, "parsed")
}

#' Convert nested lists to data frame
#'
#' @param results List of API results
#' @return data.frame
#' @keywords internal
flatten_results <- function(results) {
  df <- data.table::rbindlist(
    lapply(results, function(x) {
      # Convert all list elements to character
      lapply(x, function(y) {
        if (is.list(y)) paste(unlist(y), collapse = "|") else as.character(y)
      })
    }),
    fill = TRUE
  )

  return(df)
}
