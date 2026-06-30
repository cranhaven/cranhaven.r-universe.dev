#' @title Get Finna Records by IDs with Extended Options
#'
#' @description
#' This function retrieves multiple Finna records based on a vector of record IDs. You can specify
#' which fields to return, the language, and the pagination options.
#'
#' @name get_finna_records
#' @param ids A vector of record IDs to retrieve.
#' @param field A vector of fields to return. Defaults to NULL, which returns all default fields.
#' @param prettyPrint Logical; whether to pretty-print the response. Defaults to FALSE.
#' @param lng Language for returned translated strings. Defaults to "fi".
#' @param page The page number to retrieve. Defaults to 1.
#' @param limit The number of records to return per page. Defaults to 20.
#' @return A tibble containing the retrieved records data with provenance information.
#'
#' @importFrom stats reorder setNames
#' @examples
#' \dontrun{
#' records <- get_finna_records("fikka.3405646", field = "title", prettyPrint = TRUE, lng = "en-gb")
#' print(records)
#' }
#' @export
#' @importFrom httr GET status_code content

get_finna_records <- function(ids, field = NULL, prettyPrint = FALSE, lng = "fi", page = 1, limit = 100) {

  # Validate language code
  valid_languages <- c("fi", "sv", "en-gb", "en-us") # Define valid language codes
  if (!lng %in% valid_languages) {
    stop("Invalid language code: ", lng)
  }

  # Base URL for the record API
  base_url <- "https://api.finna.fi/v1/record"

  # Set query parameters with the provided IDs and options
  query_params <- list(
    #`id[]` = ids,
    prettyPrint = prettyPrint,
    lng = lng,
    page = page,
    limit = limit
  )

  # Add ID parameters
  query_params <- c(query_params, setNames(as.list(ids), rep("id[]", length(ids))))

  # Add fields to query parameters if specified
  if (!is.null(field)) {
    query_params <- c(query_params, setNames(as.list(field), rep("field[]", length(field))))
  }

  # Make the GET request
  response <- tryCatch(
    GET(base_url, query = query_params),
    error = function(e) {
      stop("Failed to make the request: ", e$message)
    }
  )
  #print(response)

  # Check the response status
  if (status_code(response) == 200) {
    record_data <- content(response, "parsed")
    if (is.null(record_data$records) || length(record_data$records) == 0) {
      stop("No records found for the provided IDs.")
    }
    #print(record_data)


    # Extract the data
    data <- lapply(record_data$records, function(record) {
      if (is.list(record)) {
        list(
          id = if (!is.null(record$id)) record$id else NA,
          Title = if (!is.null(record$title)) record$title else NA,
          Author = if (!is.null(record$nonPresenterAuthors)) {
            paste(sapply(record$nonPresenterAuthors, function(author) {
              if (!is.null(author$name)) author$name else NA
            }), collapse = ", ")
          } else {
            NA
          },
          Year = if (!is.null(record$year)) record$year else NA,
          Language = if (!is.null(record$languages) && length(record$languages) > 0) {
            record$languages[[1]]
          } else {
            NA
          },
          Publisher = if(!is.null(record$publisher)) record$publisher else NA,
          Formats = if (!is.null(record$formats) && length(record$formats) > 0) {
            paste(unlist(lapply(record$formats, function(format) {
              if (is.list(format) && !is.null(format$translated)) format$translated else NA
            })), collapse = ", ")
          } else {
            NA
          },
          Subjects = if (!is.null(record$subjects) && length(record$subjects) > 0) {
            paste(unlist(record$subjects), collapse = "; ")
          } else {
            NA
          },
          Library = if (!is.null(record$buildings) && length(record$buildings) > 0) {
            paste(sapply(record$buildings, function(building) {
              if (is.list(building) && !is.null(building$translated)) building$translated else NA
            }), collapse = "; ")
          } else {
            NA
          },
          Series = if (!is.null(record$series) && length(record$series) > 0) {
            paste(sapply(record$series, function(series) {
              if (!is.null(series$name)) series$name else NA
            }), collapse = ", ")
          } else {
            NA
          }
        )
      } else {
        list(
          id = NA,
          Title = NA,
          Author = NA,
          Year = NA,
          Language = NA,
          Publisher = NA,
          Formats = NA,
          Subjects = NA,
          Library = NA,
          Series = NA
        )
      }
    })

    # Convert the list to a tibble
    tibble_results <- tibble::as_tibble(do.call(rbind, lapply(data, function(x) unlist(x, recursive = FALSE))))
    # Add provenance and citation information
    #tibble_results$provenance <- "Finna API (https://www.finna.fi)"
    #tibble_results$data_license <- "CC0 for metadata (https://creativecommons.org/publicdomain/zero/1.0/), images and other linked resources may have different licenses."

    # Attach the citation as an attribute
    attr(tibble_results, "citation") <- "Data retrieved from Finna API (https://www.finna.fi) - metadata licensed under CC0."

    return(tibble_results)

  } else {
    stop("Failed to retrieve the records. Status code:", status_code(response),
         "- Response:", content(response, "text"))
  }
}



# Example usage:
#records <- get_finna_records(c("fikka.3405646"))
#print(records)
