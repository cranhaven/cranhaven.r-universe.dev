library(httr2)
library(jsonlite)
library(magrittr)  # For the `%>%` operator

#' Fetch All Operations
#'
#' Retrieve all operations for which DTM data is publicly available through the API.
#'
#' @return A data frame containing the list of all operations.
#' @export
#' @examples
#' # Fetch all operations
#' operations_df <- get_all_operations()
#' head(operations_df)
#' @importFrom httr2 request req_perform resp_status resp_body_string
#' @importFrom magrittr %>%
#' @importFrom jsonlite fromJSON
get_all_operations <- function() {

  tryCatch({
    # Load configuration
    api_url <- "https://dtmapi.iom.int/api/Common/GetAllOperationList"

    # Send GET request to the API using httr2
    response <- request(api_url) %>% req_perform()

    # Check if the request was successful
    if (resp_status(response) != 200) {
      stop("Failed to fetch data. Status code: ", resp_status(response))
    }

    # Parse the JSON content and extract the result as a data frame
    data <- resp_body_string(response)
    df <- fromJSON(data, flatten = TRUE)$result

    # Return the data frame
    return(df)

  }, error = function(e) {
    # Handle and report errors
    stop("API request failed: ", e$message)
  })
}
