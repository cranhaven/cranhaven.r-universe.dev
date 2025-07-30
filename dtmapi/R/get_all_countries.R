library(httr2)
library(jsonlite)
library(magrittr)  # For the `%>%` operator

#' Fetch All Countries
#'
#' Retrieve all countries for which DTM data is publicly available through the API.
#'
#' @return A data frame containing the list of all countries.
#' @export
#' @examples
#' # Fetch all countries
#' countries_df <- get_all_countries()
#' head(countries_df)
#' @importFrom httr2 request req_perform resp_status resp_body_string
#' @importFrom magrittr %>%
#' @importFrom jsonlite fromJSON
get_all_countries <- function() {

  tryCatch({
    # Retrieve the API URL
    api_url <- "https://dtmapi.iom.int/api/Common/GetAllCountryList"

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
