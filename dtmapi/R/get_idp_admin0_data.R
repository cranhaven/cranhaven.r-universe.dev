library(httr2)
library(jsonlite)
library(magrittr)  # For the `%>%` operator

#' Fetch IDP Admin0 Data
#'
#' Retrieve IDP data at Admin 0 level based on specified parameters.
#' At least one of the following parameters must be provided: Operation, CountryName, or Admin0Pcode.
#'
#' @param Operation Optional; Name of the DTM operation for which the data was collected.
#' @param CountryName Optional; Name of the country where the data was collected.
#' @param Admin0Pcode Optional; Country code (ISO 3166-1 alpha-3).
#' @param FromReportingDate Optional; Start date for the reporting period (format: 'YYYY-MM-DD').
#' @param ToReportingDate Optional; End date for the reporting period (format: 'YYYY-MM-DD').
#' @param FromRoundNumber Optional; Starting round number for the data collection range.
#' @param ToRoundNumber Optional; Ending round number for the data collection range.
#' @return A data frame containing the IDP Admin0 data matching the specified criteria.
#' @export
#' @examples
#' # Fetch IDP data at Admin Level 0
#' idp_admin0_df <- get_idp_admin0_data(CountryName='Ethiopia', FromRoundNumber=1, ToRoundNumber=10)
#' head(idp_admin0_df)
#' @importFrom httr2 request req_perform req_url_query resp_status resp_body_string
#' @importFrom magrittr %>%
#' @importFrom jsonlite fromJSON
get_idp_admin0_data <- function(
    Operation = NULL,
    CountryName = NULL,
    Admin0Pcode = NULL,
    FromReportingDate = NULL,
    ToReportingDate = NULL,
    FromRoundNumber = 0,
    ToRoundNumber = 0
) {
  # Retrieve the API URL
  api_url <- "https://dtmapi.iom.int/api/idpAdmin0Data/GetAdmin0Datav2"

  # Set up query parameters
  params <- list(
    Operation = Operation,
    CountryName = CountryName,
    Admin0Pcode = Admin0Pcode,
    FromReportingDate = FromReportingDate,
    ToReportingDate = ToReportingDate,
    FromRoundNumber = FromRoundNumber,
    ToRoundNumber = ToRoundNumber
  )

  tryCatch({
    # Send GET request to the API with parameters using httr2
    response <- request(api_url) %>%
      req_url_query(!!!params) %>%
      req_perform()

    # Check if the request was successful
    if (resp_status(response) != 200) {
      stop("Failed to fetch data. Status code: ", resp_status(response))
    }

    # Parse the JSON content
    data <- resp_body_string(response, encoding = "UTF-8")
    json_data <- fromJSON(data, flatten = TRUE)

    # Check if the request was successful and extract the result
    if (json_data$isSuccess) {
      # Return the result as a data frame
      return(as.data.frame(json_data$result))
    } else {
      # Handle API-specific errors
      stop("API error: ", json_data$errorMessages[1])
    }

  }, error = function(e) {
    # Handle and report errors
    stop("API request failed: ", e$message)
  })
}
