# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

#' Get South Korea's Consumer Price Index (2010 = 100) from World Bank
#'
#' @description
#' Retrieves South Korea's Consumer Price Index (CPI), with 2010 as the base year (index = 100),
#' for the years 2010 to 2022 using the World Bank Open Data API.
#' The indicator used is \code{FP.CPI.TOTL}.
#'
#' @return A tibble with the following columns:
#' \itemize{
#'   \item \code{indicator}: Indicator name (e.g., "Consumer price index (2010 = 100)")
#'   \item \code{country}: Country name ("Korea, Rep.")
#'   \item \code{year}: Year of the data (integer)
#'   \item \code{value}: Consumer Price Index (numeric, base year 2010 = 100)
#' }
#'
#' @details
#' This function sends a GET request to the World Bank API.
#' If the API request fails or returns an error status code,
#' the function returns \code{NULL} with an informative message.
#'
#' @note Requires internet connection.
#'
#' @source World Bank Open Data API: \url{https://data.worldbank.org/indicator/FP.CPI.TOTL}
#'
#' @examples
#' if (interactive()) {
#'   get_southkorea_cpi()
#' }
#'
#' @seealso \code{\link[httr]{GET}}, \code{\link[jsonlite]{fromJSON}}, \code{\link[dplyr]{as_tibble}}
#'
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#'
#' @export
get_southkorea_cpi <- function() {
  url <- "https://api.worldbank.org/v2/country/KOR/indicator/FP.CPI.TOTL?format=json&date=2010:2022&per_page=100"
  res <- httr::GET(url)
  if (res$status_code != 200) {
    message(paste("Error: status", res$status_code))
    return(NULL)
  }
  content <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
  if (length(content) < 2 || is.null(content[[2]])) {
    message("No data returned from the World Bank API.")
    return(NULL)
  }
  data <- content[[2]]
  df <- dplyr::as_tibble(data.frame(
    indicator = data$indicator$value,
    country = data$country$value,
    year = as.integer(data$date),
    value = data$value
  ))
  return(df)
}
