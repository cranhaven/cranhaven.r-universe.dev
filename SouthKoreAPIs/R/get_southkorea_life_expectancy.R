# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.


#' Get South Korea's Life Expectancy at Birth (Total, Years) from World Bank
#'
#' @description
#' Retrieves South Korea's life expectancy at birth (total, years)
#' for the years 2010 to 2022 using the World Bank Open Data API.
#' The indicator used is \code{SP.DYN.LE00.IN}.
#'
#' @return A tibble with the following columns:
#' \itemize{
#'   \item \code{indicator}: Indicator name (e.g., "Life expectancy at birth, total (years)")
#'   \item \code{country}: Country name ("Korea, Rep.")
#'   \item \code{year}: Year of the data (integer)
#'   \item \code{value}: Life expectancy at birth in years
#' }
#'
#' @details
#' This function sends a GET request to the World Bank API.
#' If the API request fails or returns an error status code,
#' the function returns \code{NULL} with an informative message.
#'
#' @note Requires internet connection.
#'
#' @source World Bank Open Data API: \url{https://data.worldbank.org/indicator/SP.DYN.LE00.IN}
#'
#' @examples
#' if (interactive()) {
#'   get_southkorea_life_expectancy()
#' }
#'
#' @seealso \code{\link[httr]{GET}}, \code{\link[jsonlite]{fromJSON}}, \code{\link[dplyr]{as_tibble}}
#'
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#'
#' @export
get_southkorea_life_expectancy <- function() {
  url <- "https://api.worldbank.org/v2/country/KOR/indicator/SP.DYN.LE00.IN?format=json&date=2010:2022&per_page=100"
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
