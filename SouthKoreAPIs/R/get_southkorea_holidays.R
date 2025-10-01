# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.


#' Get Official Public Holidays in South Korea for a Given Year
#'
#' @description
#' Retrieves the list of official public holidays in South Korea for a specific year
#' using the Nager.Date public holidays API.
#' This function returns a tibble containing the date of the holiday, the name
#' in the local language (Korean), and the English name.
#' It is useful for academic, planning, and data analysis purposes.
#' The information is retrieved directly from the Nager.Date API and reflects
#' the current status of holidays for the requested year.
#' The field names returned are consistent with the API structure.
#'
#' @param year An integer indicating the year (e.g., 2024 or 2025).
#'
#' @return A tibble with the following columns:
#' \itemize{
#'   \item \code{date}: Date of the holiday (class \code{Date})
#'   \item \code{local_name}: Holiday name in the local language (Korean)
#'   \item \code{name}: Holiday name in English
#' }
#'
#' @source Data obtained from the Nager.Date API: \url{https://date.nager.at/}
#'
#' @examples
#' get_southkorea_holidays(2024)
#' get_southkorea_holidays(2025)
#'
#' @importFrom httr GET status_code content
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#'
#' @export
get_southkorea_holidays <- function(year) {
  url <- sprintf("https://date.nager.at/api/v3/PublicHolidays/%s/KR", year)
  response <- httr::GET(url)
  if (httr::status_code(response) != 200) {
    stop("Failed to retrieve data from Nager.Date API. Check the year or try again later.")
  }
  data <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
  tibble::tibble(
    date = as.Date(data$date),
    local_name = data$localName,
    name = data$name
  )
}
