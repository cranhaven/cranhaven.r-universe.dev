# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.


#' Get Country Information for South Korea
#'
#' @description
#' Retrieves comprehensive country information for South Korea from the REST Countries API.
#' This function fetches data including official and common names, geographical information,
#' capital, area, population, and languages.
#'
#' @return A tibble with one row containing South Korea's country information, or NULL if the API is unavailable.
#'
#' @examples
#' \donttest{
#' sk_info <- get_country_info_kr()
#' print(sk_info)
#' }
#'
#' @importFrom httr GET http_error content
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#'
#' @export
get_country_info_kr <- function() {
  url <- "https://restcountries.com/v3.1/name/south%20korea?fullText=true"

  # Intentar obtener los datos y capturar errores de conexión
  response <- tryCatch(
    httr::GET(url),
    error = function(e) {
      message("No se pudo conectar a restcountries.com: ", e$message)
      return(NULL)
    }
  )

  if (is.null(response)) return(NULL)  # Si la conexión falla, retornar NULL

  # Verificar código HTTP
  if (httr::http_error(response)) {
    message("API request failed with status code: ", httr::status_code(response))
    return(NULL)
  }

  # Convertir a texto y parsear JSON
  data_raw <- httr::content(response, as = "text", encoding = "UTF-8")

  data_list <- tryCatch(
    jsonlite::fromJSON(data_raw),
    error = function(e) {
      message("Error al parsear JSON: ", e$message)
      return(NULL)
    }
  )

  if (is.null(data_list) || length(data_list) == 0) {
    message("No data found for South Korea.")
    return(NULL)
  }

  # Tomar solo el primer país
  data <- data_list[1, ]

  tibble::tibble(
    name_common   = data$name$common,
    name_official = data$name$official,
    region        = data$region,
    subregion     = data$subregion,
    capital       = paste(data$capital, collapse = ", "),
    area          = data$area,
    population    = data$population,
    languages     = paste(unlist(data$languages), collapse = ", ")
  )
}
