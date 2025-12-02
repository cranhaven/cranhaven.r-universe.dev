#'@title Get pollutant
#'
#' @description Retrieve Pollutant Data from EEA Vocabulary (https://dd.eionet.europa.eu/vocabulary/aq/pollutant)
#' Downloads and processes pollutant data from the EEA (European Environment Agency) vocabulary database.
#' The data includes relevant information such as pollutant names, codes, and descriptions.
#'
#' @return A tibble containing pollutant information with selected columns (e.g., URI, notation, and extracted code).
#'
get_pollutants<- function() {
  `%>%` <- dplyr::`%>%`

  # Check internet connection
  if (!curl::has_internet()) {
    stop("Please check your internet connection. If the problem persists, please contact the package maintainer.")
  }

  url <- "https://dd.eionet.europa.eu/vocabulary/aq/pollutant/csv"

  # Scarica il file in una directory temporanea
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file), add = TRUE)

  curl::curl_download(url, temp_file)

  pollutant <- readr::read_csv(temp_file, col_types = readr::cols(.default = readr::col_character()), name_repair = "minimal") %>%  dplyr::select(1:6)
  pollutant <- pollutant %>% dplyr::mutate(Code = stringr::str_extract(.data$URI, "\\d+$"))
  pollutant <- pollutant %>%   dplyr::mutate(RecommendedUnit = stringr::str_trim(stringr::str_extract(.data$Definition, "(?<=recommended unit:).*")))

  return(pollutant)
}
