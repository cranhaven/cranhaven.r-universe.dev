#'@title Get NUTS
#'
#'@description It automatically updates the dataset by identifying the most recent available file, accessing the corresponding page, and downloading the SHP file  at the 1:20 Million scale with the EPSG:4326 reference system from this website (https://gisco-services.ec.europa.eu/distribution/v2/nuts/)
#'@param year expressed as four digit (YYYY)
#' @return A tibble containing  LAUs  information with selected columns (NUTS_ID, LEVL_CODE...)
#'

get_NUTS <- function(year= "Null") {
  `%>%` <- dplyr::`%>%`
  # Check internet connection
  if (!curl::has_internet()) {
    stop("Please check your internet connection. If the problem persists, please contact the package maintainer.")
  }
  if (missing(year)) {
    stop("The 'year' parameter is required and must be provided as a 4-digit character string.")
  }
  if (!is.character(year) || nchar(year) != 4) {
    stop("The 'year' parameter must be a 4-digit character string")

  }
  year <- substr(year, 3, 4)
  base_url <- "https://gisco-services.ec.europa.eu/distribution/v2/nuts/"
  page <- rvest::read_html(base_url)
  #estraiamo links
  links <- page %>% rvest::html_nodes("a") %>% rvest::html_attr("href")
  #filtra link che si riferiscono al zip. file
  zip_links <-  links[grepl("download#nuts", links)]

  # Trova l'anno piUù recente disponibile

  #latest_year <-  max(as.numeric(gsub("\\D", "", zip_links)))
  #if (is.na(latest_year)) stop("No year detected in available links")
  download_link <- paste0(base_url, "download/#nuts", year)

  # acceddiamo pagina anno piu recente
  page1 <- rvest::read_html(download_link)
  #cat(as.character(page))

  #troviamo link  tr (riga) all'interno tabella . dopo di che selezionaniamo tutte righe nutrs che contengno 1:20m [1]
  shp_link <- page1 %>%
    rvest::html_nodes(
      xpath = paste0("//tr[td/a[@name='nuts", year, "']]//td[a[contains(@href, '.shp.zip') and contains(@href, '01m')]]/a[@href]")
    ) %>%
    rvest::html_attr("href")


  temp_file <- tempfile(fileext = ".zip")

  temp_dir <- tempdir()
  on.exit({
    unlink(temp_file)
    unlink(temp_dir, recursive = TRUE)
  }, add = TRUE)

  curl::curl_download(shp_link, temp_file)
  #list.files(temp_dir, full.names = TRUE)
  utils::unzip(temp_file, exdir = temp_dir)
  shp_file <- list.files(temp_dir, pattern = paste0("NUTS_RG_01M_20", year, "_4326\\.shp.zip$"), full.names = TRUE)

  #read file
  if (length(shp_file) == 1) {
    NUTS <- sf::st_read(shp_file, quiet= TRUE)
  } else {
    stop("Error: SHP file not found or multiple files detected.")
  }




  return(NUTS)
}
