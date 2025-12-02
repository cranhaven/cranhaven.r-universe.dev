#'@title Get LAU data
#'
#' @description Local Administrative Units (LAUs) are the building blocks of the NUTS classification and correspond to the municipalities and communes within the EU
#' To get the final dataframe we combine two dataset: one taken from Eurostat (https://ec.europa.eu/eurostat/web/nuts/local-administrative-units)that includes City names and City IDs, essential for querying and associations.
#' The other one taken from EEA which provides LAU information. The Latter dataset is updated automatically by selecting the most recent shapefile (SHP) available online.
#' While The Eurostat dataset URL needs to be manually updated with the latest download link to ensure the City-related data is current.
#'
#' @param year expressed as four digit (YYYY)
#' @return  A tibble containing  LAUs  information with selected columns (e.g., ISO, LAU_ID, NUTS3_ID  and geometry ).
#'

get_LAU <- function(year= "Null") {

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
  download_lau <- function (){
  base_url <- "https://gisco-services.ec.europa.eu/distribution/v2/lau/"
  page <- rvest::read_html(base_url)
  #estraiamo links
  links <- page %>% rvest::html_nodes("a") %>% rvest::html_attr("href")
  #filtra link che si riferiscono al zip. file
  zip_links <-  links[grepl("download#lau", links)]

  # Trova anno piu recente disponibile
  #latest_year <- max(as.numeric(gsub("\\D", "", zip_links)))
  #if (is.na(latest_year)) stop("No year detected in available links")
  download_link <- paste0(base_url, "download/#lau", year)

  # acceddiamo pagina anno piu recente
  page1 <- rvest::read_html(download_link)
  #cat(as.character(page))

  #troviamo link
  shp_link <- page1 %>%
    rvest::html_nodes(
      xpath = paste0("//tr[td/a/@name='lau", year,"']//a[contains(@href, '.shp.zip')]"))   %>% rvest::html_attr("href")

  ## Verifica che il link sia stato trovato e crea il link completo
  if (length(shp_link) == 0) {
    stop("File SHP EPSG:4326 not found for year ", year)
  }
  shp_url <- paste0(base_url, shp_link)

  #temp_file <- tempfile(fileext = ".zip")
  #temp_dir <- tempdir()
  temp_dir <- "C:/Temp"
  dir.create(temp_dir, showWarnings = FALSE)
  temp_file <- tempfile(tmpdir = temp_dir, fileext = ".zip")

  on.exit({
    unlink(temp_file)
    unlink(temp_dir, recursive = TRUE)
  }, add = TRUE)
  #print(shp_link)
  #Download file
  curl::curl_download(shp_link, temp_file)


  utils::unzip(temp_file, exdir = temp_dir)
  shp_zip_file <- list.files(temp_dir, pattern = "4326\\.shp\\.zip$", full.names = TRUE)

  utils::unzip(shp_zip_file, exdir = temp_dir)
  shp_file <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
  # Check if exactly one .shp file was found
  #print(shp_file)
  if (length(shp_file) == 1) {
    Lau <- sf::st_read(shp_file, quiet= TRUE)  %>%
      dplyr::mutate(LAU_ID = gsub("^0+", "", LAU_ID)) #rimuove spazi iniziali e finali
  } else {
    stop("Error: SHP file not found or multiple files detected.")
  }
  #rename column
  Lau <-  Lau %>%
    dplyr::rename(Lau_geometry = geometry )
  return(Lau)
  }


  download_lau_city <- function() {

  url2 <- "https://ec.europa.eu/eurostat/documents/345175/501971/EU-27-LAU-2023-NUTS-2021.xlsx/40820469-1dad-6ec5-2318-46ea2993a1cf?t=1708527769154"
  #temp_file <- tempfile(fileext = ".xlsx")

  temp_dir <- "C:/Temp"
  dir.create(temp_dir, showWarnings = FALSE)
  temp_file <- tempfile(tmpdir = temp_dir, fileext = ".xlsx")

  curl::curl_download(url2, temp_file)

  on.exit({
    unlink(temp_file)
  }, add = TRUE)

  sheet_names <-suppressMessages(readxl::excel_sheets(path = temp_file)[-c(1, 2, 3)])
  data_frames <- list()
  for (sheet_name in sheet_names) {
    df <- suppressMessages(readxl::read_xlsx(temp_file, sheet = sheet_name, col_types = "text")) %>%
      dplyr::select(-dplyr::starts_with("...")) %>%  # Rimuove colonne senza nome
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) # Converte tutte le colonne in caratteri
    data_frames[[sheet_name]] <- df # Aggiungi il data frame alla lista con il nome del foglio come chiave
  }

  # Usa `do.call` con `bind_rows` per unire i dataframe nella lista `data_frames`
  df_city <- do.call(dplyr::bind_rows, data_frames)
  colnames(df_city) <- gsub(" ", "_", colnames(df_city))
  df_city <- df_city %>%
    dplyr::mutate(LAU_CODE = gsub("^0+", "", .data$LAU_CODE))
  if (!"NUTS_3_CODE" %in% colnames(df_city)) {
    stop("Error: Column NUTS_3_CODE is not present in the data")
  }
#  df_city <-  df_city %>%
#    dplyr::rename(NUTS3_ID = NUTS_3_CODE)
 # df_city$CNTR_CODE<- substr(df_city$NUTS3_ID, 1, 2)
  df_city <- df_city %>%
    dplyr::rename(NUTS3_ID = .data$NUTS_3_CODE) %>%
    dplyr::mutate(CNTR_CODE = substr(.data$NUTS3_ID, 1, 2))

  return(df_city)
  }

  # Esecuzione
  Lau <- download_lau()
  df_city <- download_lau_city()

  LAU <- Lau %>%  dplyr::full_join(df_city %>% dplyr::select("LAU_CODE", "NUTS3_ID","CNTR_CODE"),
                                by= c('LAU_ID'='LAU_CODE', 'CNTR_CODE'= 'CNTR_CODE' ))  %>%  dplyr::rename(ISO = .data$CNTR_CODE)



  return(LAU)
}
