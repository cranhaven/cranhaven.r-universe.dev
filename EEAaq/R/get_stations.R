#'@title Get Station Data
#'@description
#' This function downloads detailed information for each SamplingPointId. It performs a spatial join to merge the spatial information of LAU and NUTS (specifically, the geometries of LAU and the geometry of stations) and fills in the missing data for CITY_NAME and CITY_ID (retrieved from https://discomap.eea.europa.eu/App/AQViewer/index.html?fqn=Airquality_Dissem.b2g.AirQualityStatistics) through a left join based on the AirQualityStationEoICode column. These values are essential for querying the endpoint.
#' The missing_cities file was obtained manually (from 2000 to 2024) because the website did not allow downloading more than 100,000 rows at a time. The data was collected in multiple batches, filtering SamplingPoints using the following criteria:
#' - Filter on data used in AQ Report: yes
#' - Filter on data coverage: yes
#' For each station, the column AirQualityStationEoICode (identical for all sensors at the same station) was used to select the first row containing unique values for CITY_NAME and CITY_ID. No station reported more than one value for this pair of columns.
#' To support future uploads, it is necessary to integrate updated AirQualityStationEoICode values.
#' @return a tibble

get_stations <- function() {
  `%>%` <- dplyr::`%>%`

  # Check internet connection
  if (!curl::has_internet()) {
    stop("Please check your internet connection. If the problem persists, please contact the package maintainer.")
  }

  # URL del file ZIP
  url <- "https://discomap.eea.europa.eu/App/AQViewer/download?fqn=Airquality_Dissem.b2g.measurements&f=csv"

  # Directory temporanea per il download
  temp_dir <- "C:/Temp"
  if (!base::dir.exists(temp_dir)) {
    base::dir.create(temp_dir, showWarnings = FALSE)
  }

  # Percorso per salvare il file ZIP
  temp_zip <- base::tempfile(tmpdir = temp_dir, fileext = ".zip")

  # Scarica il file ZIP
  curl::curl_download(url, temp_zip)
  if (!base::file.exists(temp_zip)) {
    stop("Il file ZIP temporaneo non e stato creato correttamente.")
  }

  # Crea una directory per estrarre i file
  extract_dir <- base::file.path(temp_dir, "extracted_data")
  if (!base::dir.exists(extract_dir)) {
    base::dir.create(extract_dir, showWarnings = FALSE)
  }

  # Estrai il contenuto del file ZIP
  utils::unzip(temp_zip, exdir = extract_dir)

  # Trova il file CSV estratto
  csv_file <- base::list.files(extract_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(csv_file) == 0) {
    stop("Non e stato trovato alcun file CSV nel file ZIP.")
  }

  # Leggi il file CSV
  data <- readr::read_csv(csv_file[1], col_types = readr::cols(.default = readr::col_character()))
  colnames(data) <- base::gsub(" ", "", colnames(data))
  # Rimuoviamo duplicati
  data <- dplyr::distinct(data)

  # Livello 0: Stato (prime due lettere del codice NUTS)
  # Livello 1: Macro zone
  # Livello 2: Regioni
  # Livello 3: Province

  Nuts <- EEAaq_get_dataframe(dataframe = "NUTS")
  levels <- list(
    nuts0 = Nuts %>% base::as.data.frame() %>% dplyr::filter(.data$LEVL_CODE == 0) %>% dplyr::select(.data$NUTS_ID,.data$NAME_LATN) %>% dplyr::rename(NUTS0_ID = .data$NUTS_ID, NUTS0 = .data$NAME_LATN),
    nuts1 = Nuts %>% base::as.data.frame() %>% dplyr::filter(.data$LEVL_CODE == 1) %>% dplyr::select(.data$NUTS_ID, .data$NAME_LATN) %>% dplyr::rename(NUTS1_ID = .data$NUTS_ID, NUTS1 = .data$NAME_LATN),
    nuts2 = Nuts %>% base::as.data.frame() %>% dplyr::filter(.data$LEVL_CODE == 2) %>% dplyr::select(.data$NUTS_ID, .data$NAME_LATN) %>% dplyr::rename(NUTS2_ID = .data$NUTS_ID, NUTS2 = .data$NAME_LATN),
    nuts3 = Nuts %>% base::as.data.frame() %>% dplyr::filter(.data$LEVL_CODE == 3) %>% dplyr::select(.data$NUTS_ID, .data$NAME_LATN) %>% dplyr::rename(NUTS3_ID = .data$NUTS_ID, NUTS3 = .data$NAME_LATN)
  )

  Nuts_join <- levels$nuts0 %>%
    dplyr::left_join(levels$nuts1 %>% dplyr::mutate(NUTS0_ID = base::substr(.data$NUTS1_ID, 1, 2)), by = c("NUTS0_ID" = "NUTS0_ID")) %>%
    dplyr::left_join(levels$nuts2 %>% dplyr::mutate(NUTS1_ID = base::substr(.data$NUTS2_ID, 1, 3)), by = c("NUTS1_ID" = "NUTS1_ID")) %>%
    dplyr::left_join(levels$nuts3 %>% dplyr::mutate(NUTS2_ID = base::substr(.data$NUTS3_ID, 1, 4)), by = c("NUTS2_ID" = "NUTS2_ID"))

  Lau <- EEAaq_get_dataframe(dataframe = "LAU")

  Lau_nuts <- Lau %>%
    dplyr::full_join(Nuts_join, by = c("NUTS3_ID" = "NUTS3_ID"))

  # Convert data to spatial object
  data <- data %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

  # Check if all geometries in Lau_nuts are valid
  invalid_geometries <- Lau_nuts %>% dplyr::filter(!sf::st_is_valid(.data$Lau_geometry))

  # Print invalid geometries with Lau_name and Lau_id
  if (base::nrow(invalid_geometries) > 0) {
    base::cat("The following geometries are invalid and will be removed:\n")
    base::print(invalid_geometries %>% dplyr::select("ISO","LAU_NAME", "LAU_ID"))
  }

  # Filter to only valid geometries
  Lau_nuts <- Lau_nuts %>%
    dplyr::filter(sf::st_is_valid(.data$Lau_geometry))%>%
    dplyr::select(-.data$POP_2021, -.data$POP_DENS_2, -.data$AREA_KM2, -.data$YEAR, -.data$GISCO_ID)


  # Perform spatial join if all geometries are valid
  stations <- data %>%
    sf::st_join(Lau_nuts, join = sf::st_within) %>%
    dplyr::mutate(
      Longitude = sf::st_coordinates(.)[, 1],
      Latitude = sf::st_coordinates(.)[, 2]
    ) %>%
    dplyr::relocate(.data$Longitude, .data$Latitude, .before = .data$Altitude) %>%
    sf::st_drop_geometry()



  # get missing cities dataframe
  temp <- tempfile()
  res <- curl::curl_fetch_disk("https://github.com/PaoloMaranzano/EEAaq_R_Support/raw/refs/heads/main/missing_cities.rds", temp)
  if (res$status_code == 200) {
    missing_cities <- readr::read_rds(temp)  # Usa 'load()' per file .rda

  } else {
    stop("The internet resource is not available at the moment, try later.
       If the problem persists, please contact the package maintainer.")
  }

  stations <- stations %>%
    dplyr::left_join(missing_cities, by = c("AirQualityStationEoICode", "AirQualityNetwork")) %>%
    dplyr::rename(CITY_NAME = .data$City, CITY_ID = .data$CityCode)

  #rimuoviamo spazi vuoti e convertiamo stringhe vuote in na
  stations <- stations %>%
    dplyr::mutate(dplyr::across(
      # tranne geometry
      .cols = dplyr::where(is.character),
      .fns = ~ {
        .x <- stringr::str_trim(.x)
        ifelse(.x == "", NA, .x)
      }
    ))


  station_nuts3_na <- stations[is.na(stations$NUTS3_ID), ]
  station_valid <- stations[!is.na(stations$NUTS3_ID), ]

  # Popola CITY_NAME e CITY_ID per righe valide
  station_valid <- station_valid %>%
    dplyr::group_by(.data$NUTS3_ID) %>%
    dplyr::mutate(
      CITY_NAME = ifelse(is.na(CITY_NAME), unique(stats::na.omit(.data$CITY_NAME))[1], .data$CITY_NAME),
      CITY_ID = ifelse(is.na(CITY_ID), unique(stats::na.omit(.data$CITY_ID))[1], .data$CITY_ID)
    ) %>%
    dplyr::ungroup()

  # Combina le due parti
  stations <- dplyr::bind_rows(station_valid, station_nuts3_na)

  stations <- stations %>%
    dplyr::mutate(SamplingPointId = paste0(.data$ISO, "/", .data$SamplingPointId)) %>%
    dplyr::distinct(.data$SamplingPointId, .keep_all = TRUE)

  return(stations)
}

