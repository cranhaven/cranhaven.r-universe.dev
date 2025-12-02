#' Download air quality data at european level from the EEA download service
#'
#' This function retrieves air quality datasets at european level, based on station, time and pollutant specifications.
#' This function generates a \code{data.frame/tibble} object of class \code{EEAaq_df}.
#' @param IDstations Numeric value. Unique ID of the stations to retrieve.
#' @param pollutants the pollutants for which to download data. It may be:
#' \itemize{
#' \item{character vector representing the short names of the pollutants to analyse. The reference is the
#' variable \code{Notation} in the dataset \code{pollutants} provided by this package.}
#' \item{numeric vector representing the codes of the pollutants to analyse. The reference is the variable \code{Code}
#' in the dataset \code{pollutants} provided by this package.}
#' }
#' @param from character defining the initial date of the period to be retrieved. The format is \code{yyyy-mm-dd}.
#' @param to character defining the final date of the period to be retrieved. The format is \code{yyyy-mm-dd}.
#' @param verbose logic value (T or F). If \code{TRUE} (the default) information about the function progress are printed.
#' If \code{FALSE} no message is printed.
#' @details
#' Recall that stations and sensors are physically managed by national or local environmental protection agencies with their own specificities and rules.
#' EEA operates as a collector of national environmental protection systems and harmonizes the information received by national offices.
#' However, data provided can change on a country basis. For instance, time resolution, sampling frequency, spatial coverage, or
#' the classifications (e.g., urban or rural) can differ country by country. Before downloading the data, we suggest to manage and filter the stations/sensors
#' of interest through their metadata files (provided by \code{EEAaq_get_stations} or \code{EEAaq_get_dataframe}). See the examples and the vignette
#' for practical examples.
#' @return A data frame of class \code{EEAaq_df}, if \code{zone_name} is specified, and of class \code{EEAaq_df_sfc}
#' if whether the parameter \code{quadrant} or \code{polygon} is specified.
#' @examples
#' \donttest{
#' `%>%` <- dplyr::`%>%`
#' ### Download PM10 data for the province (NUTS-3) of Milano (Italy)
#' ### from January 1st to January 31st, 2023
#' IDstations <- EEAaq_get_stations(byStation = TRUE, complete = FALSE)
#' IDstations <- IDstations %>%
#'                 dplyr::filter(NUTS3 %in% c("Milano")) %>%
#'                 dplyr::pull(AirQualityStationEoICode) %>%
#'                 unique()
#' data <- EEAaq_get_data(IDstations = IDstations, pollutants = "PM10",
#'                        from = "2023-01-01", to = "2023-01-31",
#'                        verbose = TRUE)
#' }
#' @export
EEAaq_get_data <- function(IDstations = NULL, pollutants = NULL, from = NULL, to = NULL, verbose = TRUE) {

  #########################################
  ########## Auxiliary functions ##########
  #########################################
  `%>%` <- dplyr::`%>%`



  ##############################
  ########## Metadata ##########
  ##############################
  pollutant <- EEAaq_get_dataframe(dataframe = "pollutant")
  stations <- EEAaq_get_dataframe(dataframe = "stations")
  NUTS <- EEAaq_get_dataframe(dataframe = "NUTS")



  ############################################
  ########## Check input parameters ##########
  ############################################

  if(verbose ==  T) {
    cat(paste0("Inputs check started at ", Sys.time(), "\n"))
  }

  ##### Check internet connection
  if(!curl::has_internet()) {
    stop("Please check your internet connection. If the problem persists, please contact the package maintainer.")
  }

  ##### Check dates format
  if (is.null(from) && is.null(to)) {
    stop("You need to specify both from and to ")
  }
  if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", from) || !grepl("^\\d{4}-\\d{2}-\\d{2}$", to)) {
    stop("Both from and to must be in the format YYYY-MM-DD.")
  }

  ##### Check pollutants
  # Format
  if (is.null(pollutants) || length(pollutants) == 0) {
    stop("No pollutant specified!")
  }
  if (!is.character(pollutants)) {
    pollutants <- as.character(unique(pollutants))
  }

  # Matching pollutants
  matched_codes <- list()

  # Check the presence of specified pollutants in column `Notation`
  if (any(pollutants %in% pollutant$Notation)) {
    matched_codes <- append(
      matched_codes,
      dplyr::pull(dplyr::filter(pollutant, .data$Notation %in% pollutants),.data$Notation)
    )
  }

  # Check the correctedness of the codes for the specified pollutants
  if (any(pollutants %in% as.character(pollutant$Code))) {
    matched_codes <- append(
      matched_codes,
      dplyr::pull(dplyr::filter(pollutant, as.character(Code) %in% pollutants), .data$Notation)
    )
  }

  # Extract the valid codes and identify the missing codes
  valid_pollutants <- unique(unlist(matched_codes))
  missing_pollutants <- setdiff(pollutants, c(pollutant$Notation, as.character(pollutant$Code)))
  if (length(missing_pollutants) > 0) {
    stop(paste("The following pollutants are not available:", paste(missing_pollutants, collapse = ", ")))
  }
  # Used as parameter for the web query
  pollutants <- valid_pollutants


  ##### Check stations
  IDstations <- unique(IDstations)
  missing_stations <- setdiff(IDstations, stations$AirQualityStationEoICode)
  if (length(missing_stations) > 0) {
    warning("The following stations are not included in stations dataframe : ",
            paste(missing_stations, collapse = ", "))
  }

  ##### Filter user-defined stations
  filter_stations <- stations %>%
    dplyr::filter(.data$AirQualityStationEoICode %in% IDstations,.data$AirPollutant %in% pollutants )

  if (all(!is.na(filter_stations$CITY_NAME))){
    zone_cities <- filter_stations %>%
      dplyr::distinct(CITY_NAME) %>%
      dplyr::pull()
  } else {
    na_samplingpoint <- filter_stations %>%
      dplyr::mutate(SamplingPointId = stringr::str_replace_all(stringr::str_sub(.data$SamplingPointId, 4), ":", "_")) %>%
      dplyr::pull(.data$SamplingPointId)  %>%
      unique()
  }
  # Used as parameter for the web query
  countries <- unique(filter_stations$ISO)



  ###################################
  ########## Download data ##########
  ###################################

  if(verbose ==  T) {
    cat(paste0("Download started at ", Sys.time(), "\n"))
  }

  # Gestione date. Separazione in request per dataset (1, 2, 3)
  date_intervals <- handle_dates(from, to)
  requests_apiUrl1 <- date_intervals[base::sapply(date_intervals, function(x) x$dataset %in% c(1, 2))]
  requests_apiUrl2 <- date_intervals[base::sapply(date_intervals, function(x) x$dataset %in% c(3))]

  combined_df1 <- NULL
  combined_df2 <- NULL
  #gestione apiurl1
  if (exists("requests_apiUrl1") && length(requests_apiUrl1) > 0) {

    queries <- list()
    for (idx in base::seq_along(requests_apiUrl1)) {
      dataset <- requests_apiUrl1[[idx]]$dataset
      dateStart <- requests_apiUrl1[[idx]]$dateStart
      dateEnd <- requests_apiUrl1[[idx]]$dateEnd

      if ((exists("na_samplingpoint") && length(na_samplingpoint) > 0)) {
        # Query senza parametro citta
        json_body <- base::paste0(
          '{"countries": [', base::paste0('"', countries, '"', collapse = ", "), '],',
          '"cities": [', "", '],',
          '"pollutants": [', base::paste0('"', pollutants , '"', collapse = ", "), '],',
          '"dataset": "', dataset, '",',
          '"dateTimeStart": "', dateStart, '",',
          '"dateTimeEnd": "', dateEnd, '"',
          '}'
        )
      } else {

        json_body <- base::paste0(
          '{"countries": [', base::paste0('"', countries, '"', collapse = ", "), '],',
          '"cities": [', base::paste0('"', zone_cities, '"', collapse = ", "), '],',
          '"pollutants": [', base::paste0('"', pollutants , '"', collapse = ", "), '],',
          '"dataset": "', dataset, '",',
          '"dateTimeStart": "', dateStart, '",',
          '"dateTimeEnd": "', dateEnd, '"',
          '}'
        )}
      queries[[idx]] <- json_body
    }
    if(verbose ==  T) {
      base::print(queries)
    }

    #richiesta singola (o filtrato o tutto dataset) parquet file non ha bisogno di filtro date
    apiUrl1 <- "https://eeadmz1-downloads-api-appservice.azurewebsites.net/ParquetFile"
    all_data <- list()

    for (idx in base::seq_along(queries)) {
      request_body <- queries[[idx]]

      res <- httr::POST(
        url = apiUrl1,
        body = request_body,
        encode = "raw",
        httr::add_headers("Content-Type" = "application/json")
      )

      if (res$status_code != 200) {
        base::warning("Request failed for idx ", idx, " with status code ", res$status_code)
        next
      }

      # Forza il nome della directory temporanea
      #temp_dir <- base::file.path("C:/Temp/ParquetProcessing", base::paste0("temp_requests_", idx))
      temp_dir <- base::file.path(tempdir(), base::paste0("temp_requests_", idx))
      if (base::dir.exists(temp_dir)) {
        base::unlink(temp_dir, recursive = TRUE)
      }
      base::dir.create(temp_dir, recursive = TRUE)

      temp_zip_file <- base::file.path(temp_dir, "temp_file.zip")
      base::on.exit({ unlink(temp_dir, recursive = TRUE) }, add = TRUE)

      content_raw1 <- httr::content(res, "raw")
      base::writeBin(content_raw1, temp_zip_file)

      # Verifica se il file .zip  valido
      if (!base::file.exists(temp_zip_file)) {
        warning("The zip file was not created correctly for idx =", idx)
        next
      }
      file_info <- base::file.info(temp_zip_file)
      if (file_info$size == 0) {
        warning("The zip file is empty for idx ", idx)
        next
      }

      # Estrai il contenuto del file .zip
      utils::unzip(temp_zip_file, exdir = temp_dir)
      unzipped_files <- base::list.files(temp_dir, recursive = TRUE, full.names = TRUE)


      # Verifica la presenza di file .parquet
      parquet_files <- base::list.files(temp_dir, pattern = "\\.parquet$", full.names = TRUE, recursive = TRUE)
      if (length(parquet_files) == 0) {
        warning("The zip file is empty for idx ", idx)
        next
      }

      ##########################################################################################################filtro se na_sampling > 0
      if (exists("na_samplingpoint") && !is.null(na_samplingpoint) && length(na_samplingpoint) > 0) {
        parquet_files <- parquet_files[stringr::str_detect(
          basename(parquet_files),
          paste(na_samplingpoint, collapse = "|")
          # converte na_samplingpoint in una sola stringa separando con operatore or
        )]}

      # if (exists("LAU_spoint") && !is.null(LAU_spoint) && length(LAU_spoint) > 0) {
      #   parquet_files <- parquet_files[stringr::str_detect(
      #     basename(parquet_files),
      #     paste(LAU_spoint, collapse = "|")
      #     # converte na_samplingpoint in una sola stringa separando con operatore or
      #   )]}

      ##########################################################################################################f
      data_for_request <- base::do.call(base::rbind, base::lapply(parquet_files, function(file) {
        arrow::read_parquet(file)
      }))

      all_data[[idx]] <- data_for_request
    }

    combined_df1 <- dplyr::bind_rows(all_data)
    if ("FkObservationLog" %in% colnames(combined_df1)) {
      combined_df1 <- combined_df1 %>%
        dplyr::select(-.data$FkObservationLog)
    }
    #print(paste("Dati combinati: ", nrow(combined_df), " righe."))
  }

  # Gestione apiUrl2
  if (exists("requests_apiUrl2") && length(requests_apiUrl2) > 0) {
    apiUrl2 <- "https://eeadmz1-downloads-api-appservice.azurewebsites.net/ParquetFile/urls"

    dateStart <- requests_apiUrl2[[1]]$dateStart
    dateEnd <- requests_apiUrl2[[1]]$dateEnd

    if ((exists("na_samplingpoint") && length(na_samplingpoint) > 0)) {
      # Query senza parametro citta
      json_body <- base::paste0(
        '{"countries": [', base::paste0('"', countries, '"', collapse = ", "), '],',
        '"cities": [', "", '],',
        '"pollutants": [', base::paste0('"', pollutants , '"', collapse = ", "), '],',
        '"dataset": "', 3, '",',
        '"dateTimeStart": "', dateStart, '",',
        '"dateTimeEnd": "', dateEnd, '"',
        '}'
      ) }  else {
        json_body <- base::paste0(
          '{"countries": [', base::paste0('"', countries, '"', collapse = ", "), '],',
          '"cities": [', base::paste0('"', zone_cities, '"', collapse = ", "), '],',
          '"pollutants": [', base::paste0('"', pollutants , '"', collapse = ", "), '],',
          '"dataset": "', 3, '",',
          '"dateTimeStart": "', dateStart, '",',
          '"dateTimeEnd": "', dateEnd, '"',
          '}'
        )}

    print(json_body)
    # Invia la richiesta POST
    res <- httr::POST(
      url = apiUrl2,
      body =  json_body ,
      encode = "raw",
      httr::add_headers("Content-Type" = "application/json")
    )

    # Estrai gli URL dei file .parquet dalla risposta
    content_raw <- httr::content(res, as = "text", encoding = "UTF-8")
    #estrae primo elemento dopo strip
    lines <- base::strsplit(content_raw, "\r\n")[[1]]
    #Controllo degli URL validi
    parquet_urls <- base::grep("\\.parquet$", lines, value = TRUE)
    if (base::length(parquet_urls) == 0) stop("No valid URL found in the answer.")

    # Usa una directory temporanea fissa per scaricare i file
    base_temp_dir <- base::file.path(tempdir(), "ParquetFiles")
    #base_temp_dir <- "C:/Temp/ParquetFiles"
    # Svuota la directory temporanea se esiste
    if (base::dir.exists(base_temp_dir)) {
      base::unlink(base_temp_dir, recursive = TRUE)
    }
    base::dir.create(base_temp_dir, recursive = TRUE)
    downloaded_files <- base::character()
    # Itera sugli URL validi e scarica ciascun file nella directory temporanea
    for (i in base::seq_along(parquet_urls)) {
      dest_file <- base::file.path(base_temp_dir, base::basename(parquet_urls[i]))
      tryCatch({
        utils::download.file(parquet_urls[i], dest_file, mode = "wb", quiet = TRUE)
        downloaded_files <- base::c(downloaded_files, dest_file)  # Aggiungi il file scaricato

      }, error = function(e) {
        base::warning("Error while downloading ", parquet_urls[i], ": ", e$message)
      })
    }
    print(downloaded_files)
    ##################################################################################
    if (exists("na_samplingpoint") && !is.null(na_samplingpoint) && length(na_samplingpoint) > 0) {
      patterns <- stringr::str_sub(na_samplingpoint, 1, 30)
      downloaded_files <- downloaded_files[stringr::str_detect(
        basename(downloaded_files),
        paste(patterns, collapse = "|") #converte na_samplingpoint in una sola stringa separando con operatore or
      )]}
    cat( "filtrati", downloaded_files)
    #########################################################################################

    if (base::length(downloaded_files) == 0) base::stop("No files successfully downloaded.")
    if(verbose ==  T) {
      # base::print(downloaded_files)
    }
    # Converti `dateStart` e `dateEnd` in formato POSIXct
    dateStart <- base::as.POSIXct(dateStart,format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    dateEnd <- base::as.POSIXct(dateEnd, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    combined_df2 <- arrow::open_dataset(downloaded_files) %>%
      dplyr::filter(.data$Start >= dateStart & .data$End <= dateEnd) %>%
      dplyr::collect()
  }



  #####################################
  ########## Post-processing ##########
  #####################################

  if(verbose ==  T) {
    cat(paste0("Post-processing started at ", Sys.time(), "\n"))
  }

  ##### Check for empty dataset
  combined_df <- dplyr::bind_rows(combined_df1, combined_df2)
  if (nrow(combined_df) == 0) {
    base::warning("Combined data frame is empty.")
  }

  ##### Add stations code and name
  combined_df <- combined_df %>%
    dplyr::left_join(
      filter_stations %>%
        dplyr::select(
          .data$SamplingPointId,
          .data$AirQualityStationEoICode,
          .data$AirQualityStationName
        ),
      by = c("Samplingpoint" = "SamplingPointId")  # Mappatura corretta delle colonne
    )

  ##### Other manipulations
  combined_df <- combined_df %>%
    # Filtro per validity
    dplyr::filter(!.data$Validity %in% c(-1, -99)) %>%
    # Remove unuseful vars
    dplyr::select(-dplyr::any_of(c("DataCapture", "ResultTime"))) %>%
    # Rename
    dplyr::rename(
      DatetimeBegin = .data$Start,
      DatetimeEnd = .data$End,
      AveragingTime = .data$AggType,
      Concentration= .data$Value
    )

  ##### Check overwriting tra day e hour
  if (any(duplicated(combined_df[, c("AirQualityStationEoICode", "Pollutant", "DatetimeBegin")]))) {
    combined_df <- combined_df %>%
      dplyr::group_by(.data$AirQualityStationEoICode, .data$Pollutant, .data$DatetimeBegin, .data$DatetimeEnd) %>%
      dplyr::mutate(n_righe = dplyr::n()) %>%
      dplyr::filter(
        .data$n_righe == 1 | (.data$n_righe > 1 & .data$AveragingTime == "hour")
      ) %>%
      dplyr::select(-.data$n_righe) %>%
      dplyr::ungroup()
  }

  ##### Rotation to wide format by separating accordin to temporal granularity
  # Extended name of the pollutants
  pollutant_map <- pollutant %>%
    dplyr::mutate(Code = base::as.integer(Code)) %>%
    dplyr::filter(Code %in% base::unique(combined_df$Pollutant)) %>%
    dplyr::select("Code", "Notation", "URI")
  # Daily data
  if (any(unique(combined_df$AveragingTime) == "day") == TRUE) {
    dayss <- combined_df %>%
      dplyr::filter(.data$AveragingTime == "day") %>%
      dplyr::mutate(PollutantName = pollutant_map$Notation[base::match(.data$Pollutant, pollutant_map$Code)]) %>%
      dplyr::select(.data$AirQualityStationEoICode,.data$AirQualityStationName,.data$PollutantName,
                    .data$AveragingTime,.data$DatetimeBegin,.data$Concentration) %>%
      dplyr::mutate(DatetimeBegin = as.character(format(x = .data$DatetimeBegin, format = "%Y-%m-%d %H:%M:%S"))) %>%
      tidyr::pivot_wider(names_from = .data$DatetimeBegin, values_from = .data$Concentration) %>%
      tidyr::pivot_longer(cols = -c(.data$AirQualityStationEoICode,.data$AirQualityStationName,
                                    .data$PollutantName,.data$AveragingTime),
                          names_to = "DatetimeBegin",
                          values_to = "Concentration") %>%
      dplyr::mutate(DatetimeBegin = lubridate::ymd_hms(.data$DatetimeBegin),
                    DatetimeEnd = .data$DatetimeBegin + lubridate::days(1), .after = .data$DatetimeBegin) %>%
      tidyr::pivot_wider(
        names_from = .data$PollutantName,
        values_from = .data$Concentration,
        values_fn = function(x) mean(x,na.rm=T)
      )
  } else {
    dayss <- NULL
  }
  # Hourly data
  if (any(unique(combined_df$AveragingTime) == "hour") == TRUE) {
    hourss <- combined_df %>%
      dplyr::filter(.data$AveragingTime == "hour") %>%
      dplyr::mutate(PollutantName = pollutant_map$Notation[base::match(.data$Pollutant, pollutant_map$Code)]) %>%
      dplyr::select(.data$AirQualityStationEoICode,.data$AirQualityStationName,.data$PollutantName,
                    .data$AveragingTime,.data$DatetimeBegin,.data$Concentration) %>%
      dplyr::mutate(DatetimeBegin = as.character(format(x = .data$DatetimeBegin, format = "%Y-%m-%d %H:%M:%S"))) %>%
      tidyr::pivot_wider(names_from = .data$DatetimeBegin,
                         values_from = .data$Concentration,
                         values_fn = function(x) mean(x,na.rm=T)) %>%
      tidyr::pivot_longer(cols = -c(.data$AirQualityStationEoICode,.data$AirQualityStationName,
                                    .data$PollutantName,.data$AveragingTime),
                          names_to = "DatetimeBegin",
                          values_to = "Concentration") %>%
      dplyr::mutate(DatetimeBegin = lubridate::ymd_hms(.data$DatetimeBegin),
                    DatetimeEnd = .data$DatetimeBegin + lubridate::hours(1), .after = .data$DatetimeBegin) %>%
      tidyr::pivot_wider(
        names_from = .data$PollutantName,
        values_from = .data$Concentration,
        values_fn = function(x) mean(x,na.rm=T)
      )
  } else {
    hourss <- NULL
  }
  # Concatenate restuls for dayss e hourss
  combined_df <- dplyr::bind_rows(dayss, hourss) %>%
    dplyr::select(.data$AirQualityStationEoICode,.data$AirQualityStationName,
                  .data$DatetimeBegin,.data$DatetimeEnd,.data$AveragingTime,dplyr::everything()) %>%
    dplyr::arrange(.data$AirQualityStationEoICode,.data$AirQualityStationName,.data$DatetimeBegin)


  #########################################################
  ########## Ex-post checking on the actual data ##########
  #########################################################
  if (all(pollutants %in% pollutant$Notation)) {
    # Se pollutants in forma Notation
    valid_pollutants <- pollutants[pollutants %in% names(combined_df)]
  } else {
    # Se pollutants in forma  Code, convertilo in Notation
    pollutants <- pollutant %>%
      dplyr::filter(Code %in% pollutants) %>%
      dplyr::pull(.data$Notation)
    # Filtra pollutants disponibili in combined_df
    valid_pollutants <- pollutants[pollutants %in% names(combined_df)]
    if (length(missing_pollutants) > 0) {
      stop(paste("Data are actually available only for the following pollutants:", paste(valid_pollutants, collapse = " & ")))
    }
  }



  #####################################################
  ########## Add attributes to final dataset ##########
  #####################################################
  attr(combined_df, "class") <- c("EEAaq_df", "tbl_df", "tbl", "data.frame")
  attr(combined_df, "frequency") <- paste(unique(combined_df$AveragingTime),collapse = " & ")
  attr(combined_df, "pollutants") <- valid_pollutants
  attr(combined_df, "countries") <- countries

  return(combined_df)
}
