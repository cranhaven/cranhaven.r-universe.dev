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
#' ## from January 1st to January 31st, 2023
#' IDstations <- EEAaq_get_stations(byStation = TRUE, complete = FALSE)
#' IDstations <- IDstations %>%
#'                 dplyr::filter(NUTS3 %in% c("Milano")) %>%
#'                 dplyr::pull(AirQualityStationEoICode) %>%
#'                 unique()
#' data <- EEAaq_get_data(IDstations = IDstations, pollutants = "PM10",
#'                        from = "2024-01-01", to = "2025-01-31", verbose = TRUE)
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
  #function to report combinations (pollutant-station) not found
  my_function <- function(x){
    for (id in IDstations){

      filter_stations <- stations %>%
        dplyr::filter(.data$AirQualityStationEoICode == id,.data$AirPollutant == x )

      if (nrow(filter_stations) == 0 && verbose== T) {
        warning(paste("The following stations:", id, "are not associated with the pollutant : ",x))
      }

    }

  }
  filter_stations <- stations %>%
    dplyr::filter(.data$AirQualityStationEoICode %in% IDstations,.data$AirPollutant %in% pollutants )

  if(nrow(filter_stations)==0){
    stop(paste("The following stations:", paste(IDstations, collapse = ", "), "are not associated with the pollutant : ",paste(pollutants, collapse = ", ")))

  } else {

    invisible(lapply(pollutants, my_function))

  }



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
  # filter dates for download phase
  dtSt_filter <- base::as.POSIXct(as.Date(from),format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  dtEn_filter <- base::as.POSIXct(as.Date(to), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  # Gestione date. Separazione in request per dataset (1, 2, 3)
  date_intervals <- handle_dates(from, to)

  combined_df <- NULL

  #gestione apiurl1
  if (exists("date_intervals") && length(date_intervals) > 0) {

    queries <- list()
    for (idx in base::seq_along( date_intervals)) {
      dataset <-  date_intervals[[idx]]$dataset
      dateStart <-  date_intervals[[idx]]$dateStart
      dateEnd <-  date_intervals[[idx]]$dateEnd

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
    apiUrl1 <- "https://eeadmz1-downloads-api-appservice.azurewebsites.net/ParquetFile/urls"
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

      #corpo risposta HTTp
      content_raw1 <- httr::content(res, as = "text", encoding = "UTF-8")


      #per separare files
      lines <- unlist(strsplit(content_raw1, "\n"))
      parquet_files <- base::grep("\\.parquet$", lines, value = TRUE)
      if (base::length(parquet_files) == 0) stop("No valid parquet file found in the answer.")

      #print(parquet_files )
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

    combined_df <- dplyr::bind_rows(all_data)

    ##### Check if we have any data before proceeding

    if (nrow(combined_df) == 0  || is.null(combined_df)) {
      stop("No data available for the specified parameters")
    }

    combined_df <- combined_df  %>% dplyr::filter(.data$Start >= dtSt_filter & .data$End <= dtEn_filter)
    ##### Check for empty dataset

    if (nrow(combined_df) == 0) {
      stop("No data is available after filtering the date")
    }


    #print(paste("n righe:", nrow(combined_df1)))
    if ("FkObservationLog" %in% colnames(combined_df)) {
      combined_df <- combined_df %>%
        dplyr::select(-FkObservationLog)
    }
    #print(paste("Dati combinati: ", nrow(combined_df), " righe."))
  }

  #####################################
  ########## Post-processing ##########
  #####################################

  if(verbose ==  T) {
    cat(paste0("Post-processing started at ", Sys.time(), "\n"))
  }




  ##### Add stations code and name
  combined_df <- combined_df %>%
    dplyr::filter(Samplingpoint %in% unique(filter_stations$SamplingPointId)) %>%
    dplyr::left_join(
      filter_stations %>%
        dplyr::select(SamplingPointId, AirQualityStationEoICode, AirQualityStationName),
      by = c("Samplingpoint" = "SamplingPointId"))
  #check se Idstazione richiesto è presente
  stations_missing <- setdiff(IDstations, unique(combined_df$	AirQualityStationEoICode))
  if ( length(stations_missing) > 0){
    if (length(IDstations) == length(stations_missing )){
      stop(paste("None of the requested stations are available in the data: ",
                 paste(IDstations, collapse = ", ")))
    }else{
      warning("The following stations are not included in stations dataframe : ",
              paste(stations_missing, collapse = ", "))
    }
  }

  ##### Other manipulations
  combined_df <- combined_df %>%
    # Filtro per validity
    dplyr::filter(!.data$Validity %in% c(-1, -99)) %>%
    # Remove unuseful vars
    dplyr::select(-dplyr::any_of(c("DataCapture", "ResultTime"))) %>%
    # Rename
    dplyr::rename(
      DatetimeBegin = Start,
      DatetimeEnd = End,
      AveragingTime = AggType,
      Concentration= Value
    )

  ##### Check overwriting tra day e hour
  if (any(duplicated(combined_df[, c("AirQualityStationEoICode", "Pollutant", "DatetimeBegin")]))) {
    combined_df <- combined_df %>%
      dplyr::group_by(.data$AirQualityStationEoICode, .data$Pollutant, .data$DatetimeBegin, .data$DatetimeEnd) %>%
      dplyr::mutate(n_righe = dplyr::n()) %>%
      dplyr::filter(
        n_righe == 1 | (.data$n_righe > 1 & .data$AveragingTime == "hour")
      ) %>%
      dplyr::select(-n_righe) %>%
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
      dplyr::select(AirQualityStationEoICode,AirQualityStationName,PollutantName,
                    AveragingTime,DatetimeBegin,Concentration) %>%
      dplyr::mutate(DatetimeBegin = as.character(format(x = .data$DatetimeBegin, format = "%Y-%m-%d %H:%M:%S"))) %>%
      tidyr::pivot_wider(names_from = DatetimeBegin, values_from = Concentration) %>%
      tidyr::pivot_longer(cols = -c(AirQualityStationEoICode,AirQualityStationName,
                                    PollutantName,AveragingTime),
                          names_to = "DatetimeBegin",
                          values_to = "Concentration") %>%
      dplyr::mutate(DatetimeBegin = lubridate::ymd_hms(.data$DatetimeBegin),
                    DatetimeEnd = .data$DatetimeBegin + lubridate::days(1), .after = .data$DatetimeBegin) %>%
      tidyr::pivot_wider(
        names_from = PollutantName,
        values_from = Concentration,
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
      dplyr::select(AirQualityStationEoICode,AirQualityStationName,PollutantName,
                    AveragingTime,DatetimeBegin,Concentration) %>%
      dplyr::mutate(DatetimeBegin = as.character(format(x = .data$DatetimeBegin, format = "%Y-%m-%d %H:%M:%S"))) %>%
      tidyr::pivot_wider(names_from = DatetimeBegin,
                         values_from = Concentration,
                         values_fn = function(x) mean(x,na.rm=T)) %>%
      tidyr::pivot_longer(cols = -c(AirQualityStationEoICode,AirQualityStationName,
                                    PollutantName,AveragingTime),
                          names_to = "DatetimeBegin",
                          values_to = "Concentration") %>%
      dplyr::mutate(DatetimeBegin = lubridate::ymd_hms(.data$DatetimeBegin),
                    DatetimeEnd = .data$DatetimeBegin + lubridate::hours(1), .after = .data$DatetimeBegin) %>%
      tidyr::pivot_wider(
        names_from = PollutantName,
        values_from = Concentration,
        values_fn = function(x) mean(x,na.rm=T)
      )
  } else {
    hourss <- NULL
  }
  # Concatenate restuls for dayss e hourss
  combined_df <- dplyr::bind_rows(dayss, hourss) %>%
    dplyr::select(AirQualityStationEoICode,AirQualityStationName,
                  DatetimeBegin,DatetimeEnd,AveragingTime,dplyr::everything()) %>%
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
