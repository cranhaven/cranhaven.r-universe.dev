#' Download air quality data at manucipal from ARPA Lombardia website
#'
#' More detailed description.
#'
#' @description 'get_ARPA_Lombardia_AQ_municipal_data' returns the air quality levels at municipal level estimated by
#' ARPA Lombardia using a physico-chemical model which simulates air quality based on weather and geo-physical
#' variables. For each municipality of Lombardy, ARPA estimates the average (NO2_mean) and maximum daily (NO2_max_day)
#' level of NO2, the daily maximum (Ozone_max_day) and the 8-hours moving window maximum (Ozone_max_8h) of Ozone
#' and the average levels of PM10 (PM10_mean) and PM2.5 (PM2.5_mean).
#' Data are available from 2011 and are updated up to the current date.
#' For more information about the municipal data visit the section 'Stime comunali dell'aria' at the webpage:
#' https://www.dati.lombardia.it/stories/s/auv9-c2sj
#'
#' @param ID_station Numeric value. ID of the station to consider. Using ID_station = NULL, all the available
#' stations are selected. Default is ID_station = NULL.
#' @param Date_begin Character vector of the first date-time to download. Format can be either "YYYY-MM-DD" or "YYYY-MM-DD hh:mm:ss". Default is Date_begin = "2022-01-01".
#' @param Date_end Character vector of the last date-time to download. Format can be either "YYYY-MM-DD" or "YYYY-MM-DD hh:mm:ss". Default is Date_end = "2022-12-31".
#' @param Frequency Temporal aggregation frequency. It can be "daily", "weekly", "monthly" or "yearly".
#' Default is Frequency = "daily".
#' @param Var_vec Character vector of variables to aggregate. If NULL (default) all the variables are averaged.
#' @param Fns_vec Character vector of aggregation function to apply to the selected variables.
#' Available functions are mean, median, min, max, sum, qPP (PP-th percentile), sd, var,
#' vc (variability coefficient), skew (skewness) and kurt (kurtosis).
#' @param by_sensor Logic value (TRUE or FALSE). If 'by_sensor = TRUE', the function returns the observed concentrations by sensor code, while
#' if 'by_sensor = FALSE' (default) it returns the observed concentrations by station.
#' @param verbose Logic value (TRUE or FALSE). Toggle warnings and messages. If 'verbose = TRUE' (default) the function
#' prints on the screen some messages describing the progress of the tasks. If 'verbose = FALSE' any message about
#' the progression is suppressed.
#' @param parallel Logic value (TRUE or FALSE). If 'parallel = FALSE' (default), data downloading is performed using a sequential/serial approach and additional parameters 'parworkers' and 'parfuturetype' are ignored.
#' When 'parallel = TRUE', data downloading is performed using parallel computing through the Futureverse setting.
#' More detailed information about parallel computing in the Futureverse can be found at the following webpages:
#' https://future.futureverse.org/ and https://cran.r-project.org/web/packages/future.apply/vignettes/future.apply-1-overview.html
#' @param parworkers Numeric integer value. If 'parallel = TRUE' (parallel mode active), the user can declare the number of parallel workers to be activated using 'parworkers = integer number'. By default ('parworkers = NULL'), the number of active workers is half of the available local cores.
#' @param parfuturetype Character vector. If 'parallel = TRUE' (parallel mode active), the user can declare the parallel strategy to be used according to the Futureverse syntax through 'parfuturetype'. By default, the 'multisession' (background R sessions on local machine) is used. In alternative, the 'multicore' (forked R processes on local machine. Not supported by Windows and RStudio) setting can be used.
#'
#' @return A data frame of class 'data.frame' and 'ARPALdf'. The object is fully compatible with Tidyverse.
#' The column 'NameStation' identifies the name of each municipality. The column 'IDStation' is an ID code
#' (assigned from ARPA) uniquely identifying each municipality.
#'
#' @examples
#' \donttest{
#' ## Download daily concentrations at municipal levels observed in 2020
#' ## for all the municipalities in Lombardy
#' if (require("RSocrata")) {
#'     get_ARPA_Lombardia_AQ_municipal_data(ID_station=NULL,Date_begin = "2022-01-01",
#'           Date_end = "2023-12-31", Frequency="daily")
#' }
#' ## Download monthly concentrations of NO2 (average and maximum) observed in 2021
#' ## at city number 100451.
#' if (require("RSocrata")) {
#'     get_ARPA_Lombardia_AQ_municipal_data(ID_station=100451,Date_begin = "2023-01-01",
#'           Date_end = "2023-12-31", Frequency="monthly",Var_vec=c("NO2_mean","NO2_mean"),
#'           Fns_vec=c("mean","max"))
#' }
#' ## Download daily concentrations observed in March and April 2022 at city number 100451.
#' ## Data are reported by sensor.
#' if (require("RSocrata")) {
#'     get_ARPA_Lombardia_AQ_municipal_data(ID_station=100451, Date_begin = "2024-03-01",
#'           Date_end = "2024-04-30", by_sensor = TRUE)
#' }
#' }
#'
#' @export

get_ARPA_Lombardia_AQ_municipal_data <-
  function(ID_station = NULL, Date_begin = "2021-01-01", Date_end = "2022-12-31", Frequency = "daily",
           Var_vec = NULL, Fns_vec = NULL, by_sensor = FALSE, verbose=TRUE,
           parallel = FALSE, parworkers = NULL, parfuturetype = "multisession") {

    ###  Welcome message
    if (verbose == TRUE) {
      cat("Retrieving desired ARPA Lombardia dataset: started at", as.character(Sys.time()), "\n")
    }

    ############################
    ########## Checks ##########
    ############################

    ##### Check for internet connection
    if(!curl::has_internet()) {
      message("Internet connection not available at the moment.\nPlease check your internet connection. If the problem persists, please contact the package maintainer.")
      return(invisible(NULL))
    }

    ##### Check if package 'RSocrata' is installed
    # See: https://r-pkgs.org/dependencies-in-practice.html#sec-dependencies-in-suggests-r-code
    rlang::check_installed("RSocrata",
                           reason = "Package \"RSocrata\" must be installed to download data from ARPA Lombardia Open Database.")

    ##### Check if Futureverse is installed
    rlang::check_installed("future",
                           reason = "Package \"future\" must be installed to download (parallel) data from ARPA Lombardia Open Database.")
    rlang::check_installed("future.apply",
                           reason = "Package \"future\" must be installed to download (parallel) data from ARPA Lombardia Open Database.")

    ##### Define %notin%
    '%notin%' <- Negate('%in%')

    ##### Checks if by_sensor setup properly
    if (by_sensor %notin% c(0,1,FALSE,TRUE)) {
      stop("Wrong setup for 'by_sensor'. Use 1 or 0 or TRUE or FALSE.",
           call. = FALSE)
    }

    ##### Checks if parallel setup properly
    if (parallel %notin% c(FALSE,TRUE)) {
      stop("Wrong setup for 'parallel'. Use TRUE or FALSE.",
           call. = FALSE)
    }
    if (parfuturetype %notin% c("multisession","multicore")) {
      stop("Wrong setup for 'parallel'. Use TRUE or FALSE.",
           call. = FALSE)
    }

    ##### Checks Input frequency
    stopifnot("Frequency cannot be hourly as municipal estimates are provided on a daily basis" = (Frequency != "hourly") == T)


    ######################################
    ########## Downloading data ##########
    ######################################

    ##### Define %notin%
    '%notin%' <- Negate('%in%')

    ### Registry/Metadata
    Metadata <- AQ_municipal_metadata_reshape()
    Metadata <- Metadata %>%
      dplyr::select(-c(.data$Province,.data$DateStart,.data$DateStop))

    ### Checks if ID_station is valid (in the list of active stations)
    if (!is.null(ID_station) & all(ID_station %notin% Metadata$IDStation)) {
      stop("ID_station NOT in the list of active stations. Change ID_station or use ID_station = NULL",
           call. = FALSE)
    }

    if (!is.null(ID_station)) {
      Metadata <- Metadata %>%
        dplyr::filter(.data$IDStation %in% ID_station)
    }

    ##### Splitting strategy for improving download speed
    n_blocks <- 12
    # Check dates format
    if (is.null(lubridate::guess_formats(x = Date_begin, orders = c("ymd HMS","ymd")))) {
      stop("Wrong format for 'Date_begin'. Use 'YYYY-MM-DD' or 'YYYY-MM-DD hh:mm:ss'", call. = FALSE)
    }
    if (is.null(lubridate::guess_formats(x = Date_end, orders = c("ymd HMS","ymd")))) {
      stop("Wrong format for 'Date_end'. Use 'YYYY-MM-DD' or 'YYYY-MM-DD hh:mm:ss'", call. = FALSE)
    }
    if (is.na(lubridate::ymd_hms(Date_begin, quiet = TRUE))) {
      Date_begin <- lubridate::ymd_hms(paste0(Date_begin," 00:00:00"))
    }
    if (is.na(lubridate::ymd_hms(Date_end, quiet = TRUE))) {
      Date_end <- lubridate::ymd_hms(paste0(Date_end," 23:00:00"))
    }
    ### Check for the presence of breaking dates (URLs change in several years)
    if (lubridate::year(Date_begin) == lubridate::year(Date_end)) {
      break_years <- lubridate::year(Date_begin)
    } else {
      break_years <- seq(from = lubridate::year(Date_begin), to = lubridate::year(Date_end), by = 1)
    }

    ##### Check online availability of the resources for the specified years
    if (verbose == TRUE) {
      cat("Checking availability of online resources: started at", as.character(Sys.time()), "\n")
    }
    URLs <- res_check <- numeric(length = length(break_years))
    for (yr in 1:length(break_years)) {
      URLs[yr] <- url <- url_AQ_year_dataset(Stat_type = "AQ_municipal_check", Year = break_years[yr])
      temp <- tempfile()
      res <- suppressWarnings(try(curl::curl_fetch_disk(url, temp), silent = TRUE))
      URLs[yr] <- url <- url_AQ_year_dataset(Stat_type = "AQ_municipal", Year = break_years[yr])
      if(res$status_code != 200) {
        message(paste0("The internet resource for year ", break_years[yr]," is not available at the moment. Status code: ",res$status_code,".\nPlease, try later. If the problem persists, please contact the package maintainer."))
      } else {
        res_check[yr] <- 1
      }
    }
    if (verbose == TRUE) {
      if (sum(res_check) == length(break_years)) {
        message("All the online resources are available.\n")
      }
      if (sum(res_check) > 0 & sum(res_check) < length(break_years)) {
        message("Part of the required online resources are not available. Please, try with a new request.\n")
        return(invisible(NULL))
      }
      if (sum(res_check) == 0) {
        message("None of the required online resources are available. Please, try with a new request.\n")
        return(invisible(NULL))
      }
    }

    ### Downloading data
    if (verbose == TRUE) {
      cat("Downloading data: started at", as.character(Sys.time()), "\n")
    }

    ##### Years up to 2021 #####
    if (any(break_years %notin% c(2022,2023,2024))) {
      ### Files names (from ARPA database)
      file_name <- dplyr::case_when(break_years %in% c(2021) ~ paste0("dati_stime_comunali_2021.csv"),
                                    break_years %in% c(2020) ~ paste0("dati_stime_comunali_2020.csv"),
                                    break_years %in% c(2019) ~ paste0("dati_stime_comunali_2019.csv"),
                                    break_years %in% c(2018) ~ paste0("dati_stime_comunali_2018.csv"),
                                    break_years %in% c(2017) ~ paste0("dati_stime_comunali_2017.csv"),
                                    break_years %in% 2011:2016 ~ "dati_stime_comunali_2011-2016.csv")
      URL_blocks <- as.matrix(paste(URLs,file_name))

      ### Download
      Aria1 <- do.call(
        rbind,
        future.apply::future_apply(X = as.matrix(URL_blocks[break_years %notin% c(2022,2023,2024),]), MARGIN = 1, FUN = function(x) {

          ##### Download
          link_str <- stringr::str_split(string = x, pattern = " ")[[1]][1]
          file_str <- stringr::str_split(string = x, pattern = " ")[[1]][2]
          options(timeout = 10000)
          Aria_temp <- readr::read_csv(archive::archive_read(link_str, file = file_str), col_types = readr::cols())
          options(timeout = 100)

          ##### Processing data
          if (verbose == TRUE) {
            cat("Processing data: started at", as.character(Sys.time()), "\n")
          }

          ### Change variable names
          Aria_temp <- Aria_temp %>%
            dplyr::select(IDSensor = .data$IdSensore, Date = .data$Data, Value = .data$Valore,
                          Operator = .data$idOperatore) %>%
            dplyr::mutate(Date =lubridate::dmy_hms(.data$Date),
                          Operator = dplyr::case_when(.data$Operator == 1 ~ "mean",
                                                      .data$Operator == 3 ~ "max",
                                                      .data$Operator == 11 ~ "max_8h",
                                                      .data$Operator == 12 ~ "max_day"))
        })
      )
    } else {
      Aria1 <- NULL
    }
    # clean RAM
    invisible(gc())


    #####  Data from 2022 to current (2024) are provided via Socrata API #####
    if (any(break_years %in% c(2022,2023,2024))) {

      ### Filter valid URLs and dates
      URLs_22on <- URLs[break_years %in% c(2022,2023,2024)]
      break_years_22on <- break_years[break_years %in% c(2022,2023,2024)]
      if (lubridate::ymd("2022-01-01") > Date_begin) {
        Date_begin_22on <- lubridate::ymd("2022-01-01")
      } else {
        Date_begin_22on <- Date_begin
      }

      ### Building URLs/links in Socrata format using sequences of dates
      if (length(break_years_22on) == 1) {
        break_dates <- paste0(break_years_22on,"-12-31 23:00:00")
      } else {
        break_dates <- paste0(break_years_22on[-length(break_years_22on)],"-12-31 23:00:00")
      }
      dates_seq_end <- c(Date_begin_22on,lubridate::ymd_hms(break_dates),Date_end)
      dates_seq_end <- unique(dates_seq_end)
      dates_seq_end <- dates_seq_end[-1]
      dates_seq_end <- dates_seq_end[dates_seq_end <= Date_end & dates_seq_end >= Date_begin_22on]
      dates_seq_begin <- c(Date_begin_22on,lubridate::ymd_hms(break_dates) + lubridate::hours(1))
      dates_seq_begin <- unique(dates_seq_begin)
      dates_seq_begin <- dates_seq_begin[dates_seq_begin <= Date_end & dates_seq_begin >= Date_begin_22on]

      URL_blocks <- vector(mode = "list", length = length(dates_seq_begin))
      for (b in 1:length(dates_seq_begin)) {
        seq_temp <- seq(dates_seq_begin[b],
                        dates_seq_end[b],
                        length.out = n_blocks + 1)

        seq_begin_temp <- seq_temp[-length(seq_temp)]
        seq_begin_temp <- lubridate::round_date(seq_begin_temp, unit = "hour")
        seq_begin_temp <- stringr::str_replace(string = seq_begin_temp, pattern = " ", replacement = "T")

        seq_end_temp <- c(seq_temp[-c(1,length(seq_temp))] - lubridate::hours(1),seq_temp[length(seq_temp)])
        seq_end_temp <- lubridate::round_date(seq_end_temp, unit = "hour")
        seq_end_temp <- stringr::str_replace(string = seq_end_temp, pattern = " ", replacement = "T")

        if (is.null(ID_station)) {
          str_sensor <- NULL
        } else {
          str_sensor <- paste0("AND idsensore in(",paste0(sapply(X = Metadata$IDSensor, function(x) paste0("'",x,"'")),collapse = ","),")")
        }

        URL_blocks[[b]] <- data.frame(seq_begin_temp,seq_end_temp) %>%
          dplyr::mutate(link = paste0(URLs_22on[b],"?$where=data between '", seq_begin_temp, "' and '", seq_end_temp,"'", str_sensor)) %>%
          dplyr::select(.data$link)
      }
      URL_blocks <- dplyr::bind_rows(URL_blocks)

      ### Preparing parallel computation: explicitly open multisession/multicore workers by switching plan
      if (parallel == TRUE) {
        future::plan(future::multisession, workers = 12)
        if (is.null(parworkers)) {
          parworkers <- future::availableCores()/2
        }
        eval(parse(text = paste0("future::plan(future::",parfuturetype,", workers = ",parworkers,")")))
        if (verbose == TRUE) {
          message("Start parallel computing: number of parallel workers = ", future::nbrOfWorkers())
        }
      }

      Aria2 <- do.call(
        rbind,
        future.apply::future_apply(X = as.matrix(URL_blocks), MARGIN = 1, FUN = function(x) {
          RSocrata::read.socrata(url = x, app_token = "Fk8hvoitqvADHECh3wEB26XbO")
        })
      )

      ### Preparing parallel computation: explicitly open multisession/multicore workers by switching plan
      if (parallel == TRUE) {
        # Explicitly close multisession/multicore workers by switching plan
        future::plan(future::sequential)
        if (verbose == TRUE) {
          message("Stop parallel computing: number of parallel workers = ", future::nbrOfWorkers())
        }
      }

      ### Processing data
      Aria2 <- Aria2 %>%
        dplyr::select(IDSensor = .data$idsensore,
                      Date = .data$data,
                      Value = .data$valore,
                      Operator = .data$idoperatore) %>%
        dplyr::mutate(IDSensor = as.numeric(.data$IDSensor),
                      Value = as.numeric(.data$Value),
                      Date = lubridate::ymd(.data$Date),
                      Operator = dplyr::case_when(.data$Operator == 1 ~ "mean",
                                                  .data$Operator == 3 ~ "max",
                                                  .data$Operator == 11 ~ "max_8h",
                                                  .data$Operator == 12 ~ "max_day"))

      ### Ending parallel computation: explicitly close multisession/multicore workers by switching plan
      if (parallel == TRUE) {
        future::plan(future::sequential)
        if (verbose == TRUE) {
          message("Stop parallel computing: number of parallel workers = ", future::nbrOfWorkers())
        }
      }
    } else {
      Aria2 <- NULL
    }



    #####################################
    ########## Processing data ##########
    #####################################

    ### Append datasets
    Aria <- dplyr::bind_rows(Aria1,Aria2)
    rm(Aria1,Aria2)
    # clean RAM
    invisible(gc())

    ### Add metadata
    Aria <- dplyr::right_join(Aria,Metadata, by = "IDSensor")

    ### Cleaning
    if (by_sensor %in% c(1,TRUE)) {
      Aria <- Aria %>%
        dplyr::mutate(Pollutant = paste0(.data$Pollutant,"_",.data$Operator)) %>%
        dplyr::filter(!is.na(.data$Date)) %>%
        dplyr::select(-c(.data$Operator)) %>%
        dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric),
                                    ~ dplyr::na_if(.,-9999))) %>%
        dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric),
                                    ~ dplyr::na_if(.,NaN))) %>%
        dplyr::select(.data$Date,.data$IDStation,.data$NameStation,.data$IDSensor,
                      .data$Pollutant,.data$Value)
    } else if (by_sensor %in% c(0,FALSE)) {
      Aria <- Aria %>%
        dplyr::mutate(Pollutant = paste0(.data$Pollutant,"_",.data$Operator)) %>%
        dplyr::filter(!is.na(.data$Date)) %>%
        dplyr::select(-c(.data$IDSensor,.data$Operator)) %>%
        dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric),
                                    ~ dplyr::na_if(.,-9999))) %>%
        dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric),
                                    ~ dplyr::na_if(.,NaN))) %>%
        tidyr::pivot_wider(names_from = .data$Pollutant, values_from = .data$Value,
                           values_fn = function(x) mean(x,na.rm=T)) # Mean (without NA) of a NA vector = NaN
    }
    Aria[is.na(Aria)] <- NA
    Aria[is.nan_df(Aria)] <- NA

    ### Add dataset attributes
    attr(Aria, "class") <- c("ARPALdf","ARPALdf_AQ","tbl_df","tbl","data.frame")

    ###
    if (is.null(Var_vec) & is.null(Fns_vec)) {
      vv <- c("NO2_mean","NO2_max_day","Ozone_max_8h","Ozone_max_day","PM10_mean","PM2.5_mean")
      vv <- vv[vv %in% names(Aria)]
      fv <- rep("mean",length(vv))
    } else {
      vv <- Var_vec
      fv <- Fns_vec
    }

    # Checks if all the pollutants are available for the selected stations
    if (all(dplyr::all_of(vv) %in% names(Aria)) == FALSE) {
      stop("One ore more pollutants are not avaiable for the selected stations! Change the values of 'Var_vec'",
           call. = FALSE)
    }

    if (by_sensor %in% c(0,FALSE)) {
      ### Aggregating dataset
      if (Frequency != "daily") {
        if (verbose==T) {
          cat("Aggregating ARPA Lombardia data: started started at", as.character(Sys.time()), "\n")
        }
        Aria <- Aria %>%
          Time_aggregate(Frequency = Frequency, Var_vec = Var_vec, Fns_vec = Fns_vec, verbose = verbose) %>%
          dplyr::arrange(.data$NameStation, .data$Date)
      } else {
        Aria <- Aria %>%
          dplyr::select(.data$Date,.data$IDStation,.data$NameStation,vv) %>%
          dplyr::arrange(.data$NameStation, .data$Date)
      }

      ### Regularizing dataset: same number of timestamps for each station and variable
      if (verbose == TRUE) {
        cat("Regularizing ARPA Lombardia data: started started at", as.character(Sys.time()), "\n")
      }
      freq_unit <- dplyr::case_when(Frequency == "daily" ~ "days",
                                    Frequency == "weekly" ~ "weeks",
                                    Frequency == "monthly" ~ "months",
                                    Frequency == "yearly" ~ "years")

      Aria <- Aria %>%
        dplyr::arrange(.data$Date) %>%
        dplyr::filter(.data$Date >= Date_begin,
                      .data$Date <= Date_end) %>%
        tidyr::pivot_longer(cols = -c(.data$Date,.data$IDStation,.data$NameStation),
                            names_to = "Measure", values_to = "Value") %>%
        tidyr::pivot_wider(names_from = .data$Date, values_from = .data$Value) %>%
        tidyr::pivot_longer(cols = -c(.data$Measure,.data$IDStation,.data$NameStation),
                            names_to = "Date", values_to = "Value") %>%
        tidyr::pivot_wider(names_from = .data$Measure, values_from = .data$Value) %>%
        dplyr::mutate(Date = ymd(.data$Date)) %>%
        dplyr::arrange(.data$IDStation,.data$Date)

      structure(list(Aria = Aria))
      attr(Aria, "class") <- c("ARPALdf","ARPALdf_AQ_mun","tbl_df","tbl","data.frame")
      attr(Aria, "frequency") <- Frequency
      attr(Aria, "units") <- freq_unit

    } else if (by_sensor %in% c(1,TRUE)) {
      Aria <- Aria %>%
        dplyr::arrange(.data$Date) %>%
        dplyr::filter(.data$Date >= Date_begin,
                      .data$Date <= Date_end)

      structure(list(Aria = Aria))
      attr(Aria, "class") <- c("ARPALdf","ARPALdf_AQ","tbl_df","tbl","data.frame")
      attr(Aria, "frequency") <- "daily"
      attr(Aria, "units") <- "days"
    }

    if (verbose == TRUE) {
      cat("Processing data: ended at", as.character(Sys.time()), "\n")
    }

    if (verbose == TRUE) {
      cat("Retrieving desired ARPA Lombardia dataset: ended at", as.character(Sys.time()), "\n")
    }

    return(Aria)
  }
