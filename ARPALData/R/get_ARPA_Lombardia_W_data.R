#' Download weather/meteorological data from ARPA Lombardia website
#'
#' More detailed description.
#'
#' @description 'get_ARPA_Lombardia_W_data' returns observed air weather measurements collected by
#' ARPA Lombardia ground detection system for Lombardy region in Northern Italy.
#' Available meteorological variables are: temperature (Celsius degrees), rainfall (mm), wind speed (m/s),
#' wind direction (degrees), relative humidity (%), global solar radiation (W/m2), and snow height (cm).
#' Data are available from 1989 and are updated up to the current date.
#' For more information about the municipal data visit the section 'Idro-Nivo-Meteo' at the webpage:
#' https://www.dati.lombardia.it/stories/s/auv9-c2sj
#'
#' @param ID_station Numeric value. ID of the station to consider. Using ID_station = NULL, all the available
#' stations are selected. Default is ID_station = NULL.
#' @param Date_begin Character vector of the first date-time to download. Format can be either "YYYY-MM-DD" or "YYYY-MM-DD hh:mm:ss". Default is Date_begin = "2022-01-01".
#' @param Date_end Character vector of the last date-time to download. Format can be either "YYYY-MM-DD" or "YYYY-MM-DD hh:mm:ss". Default is Date_end = "2022-12-31".
#' @param Frequency Temporal aggregation frequency. It can be "10mins", "hourly", "daily", "weekly",
#' "monthly". Default is Frequency = "10mins"
#' @param Var_vec Character vector of variables to aggregate. If NULL (default) all the variables are averaged,
#' except for 'Temperature' and 'Snow_height', which are cumulated.
#' @param Fns_vec Character vector of aggregation function to apply to the selected variables.
#' Available functions are mean, median, min, max, sum, qPP (PP-th percentile), sd, var,
#' vc (variability coefficient), skew (skewness) and kurt (kurtosis). Attention: for Wind Speed and
#' Wind Speed Gust only mean, min and max are available; for Wind Direction and Wind Direction Gust
#' only mean is available.
#' @param by_sensor Logic value (TRUE or FALSE). If 'by_sensor = TRUE', the function returns the observed concentrations
#' by sensor code, while if 'by_sensor = FALSE' (default) it returns the observed concentrations by station.
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
#'
#' @return A data frame of class 'data.frame' and 'ARPALdf'. The object is fully compatible with Tidyverse.
#'
#' @examples
#' \donttest{
#' ## Download all the (10 minutes frequency) weather measurements at station 100
#' ## between August 2022 and April 2024.
#' if (require("RSocrata")) {
#'     get_ARPA_Lombardia_W_data(ID_station = 100, Date_begin = "2022-08-01",
#'           Date_end = "2024-04-30", Frequency = "10mins")
#' }
#' ## Download all the (daily frequency) weather measurements at station 1974 during 2023
#' if (require("RSocrata")) {
#'     get_ARPA_Lombardia_W_data(ID_station = 1974, Date_begin = "2023-01-01",
#'           Date_end = "2023-12-31", Frequency = "daily")
#' }
#' }
#'
#' @export

get_ARPA_Lombardia_W_data <-
  function(ID_station = NULL, Date_begin = "2021-01-01", Date_end = "2022-12-31",
           Frequency = "10mins", Var_vec = NULL, Fns_vec = NULL, by_sensor = FALSE, verbose = TRUE,
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

    ##### Check if package 'RSocrata' is installed
    # See: https://r-pkgs.org/dependencies-in-practice.html#sec-dependencies-in-suggests-r-code
    rlang::check_installed("RSocrata", reason = "Package \"RSocrata\" must be installed to download data from ARPA Lombardia Open Database.")

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

    ##### Control checks on Var_vec and Fns_vec
    # Wind_speed and Wind_direction must be selected together
    if (!is.na(match("Wind_speed",Var_vec)) & is.na(match("Wind_direction",Var_vec))) {
      stop("It's not possible to select only Wind_speed: also Wind_direction must be included in the list of selected variables! Change the values of 'Var_vec' and 'Var_funs'",
           call. = FALSE)
    }
    if (is.na(match("Wind_speed",Var_vec)) & !is.na(match("Wind_direction",Var_vec))) {
      stop("It's not possible to select only Wind_direction: also Wind_speed must be included in the list of selected variables! Change the values of 'Var_vec' and 'Var_funs'",
           call. = FALSE)
    }
    # Wind direction can only be averaged
    if(sum(Var_vec %in% c("Wind_direction","Wind_direction_gust") & Fns_vec != "mean") > 0) {
      stop("Error: on Wind_direction and Wind_direction_gust is possible to calculate only the average value. Use 'mean' in 'Fns_vec.'",
           call. = FALSE)
    }
    # Wind speed can only be averaged, maximized or minimized
    if(sum(Var_vec %in% c("Wind_speed","Wind_speed_gust") & Fns_vec %notin% c("mean","min","max")) > 0) {
      stop("Error: on Wind_speed and Wind_speed_gust is possible to calculate only mean, max or min values. Use 'mean' or 'max' or 'min' in 'Fns_vec.'",
           call. = FALSE)
    }



    ######################################
    ########## Downloading data ##########
    ######################################

    ### Registry
    Metadata <- W_metadata_reshape()
    # Metadata <- Metadata %>%
    #   dplyr::select(-c(.data$Altitude,.data$Province,
    #                    .data$DateStart,.data$DateStop,
    #                    .data$Latitude,.data$Longitude))

    ### Checks if ID_station is valid (in the list of active stations)
    if (!is.null(ID_station) & all(ID_station %notin% Metadata$IDStation)) {
      stop("ID_station NOT in the list of active stations. Change ID_station or use ID_station = NULL",
           call. = FALSE)
    }

    Metadata <- Metadata %>%
      dplyr::filter((.data$DateStop >= Date_begin | is.na(.data$DateStop)) &
                      .data$DateStart <= Date_begin)
    if (!is.null(ID_station)) {
      Metadata <- Metadata %>%
        dplyr::filter(.data$IDStation %in% ID_station)
    }
    if (!is.null(Var_vec)) {
      Metadata <- Metadata %>%
        dplyr::filter(.data$Measure %in% Var_vec)
    }

    ##### Download using Socrata API
    # Attention: this is a correction for issues with adaptive database of ARPA Lombardia
    begin <- Sys.time()
    Today <- Sys.time()
    Past <- Today - lubridate::dmonths(7) - lubridate::ddays(1)
    Past <- lubridate::round_date(Past, "day")
    # Checked on 01/05/2025: weather data from 02/02/2024 to Today - 7 months are unavailable
    Break_date_current <- min(c(Past,"2024-02-01"))
    if (Date_end >= Break_date_current & Date_begin <= Break_date_current) {
      Meteo1 <- W_download_past(Metadata,Date_begin,Break_date_current)
      Meteo2 <- try(
        expr = {
          W_download_current(Metadata,Break_date_current + lubridate::minutes(5),Date_end)
        },silent = TRUE
      )
      if (is.element("try-error", attr(Meteo2,"class"))) {
        Meteo2 <- NULL
      }
      if (verbose==T) {
        cat(paste0("Attention: it is possible that part of the weather data from 2024-02-01 to ", Date_end," were made unavailable from ARPA Lombardia or the regional administration: please, contact the package maintainer for details or report a bug, thank you.\n"))
      }
      Meteo <- bind_rows(Meteo1,Meteo2)
    }
    if (Date_end < Break_date_current & Date_begin < Break_date_current) {
      Meteo <- W_download_past(Metadata,Date_begin,Date_end)
    }
    if (Date_end > Break_date_current & Date_begin > Break_date_current) {
      Meteo <- W_download_current(Metadata,Date_begin,Date_end,Date_end)
    }
    end <- Sys.time()
    end - begin





    #####################################
    ########## Processing data ##########
    #####################################
    if (verbose == TRUE) {
      cat("Processing data: started at", as.character(Sys.time()), "\n")
    }
    ### Change variable names
    Meteo <- Meteo %>%
      dplyr::filter(!is.na(.data$data),
                    .data$stato %in% c("VA")) %>%
      dplyr::select(IDSensor = .data$idsensore, Date = .data$data, Value = .data$valore,
                    Status = .data$stato, Operator = .data$operatore) %>%
      dplyr::mutate(
        # Date = lubridate::ymd_hms(.data$Date),
        Value = as.numeric(.data$Value),
        IDSensor = as.character(.data$IDSensor)
      )
    ### Add metadata
    Meteo <- dplyr::right_join(Meteo,Metadata, by = "IDSensor")
    ### Cleaning
    if (by_sensor %in% c(1,TRUE)) {
      Meteo <- Meteo %>%
        dplyr::select(.data$Date,.data$IDStation,.data$NameStation,.data$IDSensor,
                      .data$Measure,.data$Value) %>%
        dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric),
                                    ~ dplyr::na_if(.,-9999))) %>%
        dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric),
                                    ~ dplyr::na_if(.,NaN)))
    } else if (by_sensor %in% c(0,FALSE)) {
      Meteo <- Meteo %>%
        dplyr::mutate(Measure = dplyr::case_when(.data$Measure == "Wind_direction" & .data$Operator == "Max" ~ "Wind_direction_gust",
                                                 .data$Measure == "Wind_speed" & .data$Operator == "Max" ~ "Wind_speed_gust",
                                                 TRUE ~ as.character(.data$Measure))) %>%
        # dplyr::select(-c(.data$IDSensor, .data$Operator, .data$Status, .data$Unit_meas)) %>%
        dplyr::select(.data$Date,.data$IDStation,.data$NameStation,.data$Measure,.data$Value) %>%
        tidyr::pivot_wider(names_from = .data$Measure, values_from = .data$Value,
                           values_fn = function(x) mean(x,na.rm=T)
                           ) %>%
        dplyr::mutate(dplyr::across(dplyr::matches(c("Wind_direction","Wind_direction_gust")), ~ round(.x,0))) %>%
        dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric),
                                    ~ dplyr::na_if(.,-9999))) %>%
        dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric),
                                    ~ dplyr::na_if(.,NaN)))
    }
    Meteo[is.na(Meteo)] <- NA
    Meteo[is.nan_df(Meteo)] <- NA

    ### Add dataset attributes
    attr(Meteo, "class") <- c("ARPALdf","ARPALdf_W","tbl_df","tbl","data.frame")

    ### Cleaning
    if (is.null(Var_vec) & is.null(Fns_vec)) {
      vv <- c("Rainfall","Temperature","Relative_humidity","Global_radiation","Water_height",
              "Snow_height","Wind_speed","Wind_speed_gust","Wind_direction")
      vv <- vv[vv %in% names(Meteo)]
      fv <- ifelse(vv == "Rainfall" | vv == "Snow_height", "sum", "mean")
      fv <- ifelse(vv == "Wind_speed_gust", "max", "mean")
    } else {
      vv <- Var_vec
      fv <- Fns_vec
    }

    # Checks if all the variables are available for the selected stations
    if (all(dplyr::all_of(vv) %in% names(Meteo)) == F) {
      stop("One ore more variables are not avaiable for the selected stations! Change the values of 'Var_vec'",
           call. = FALSE)
    }

    if (by_sensor %in% c(0,FALSE)) {
      ### Aggregating dataset
      if (Frequency != "10mins") {
        if (verbose==T) {
          cat("Aggregating ARPA Lombardia data: started started at", as.character(Sys.time()), "\n")
        }
        Meteo <- Meteo %>%
          Time_aggregate(Frequency = Frequency, Var_vec = Var_vec, Fns_vec = Fns_vec, verbose = verbose) %>%
          dplyr::arrange(.data$NameStation, .data$Date)
      } else {
        Meteo <- Meteo %>%
          dplyr::select(.data$Date,.data$IDStation,.data$NameStation,vv) %>%
          dplyr::arrange(.data$NameStation, .data$Date)
      }

      ### Regularizing dataset: same number of timestamps for each station and variable
      if (verbose == TRUE) {
        cat("Regularizing ARPA data: started started at", as.character(Sys.time()), "\n")
      }

      Meteo <- Meteo %>%
        dplyr::arrange(.data$Date) %>%
        dplyr::filter(.data$Date >= Date_begin,
                      .data$Date <= Date_end) %>%
        tidyr::pivot_longer(cols = -c(.data$Date,.data$IDStation,.data$NameStation),
                            names_to = "Measure", values_to = "Value") %>%
        dplyr::mutate(Date = case_when(Frequency %in% c("10mins","hourly") ~ as.character(format(x = .data$Date, format = "%Y-%m-%d %H:%M:%S")),
                                       TRUE ~ as.character(format(x = .data$Date, format = "%Y-%m-%d")))) %>%
        tidyr::pivot_wider(names_from = .data$Date, values_from = .data$Value) %>%
        tidyr::pivot_longer(cols = -c(.data$Measure,.data$IDStation,.data$NameStation),
                            names_to = "Date", values_to = "Value") %>%
        tidyr::pivot_wider(names_from = .data$Measure, values_from = .data$Value)

      if (Frequency %notin% c("10mins","hourly")) {
        Meteo <- Meteo %>%
          dplyr::mutate(Date = ymd(.data$Date)) %>%
          dplyr::arrange(.data$IDStation,.data$Date)
      } else {
        Meteo <- Meteo %>%
          dplyr::mutate(Date = ymd_hms(.data$Date)) %>%
          dplyr::arrange(.data$IDStation,.data$Date)
      }

      freq_unit <- dplyr::case_when(Frequency == "10mins" ~ "10 min",
                                    Frequency == "hourly" ~ "hours",
                                    Frequency == "daily" ~ "days",
                                    Frequency == "weekly" ~ "weeks",
                                    Frequency == "monthly" ~ "months")

      structure(list(Meteo = Meteo))
      attr(Meteo, "class") <- c("ARPALdf","ARPALdf_W","tbl_df","tbl","data.frame")
      attr(Meteo, "frequency") <- Frequency
      attr(Meteo, "units") <- freq_unit
    } else if (by_sensor %in% c(1,TRUE)) {
      Meteo <- Meteo %>%
        dplyr::arrange(.data$Date) %>%
        dplyr::filter(.data$Date >= Date_begin,
                      .data$Date <= Date_end)

      structure(list(Meteo = Meteo))
      attr(Meteo, "class") <- c("ARPALdf","ARPALdf_W","tbl_df","tbl","data.frame")
      attr(Meteo, "frequency") <- "10mins"
      attr(Meteo, "units") <- "10 min"
    }

    if (verbose == TRUE) {
      cat("Processing data: ended at", as.character(Sys.time()), "\n")
    }

    if (verbose == TRUE) {
      cat("Retrieving desired ARPA Lombardia dataset: ended at", as.character(Sys.time()), "\n")
    }

    invisible(gc())
    return(Meteo)

  }
