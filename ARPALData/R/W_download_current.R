#' @keywords internal
#' @noRd

W_download_current <- function(Metadata,Date_begin,Date_end,verbose = TRUE,
                               parallel = FALSE, parworkers = NULL, parfuturetype = "multisession") {
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
    Date_end <- lubridate::ymd_hms(paste0(Date_end," 23:55:00"))
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

  ##### Check for wind gust (same ID codes of wind speed but different link)
  if (any(unique(Metadata$Measure) == "Wind_speed")) {
    measures <- c(unique(Metadata$Measure),"Wind_speed_gust")
  } else {
    measures <- unique(Metadata$Measure)
  }

  ##### Create combinations of weather parameters and years
  url_combs <- expand.grid(measures,break_years)
  colnames(url_combs) <- c("WPar","Year")

  URLs <- res_check <- numeric(length = dim(url_combs)[1])

  for (yr in 1:dim(url_combs)[1]) {
    URLs[yr] <- url <- "https://www.dati.lombardia.it/Ambiente/Dati-sensori-meteo/647i-nhxk/about_data"
    temp <- tempfile()
    res <- suppressWarnings(try(curl::curl_fetch_disk(url, temp), silent = TRUE))
    url_combs$link1[yr] <- URLs[yr] <- url <- "https://www.dati.lombardia.it/resource/647i-nhxk.json"
    if(res$status_code != 200) {
      message(paste0("The internet resource for ",url_combs[yr,"WPar"]," in year ", url_combs[yr,"Year"]," is not available at the moment. Status code: ",res$status_code,".\nPlease, try later. If the problem persists, please contact the package maintainer."))
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
      message("None of the required online resources is available. Please, try with a new request.\n")
      return(invisible(NULL))
    }
  }


  ### Downloading data
  if (verbose == TRUE) {
    cat("Downloading data: started at", as.character(Sys.time()), "\n")
  }


  ### Building URLs/links in Socrata format using sequences of dates
  if (length(break_years) == 1) {
    break_dates <- paste0(break_years,"-12-31 23:00:00")
  } else {
    break_dates <- paste0(break_years[-length(break_years)],"-12-31 23:00:00")
  }
  dates_seq_end <- c(Date_begin,lubridate::ymd_hms(break_dates),Date_end)
  dates_seq_end <- unique(dates_seq_end)
  dates_seq_end <- dates_seq_end[-1]
  dates_seq_end <- dates_seq_end[dates_seq_end <= Date_end & dates_seq_end >= Date_begin]
  dates_seq_begin <- c(Date_begin,lubridate::ymd_hms(break_dates) + lubridate::hours(1))
  dates_seq_begin <- unique(dates_seq_begin)
  dates_seq_begin <- dates_seq_begin[dates_seq_begin <= Date_end & dates_seq_begin >= Date_begin]

  URL_blocks <- vector(mode = "list", length = dim(url_combs)[1])
  for (i in 1:dim(url_combs)[1]) {

    if (url_combs$WPar[i] == "Wind_speed_gust") {
      IDSensor_link <- Metadata %>%
        dplyr::filter(.data$Measure %in% "Wind_speed") %>%
        dplyr::pull(.data$IDSensor)
    } else {
      IDSensor_link <- Metadata %>%
        dplyr::filter(.data$Measure %in% url_combs$WPar[i]) %>%
        dplyr::pull(.data$IDSensor)
    }

    url_combs$str_sensor[i] <- paste0(" AND idsensore in(",paste0(sapply(X = IDSensor_link, function(x) paste0("'",x,"'")),collapse = ","),")")

    dates_blocks <- vector(mode = "list", length = length(dates_seq_begin))
    for (b in 1:length(dates_seq_begin)) {
      seq_temp <- seq(lubridate::as_datetime(dates_seq_begin[b]),
                      lubridate::as_datetime(dates_seq_end[b]),
                      length.out = n_blocks + 1)

      seq_begin_temp <- seq_temp[-length(seq_temp)]
      seq_begin_temp <- lubridate::round_date(seq_begin_temp, unit = "hour")
      seq_begin_temp <- stringr::str_replace(string = seq_begin_temp, pattern = " ", replacement = "T")

      seq_end_temp <- c(seq_temp[-c(1,length(seq_temp))] - lubridate::hours(1),seq_temp[length(seq_temp)])
      seq_end_temp <- lubridate::round_date(seq_end_temp, unit = "hour")
      seq_end_temp <- stringr::str_replace(string = seq_end_temp, pattern = " ", replacement = "T")
      dates_blocks[[b]] <- data.frame(seq_begin_temp,seq_end_temp)
    }

    URL_blocks[[i]] <- dplyr::bind_cols(dplyr::bind_rows(dates_blocks),url_combs[i,]) %>%
      dplyr::mutate(link = paste0(.data$link1,"?$where=data between '", seq_begin_temp, "' and '", seq_end_temp,"'", .data$str_sensor)) %>%
      dplyr::select(.data$link)

  }
  URL_blocks <- dplyr::bind_rows(URL_blocks)


  ### Preparing parallel computation: explicitly open multisession/multicore workers by switching plan
  if (parallel == TRUE) {
    # Explicitly open multisession/multicore workers by switching plan
    future::plan(future::multisession, workers = 12)
    if (is.null(parworkers)) {
      parworkers <- future::availableCores()/2
    }
    eval(parse(text = paste0("future::plan(future::",parfuturetype,", workers = ",parworkers,")")))
    if (verbose == TRUE) {
      message("Start parallel computing: number of parallel workers = ", future::nbrOfWorkers())
    }
  }

  ##### Download using Socrata API
  Meteo <- do.call(
    rbind,
    future.apply::future_apply(X = as.matrix(URL_blocks), MARGIN = 1, FUN = function(x) {
      RSocrata::read.socrata(url = x, app_token = "Fk8hvoitqvADHECh3wEB26XbO")
    })
  )
  Meteo <- Meteo %>%
    dplyr::rename(operatore = .data$idoperatore) %>%
    dplyr::mutate(operatore = case_when(.data$operatore == 1 ~ "Average",
                                        .data$operatore == 3 ~ "Max"))

  return(Meteo)
}
