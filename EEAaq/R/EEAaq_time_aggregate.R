#' Time aggregation of an \code{EEAaq_df} class object.
#'
#' \code{EEAaq_time_aggregate} compute a time aggregation of an \code{EEAaq_df} or \code{EEAaq_df_sfc} class object,
#' based on the specified \code{frequency} and the aggregation functions \code{aggr_fun}.
#' @param data an \code{EEAaq_df} or \code{EEAaq_df_sfc} class object, which is the output of the \code{\link{EEAaq_get_data}} function.
#' @param frequency vector containing the time frequency for which to aggregate the \code{data} object.
#' Admissible values are 'yearly', 'monthly', 'weekly', 'daily' 'hourly'.
#' @param aggr_fun character vector containing one or more agregation functions. Admissible values are 'mean', 'median',
#' 'min', 'max', 'sd', 'var',
#' 'quantile_pp' (where pp is a number in the range \[0,1\], representing the required percentile).
#' @return A \code{EEAaq_taggr_df} or a \code{EEAaq_taggr_df_sfc} class object, which is a tibble containing the
#' required time aggregation.
#' @examples
#' \donttest{
#' data <- EEAaq_get_data(zone_name = "Milano", NUTS_level = "LAU",
#'   pollutant = "PM10", from = 2023, to = 2023, ID = FALSE, verbose = TRUE)
#' EEAaq_time_aggregate(data = data, frequency = "monthly", aggr_fun = c("mean", "min", "max"))
#' EEAaq_time_aggregate(data = data, frequency = "yearly", aggr_fun = "mean")
#' }
#' @export

EEAaq_time_aggregate <- function(data = NULL, frequency = "monthly", aggr_fun = c("mean", "min", "max")) {

  `%>%` <- dplyr::`%>%`




  "%notin%" <- Negate("%in%")
  #Verifica che l'oggetto dato in input per il parametro data appartenga alla classe EEAaq_df,
  #quindi che sia l'output della funzione get_EEA_AQ_data
  stopifnot("Data is not of class 'EEAaq_df'" = "EEAaq_df" %in% class(data) | "EEAaq_df_sfc" %in% class(data))

  pollutant <- attributes(data)$pollutant

  #Sistemo le date in modo da valutare i periodi corretti
  data <- data %>% dplyr::mutate(DatetimeBegin = lubridate::with_tz(data$DatetimeBegin, tzone = "CET"), DatetimeEnd = lubridate::with_tz(data$DatetimeBegin, tzone = "CET"))

  data_aggr <- lapply(pollutant, function(pollutant, data, frequency) {switch (frequency, yearly = {
    data %>% dplyr::mutate(Date = lubridate::year(.data$DatetimeBegin))  %>% dplyr::group_by(.data$AirQualityStationEoICode, .data$AirQualityStationName, .data$Date) %>% dplyr::summarise(summ = my_summarise(stats::na.omit(get(pollutant)), aggr_fun)) %>%
      dplyr::mutate(name = aggr_fun) %>% dplyr::ungroup() %>% tidyr::pivot_wider(names_from = "name", values_from = "summ") %>% dplyr::mutate(Date = as.Date(ISOdate(.data$Date, 1, 1)))
  },
  monthly = {
    data %>% dplyr::mutate(Year = lubridate::year(.data$DatetimeBegin), Month = lubridate::month(.data$DatetimeBegin, label = F)) %>%
      dplyr::group_by(.data$AirQualityStationEoICode, .data$AirQualityStationName, .data$Year, .data$Month) %>% dplyr::summarise(summ = my_summarise(stats::na.omit(get(pollutant)), aggr_fun)) %>%
      dplyr::mutate(name = aggr_fun) %>% dplyr::ungroup() %>% tidyr::pivot_wider(names_from = "name", values_from = "summ") %>% dplyr::mutate(Date = lubridate::ym(paste(.data$Year, .data$Month))) %>% dplyr::select(-"Year", -"Month") %>%
      dplyr::relocate(.data$Date, .after = .data$AirQualityStationName)
  },
  weekly = {
    data %>% dplyr::mutate(Year = lubridate::year(.data$DatetimeBegin), Month = lubridate::month(.data$DatetimeBegin, label = T), Week = aweek::date2week(.data$DatetimeBegin, factor = T)) %>% dplyr::group_by(.data$AirQualityStationEoICode, .data$AirQualityStationName, .data$Week) %>% dplyr::summarise(summ = my_summarise(stats::na.omit(get(pollutant)), aggr_fun)) %>%
      dplyr::mutate(name = aggr_fun) %>% dplyr::ungroup() %>% tidyr::pivot_wider(names_from = "name", values_from = "summ") %>%
      dplyr::mutate(Week = lubridate::as_date(aweek::week2date(.data$Week, week_start = 1), tz = "CET")) %>% dplyr::rename(Date = .data$Week)
  },
  daily = {
    data %>% dplyr::mutate(Year = lubridate::year(.data$DatetimeBegin), Month = lubridate::month(.data$DatetimeBegin, label = T), Day = lubridate::day(.data$DatetimeBegin)) %>%
      dplyr::group_by(.data$AirQualityStationEoICode, .data$AirQualityStationName, .data$Year, .data$Month, .data$Day) %>% dplyr::summarise(summ = my_summarise(stats::na.omit(get(pollutant)), aggr_fun)) %>%
      dplyr::mutate(name = aggr_fun) %>% dplyr::ungroup() %>% tidyr::pivot_wider(names_from = "name", values_from = "summ") %>% dplyr::mutate(Date = lubridate::ymd(paste(.data$Year, .data$Month, .data$Day))) %>% dplyr::select(-"Year", -"Month", -"Day") %>%
      dplyr::relocate(.data$Date, .after = .data$AirQualityStationName)
  },
  hourly = {
    data %>% dplyr::mutate(Year = lubridate::year(.data$DatetimeBegin), Month = lubridate::month(.data$DatetimeBegin, label = T), Day = lubridate::day(.data$DatetimeBegin), Hour = lubridate::hour(.data$DatetimeBegin)) %>%
      dplyr::group_by(.data$AirQualityStationEoICode, .data$AirQualityStationName, .data$Year, .data$Month, .data$Day, .data$Hour) %>% dplyr::summarise(summ = my_summarise(stats::na.omit(get(pollutant)), aggr_fun)) %>%
      dplyr::mutate(name = aggr_fun) %>% dplyr::ungroup() %>% tidyr::pivot_wider(names_from = "name", values_from = "summ") %>% dplyr::mutate(Date = lubridate::ymd_h(paste(.data$Year, .data$Month, .data$Day, .data$Hour))) %>% dplyr::select(-"Year", -"Month", -"Day", -"Hour") %>%
      dplyr::relocate(.data$Date, .after = .data$AirQualityStationName)
  }
  )}, data = data, frequency = frequency) %>% suppressMessages() %>% suppressWarnings()

  #Unisco le tabelle di aggregazione di ogni pollutant in un unico dataframe

  t_aggr <- dplyr::select(data_aggr[[1]], "AirQualityStationEoICode", "AirQualityStationName", "Date")
  for (i in 1:length(data_aggr)) {
    if("quantile" %in% substr(aggr_fun, 1, 8)) {
      colnames(data_aggr[[i]]) <- c("AirQualityStationEoICode", "AirQualityStationName", "Date", aggr_fun[-which(substr(aggr_fun, 1, 8) == "quantile")], aggr_fun[which(substr(aggr_fun, 1, 8) == "quantile")])
    }

    data_aggr[[i]][sapply(data_aggr[[i]], is.nan)] <- NA
    data_aggr[[i]][sapply(data_aggr[[i]], is.infinite)] <- NA
    t_aggr <- dplyr::left_join(t_aggr, data_aggr[[i]], by = c("AirQualityStationEoICode", "AirQualityStationName", "Date"))
  }

  #Se ci sono piu' pollutants, restituisco un dataframe che contiene tutti gli inquinanti e uno per ognni inquinante
  if(length(pollutant) > 1) {
    #colnames(t_aggr) <- c("AirQualityStationEoICode", "AirQualityStationName", "Date", expand.grid(pollutant, aggr_fun) %>% dplyr::arrange(.data$Var1) %>% tidyr::unite("names", "Var1":"Var2", sep = "_") %>% purrr::as_vector())
    colnames(t_aggr) <- c("AirQualityStationEoICode", "AirQualityStationName", "Date", tidyr::unite(dplyr::arrange(expand.grid(pollutant, aggr_fun), .data$Var1), "names", "Var1":"Var2", sep = "_")[,1])
    output <- list()
    output$TimeAggr <- t_aggr
    output$TimeAggr_byPollutant <- data_aggr
    names(output$TimeAggr_byPollutant) <- pollutant
    if("EEAaq_df_sfc" %notin% class(data)) {
      attr(output$TimeAggr, "class") <- c("EEAaq_taggr_df", "tbl_df", "tbl", "data.frame")
      attr(output$TimeAggr, "frequency") <- frequency
      attr(output$TimeAggr, "NUTS_level") <- attributes(data)$NUTS_level
      attr(output$TimeAggr,"zone_name") <- attributes(data)$zone_name
      attr(output$TimeAggr, "pollutants") <- attributes(data)$pollutants
      for (i in 1:length(output$TimeAggr_byPollutant)) {
        attr(output$TimeAggr_byPollutant[[i]], "class") <- c("EEAaq_taggr_df", "tbl_df", "tbl", "data.frame")
        attr(output$TimeAggr_byPollutant[[i]], "frequency") <- frequency
        attr(output$TimeAggr_byPollutant[[i]], "NUTS_level") <- attributes(data)$NUTS_level
        attr(output$TimeAggr_byPollutant[[i]],"zone_name") <- attributes(data)$zone_name
        attr(output$TimeAggr_byPollutant[[i]], "pollutants") <- attributes(data)$pollutants
      }
      attr(output, "class") <- c("list", "EEAaq_taggr_list")
    } else if("EEAaq_df_sfc" %in% class(data)) {
      attr(output$TimeAggr, "class") <- c("EEAaq_taggr_df_sfc", "tbl_df", "tbl", "data.frame")
      attr(output$TimeAggr, "frequency") <- frequency
      attr(output$TimeAggr,"zone_geometry") <- attributes(data)$zone_geometry
      attr(output$TimeAggr, "pollutants") <- pollutant
      for (i in 1:length(output$TimeAggr_byPollutant)) {
        attr(output$TimeAggr_byPollutant[[i]], "class") <- c("EEAaq_taggr_df_sfc", "tbl_df", "tbl", "data.frame")
        attr(output$TimeAggr_byPollutant[[i]], "frequency") <- frequency
        attr(output$TimeAggr_byPollutant[[i]],"zone_geometry") <- attributes(data)$zone_geometry
        attr(output$TimeAggr_byPollutant[[i]], "pollutants") <- attributes(data)$pollutants
      }
      attr(output, "class") <- c("list", "EEAaq_taggr_list")
    }
  } else {
    output <- data_aggr[[1]]
    if("EEAaq_df_sfc" %notin% class(data)) {
      attr(output, "class") <- c("EEAaq_taggr_df", "tbl_df", "tbl", "data.frame")
      attr(output, "frequency") <- frequency
      attr(output, "NUTS_level") <- attributes(data)$NUTS_level
      attr(output,"zone_name") <- attributes(data)$zone_name
      attr(output, "pollutants") <- attributes(data)$pollutants
    } else if("EEAaq_df_sfc" %in% class(data)) {
      attr(output, "class") <- c("EEAaq_taggr_df_sfc", "tbl_df", "tbl", "data.frame")
      attr(output, "frequency") <- frequency
      attr(output,"zone_geometry") <- attributes(data)$zone_geometry
      attr(output, "pollutants") <- pollutant
    }
  }
  attributes(output)

  return(output)
}



#' Aggregate data based on a specific statistic
#'
#' Given data and the aggregation function desired, this function compute a time aggregation of the data.
#' @param data An \code{EEAaq_df} or \code{EEAaq_df_sfc} class object, which is the output of the
#' \code{\link{EEAaq_get_data}} function.
#' @param fun_aggr Vector character containing the aggregation function for which to time aggregate.
#' @return A tibble with the required aggregation.


my_summarise <- function(data, fun_aggr) {

  if("quantile" %in% substr(fun_aggr, 1, 8)) {
    q <- as.numeric(substr(fun_aggr[which(substr(fun_aggr, 1, 8) == "quantile")], 10, nchar(fun_aggr[which(substr(fun_aggr, 1, 8) == "quantile")])))
    unlist(c(lapply(fun_aggr[-which(substr(fun_aggr, 1, 8) == "quantile")], do.call, args = list(data)), list(stats::quantile(data, probs = q))))
  } else {
    unlist(lapply(fun_aggr, do.call, args = list(data)))
  }
}


