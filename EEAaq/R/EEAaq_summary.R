#' Generate an \code{EEAaq_df} data summary.
#' This function must be applied to an \code{EEAaq_df} or \code{EEAaq_df_sfc} class object and produces a list of data frames,
#' containing relevant information about the data, such as descriptive statistics, missing values statistics,
#' gap length and linear correlation.
#'
#' @param data an \code{EEAaq_df} or \code{EEAaq_df_sfc} class object, which is the output of the \code{\link{EEAaq_get_data}} function.
#' @param verbose logic value (T or F). If \code{TRUE} (the default) messages about the function progress are printed.
#' If \code{FALSE} no message is printed.
#'
#' @return The function \code{EEAaq_summary} computes and return a list of summary statistics of the dataset given in
#' \code{data}. In particular the elements of the list are:
#' \itemize{
#' \item{\code{Summary} global missing count, missing rate, negative count, minimum, maximum,
#' mean and standard deviation, organized by pollutant.}
#' \item{\code{Summary_byStat} list of data frames, one for each different station, containing
#' the descriptive statistics (missing count, missing rate, negative count, minimum, maximum,
#' mean and standard deviation), organized by station.}
#' \item{\code{gap_length} one data frame for each pollutant, containing the gap length organized by station.}
#' \item{\code{Corr_Matrix} if \code{data} contains more than one pollutant, the correlation matrix between
#' pollutans is provided, organised by station.}
#' }
#'
#' @examples
#' \donttest{
#' `%>%` <- dplyr::`%>%`
#' ### Download PM10 data for the province (NUTS-3) of Milano (Italy)
#' ###   from January 1st to January 31st, 2023
#' IDstations <- EEAaq_get_stations(byStation = TRUE, complete = FALSE)
#' IDstations <- IDstations %>%
#'                 dplyr::filter(NUTS3 %in% c("Milano")) %>%
#'                 dplyr::pull(AirQualityStationEoICode) %>%
#'                 unique()
#' data <- EEAaq_get_data(IDstations = IDstations, pollutants = "PM10",
#'                        from = "2023-01-01", to = "2023-01-31",
#'                        verbose = TRUE)
#'
#' ### Compute summary statistics
#' EEAaq_summary(data)
#' }
#'
#' @export

EEAaq_summary <- function(data = NULL, verbose = TRUE) {

  `%>%` <- dplyr::`%>%`

  #Verifica connessione a internet
  if(!curl::has_internet()) {
    stop("Please check your internet connection. If the problem persists, please
         contact the package maintainer.")
  }

  stations <- EEAaq_get_stations()

  if(verbose == T) {
    cat(paste0("The dataset contains:\n ** ", nrow(data), " total observations \n ** ",
               length(unique(data$AirQualityStationEoICode)), " stations \n ** ",
               length(unique(data$DatetimeBegin)), " time stamps: from ", min(data$DatetimeBegin), " to ", max(data$DatetimeBegin), "\n"))

  }


  output <- list()
  pollutant <- attributes(data)$pollutant

  EEA_summ <- function(pollutant, data) {
    data_poll <- dplyr::pull(data, pollutant)
    row <- tibble::tibble(Pollutant = pollutant) %>% dplyr::mutate(NA_count = sum(is.na(data_poll)),
                                                                   NA_perc = sum(is.na(data_poll))/length(data_poll) * 100,
                                                                   negative_count = sum(stats::na.omit(data_poll) < 0),
                                                                   min = min(data_poll[data_poll >= 0], na.rm = T),
                                                                   mean = mean(data_poll, na.rm = T),
                                                                   max = max(data_poll, na.rm = T),
                                                                   sd = stats::sd(data_poll, na.rm = T))
  }

  output$Summary <- dplyr::bind_rows(lapply(pollutant, EEA_summ, data = data))

  #INFORMAZIONI STAZIONI:
  unique_stations <- tibble::tibble(AirQualityStationEoICode = unique(data$AirQualityStationEoICode), AirQualityStationName = unique(data$AirQualityStationName))
  output$Stations_info <- dplyr::left_join(unique_stations, stations, by = c("AirQualityStationEoICode", "AirQualityStationName"), multiple = "all") %>%
    dplyr::filter(.data$AirPollutant %in% pollutant) %>% dplyr::distinct(.data$AirQualityStationEoICode, .data$AirPollutant, .keep_all = T)

  #SUMMARY BY STATION

  NA_count_byStat <- dplyr::bind_cols(unique_stations,
                                      lapply(pollutant, function(pollutant, data) data %>% dplyr::group_by(.data$AirQualityStationEoICode) %>% dplyr::summarise(poll = sum(is.na(data[[pollutant]]))) %>% dplyr::select("poll"), data = data)) %>%
    suppressMessages()

  NA_perc_byStat <- dplyr::bind_cols(unique_stations,
                                     lapply(pollutant, function(pollutant, data) data %>% dplyr::group_by(.data$AirQualityStationEoICode) %>% dplyr::summarise(poll = sum(is.na(data[[pollutant]]))/length(data[[pollutant]]) * 100) %>% dplyr::select("poll"), data = data)) %>%
    suppressMessages()

  Negative_count_byStat <- dplyr::bind_cols(unique_stations,
                                            lapply(pollutant, function(pollutant, data) data %>% dplyr::group_by(.data$AirQualityStationEoICode) %>% dplyr::summarise(poll = sum(stats::na.omit(data[[pollutant]]) < 0)) %>% dplyr::select("poll"), data = data)) %>%
    suppressMessages()

  Min_byStat <- dplyr::bind_cols(unique_stations,
                                 lapply(pollutant, function(pollutant, data) data %>% dplyr::group_by(.data$AirQualityStationEoICode) %>% dplyr::summarise(poll = min(data[[pollutant]], na.rm = T)) %>% dplyr::select("poll"), data = data)) %>%
    suppressMessages() %>% suppressWarnings()
  Min_byStat[sapply(Min_byStat, is.infinite)] <- NA

  Mean_byStat <- dplyr::bind_cols(unique_stations,
                                  lapply(pollutant, function(pollutant, data) data %>% dplyr::group_by(.data$AirQualityStationEoICode) %>% dplyr::summarise(poll = mean(data[[pollutant]], na.rm = T)) %>% dplyr::select("poll"), data = data)) %>%
    suppressMessages()
  Mean_byStat[sapply(Mean_byStat, is.nan)] <- NA

  Max_byStat <- dplyr::bind_cols(unique_stations,
                                 lapply(pollutant, function(pollutant, data) data %>% dplyr::group_by(.data$AirQualityStationEoICode) %>% dplyr::summarise(poll = max(data[[pollutant]], na.rm = T)) %>% dplyr::select("poll"), data = data)) %>%
    suppressMessages() %>% suppressWarnings()
  Max_byStat[sapply(Max_byStat, is.infinite)] <- NA

  Sd_byStat <- dplyr::bind_cols(unique_stations,
                                lapply(pollutant, function(pollutant, data) data %>% dplyr::group_by(.data$AirQualityStationEoICode) %>% dplyr::summarise(poll = stats::sd(data[[pollutant]], na.rm = T)) %>% dplyr::select("poll"), data = data)) %>%
    suppressMessages()


  base <-  lapply(pollutant, function(pollutant, data) {data %>% dplyr::select(-"DatetimeBegin", -"DatetimeEnd") %>%
      dplyr::filter(!is.na(data[[pollutant]])) %>% dplyr::select("AirQualityStationEoICode", dplyr::contains(pollutant), "AveragingTime") %>%
      tidyr::pivot_longer(cols = pollutant, names_to = "AirPollutant") %>% dplyr::select(-"value") %>% dplyr::distinct(.data$AirQualityStationEoICode, .data$AveragingTime, .data$AirPollutant) %>%
      tidyr::pivot_wider(names_from = "AirPollutant", values_from = "AveragingTime", values_fn = ~paste(.x, collapse = ", "))}, data = data)
  AveragingTime_byStat <- dplyr::left_join(unique_stations, dplyr::distinct(data, .data$AirQualityStationEoICode, .data$AirQualityStationName), by = c("AirQualityStationEoICode", "AirQualityStationName"))
  for (i in 1:length(base)) {
    AveragingTime_byStat <- dplyr::left_join(AveragingTime_byStat, base[[i]], by = "AirQualityStationEoICode")
  }




  colnames(NA_count_byStat) <-  colnames(NA_perc_byStat) <- colnames(Negative_count_byStat) <-
    colnames(Min_byStat) <- colnames(Mean_byStat) <- colnames(Max_byStat) <- colnames(Sd_byStat) <-
    c("AirQualityStationEoICode", "AirQualityStationName", pollutant)

  #Aggiungo tutte le statistiche di sintesi
  output$Summary_byStat$NA_count_byStat <- NA_count_byStat
  output$Summary_byStat$NA_perc_byStat <- NA_perc_byStat
  output$Summary_byStat$Negative_count_byStat <- Negative_count_byStat
  output$Summary_byStat$Min_byStat <- Min_byStat
  output$Summary_byStat$Mean_byStat <- Mean_byStat
  output$Summary_byStat$Max_byStat <- Max_byStat
  output$Summary_byStat$Sd_byStat <- Sd_byStat
  output$Summary_byStat$AveragingTime_byStat <- AveragingTime_byStat

  #Creazione della tabella relativa alla gap length
  un_st <- unique(data$AirQualityStationEoICode)
  g_length <- function(pollutant, data) {
    for (i in 1:length(un_st)) {
      d_by_st <- data %>% dplyr::filter(.data$AirQualityStationEoICode == un_st[i])
      start <- d_by_st %>% dplyr::filter(.data$DatetimeBegin == min(d_by_st$DatetimeBegin)) %>% dplyr::slice(1)
      end <- d_by_st %>% dplyr::filter(.data$DatetimeBegin == max(d_by_st$DatetimeBegin)) %>% dplyr::slice(1)
      if(is.na(start[,pollutant])) {
        id <- which(data$AirQualityStationEoICode == un_st[i] & data$DatetimeBegin == min(d_by_st$DatetimeBegin))
        data[id,pollutant] <- 0
      }
      if(is.na(end[,pollutant])) {
        id <- which(data$AirQualityStationEoICode == un_st[i] & data$DatetimeBegin == max(d_by_st$DatetimeBegin))
        data[id,pollutant] <- 0
      }
    }

    data %>% dplyr::select("DatetimeBegin", "AirQualityStationEoICode", var = pollutant[1]) %>% dplyr::filter(!is.na(.data$var)) %>%
      dplyr::group_by(.data$AirQualityStationEoICode) %>%
      dplyr::summarise(.groups = "keep", gap = lubridate::interval(.data$DatetimeBegin, .data$DatetimeBegin[-1])) %>%
      dplyr::mutate(gap = lubridate::time_length(.data$gap, unit = "hour")) %>% dplyr::filter(.data$gap > 0) %>%
      dplyr::summarise(.groups = "keep", min_gap = min(.data$gap),
                       q25_gap = stats::quantile(.data$gap, probs = 0.25), mean_gap = round(mean(.data$gap), 3),
                       median_gap = stats::quantile(.data$gap, probs = 0.5), q75_gap = stats::quantile(.data$gap, probs = 0.75),
                       max_gap = max(.data$gap), sd_gap = round(stats::sd(.data$gap), 3)) %>% as.data.frame() %>%
      dplyr::mutate(dplyr::across(dplyr::contains("gap"), ~paste(.x, "hours"))) %>% suppressWarnings() %>% dplyr::tibble()
  }
  gl <- lapply(pollutant, g_length, data = data)
  names(gl) <- pollutant
  gl <- lapply(gl, function(x) {dplyr::left_join(x, unique_stations, by = "AirQualityStationEoICode") %>% dplyr::relocate(.data$AirQualityStationName, .after = .data$AirQualityStationEoICode)})
  output$gap_length <- gl


  if(length(pollutant) > 1) {
    combs <- subset(expand.grid(rep(list(pollutant),2)), Var1 != Var2) %>% dplyr::tibble() %>%
      dplyr::arrange(.data$Var1, .data$Var2) %>% dplyr::mutate(names = paste(.data$Var1, .data$Var2, sep = "_"))
    name_combs <- combs$names
    lista_comb <- list()
    lista_comb <- apply(combs[,1:2], 1, function(x) c(lista_comb, x))
    lista_comb <- lapply(lista_comb, unlist)

    cor_mat <- function(lista_comb, data) {

      data %>% dplyr::group_by(.data$AirQualityStationEoICode) %>% dplyr::summarise(corr = stats::cor(get(lista_comb[1]), get(lista_comb[2]), use = "pairwise.complete.obs")) %>%
        dplyr::select("corr")

    }
    corr <- dplyr::bind_cols(unique_stations, lapply(lista_comb, cor_mat, data = data)) %>% suppressMessages()
    colnames(corr) <- c("AirQualityStationEoICode", "AirQualityStationName", name_combs)
    output$Corr_Matrix <- corr
  }



  return(output)

}
