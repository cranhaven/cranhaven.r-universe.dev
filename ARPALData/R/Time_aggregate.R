#' Aggregate any ARPALdf object (with higher temporal frequency) to hourly, daily, weekly, monthly and yearly
#' temporal frequencies.
#'
#' @description Starting from an ARPALdf object with high frequency (e.g., 10mins or hourly), 'Time_aggregate'
#' aggregates the dataset to lower temporal frequencies (e.g., hourly, daily, weekly, monthly and yearly) by station.
#' The output is an ARPALdf object with observations having hourly, daily, weekly, monthly or yearly frequency.
#' The function can be applied only to ARPALdf objects.
#' User can indicate specific variables to aggregate and an aggregation function among
#' mean, median, sum (cumulated), min, max, quantiles, and variability metrics for each variable.
#' It is possible to specify different aggregation functions on the same variable
#' by repeating the name of the variable in 'Var_vec' and specifying the functions in 'Fns_vec'.
#'
#' @param Dataset ARPALdf dataframe to aggregate.
#' @param Frequency Temporal aggregation frequency. It can be "hourly", "daily", "weekly",
#' "monthly" or "yearly.
#' @param Var_vec Vector of variables to aggregate. If NULL (default) all the variables are averaged,
#' expect for 'Temperature' and 'Snow_height' which are summed.
#' @param Fns_vec Vector of aggregation functions to apply to the selected variables. Available functions
#' are 'mean', 'median', 'min', 'max', 'sum', 'qPP' (PP-th percentile), 'sd', 'var', 'vc' (variability coefficient),
#' 'skew' (skewness) and 'kurt' (kurtosis). Attention: for Wind Speed and Wind Speed Gust only mean, min
#' and max are available; for Wind Direction and Wind Direction Gust only mean is available.
#' @param verbose Logic value (TRUE or FALSE). Toggle warnings and messages. If 'verbose=T' (default) the function
#' prints on the screen some messages describing the progress of the tasks. If 'verbose=F' any message about
#' the progression is suppressed.
#'
#' @return A data frame
#'
#' @examples
#' \donttest{
#' ## Download hourly observed concentrations during 2020 for station 501 (Milano - Via Marche).
#' if (require("RSocrata")) {
#'     data <- get_ARPA_Lombardia_AQ_data(ID_station=501, Date_begin = "2020-01-01",
#'                                        Date_end = "2020-12-31", Frequency="hourly")
#' }
#' ## Aggregate all the data to daily frequency
#' Time_aggregate(Dataset=data,Frequency="daily",Var_vec=NULL,Fns_vec=NULL)
#' ## Aggregate NO2 to weekly maximum concentrations and NOx to weekly minimum concentrations.
#' Time_aggregate(Dataset=data,Frequency="weekly",Var_vec=c("NO2","NOx"),Fns_vec=c("max","min"))
#' }
#'
#' @export

Time_aggregate <- function(Dataset, Frequency, Var_vec = NULL, Fns_vec = NULL, verbose = T) {

  ### Checks
  stopifnot("Data is not of class 'ARPALdf'" = is_ARPALdf(Data = Dataset) == T)

  if (is.null(Var_vec) & is.null(Fns_vec)) {
    vv <- c("Ammonia","Arsenic","Benzene","Benzo_a_pyrene","BlackCarbon","Cadmium",
            "CO","Lead","Nikel","NO","NO2","NOx","Ozone","PM_tot","PM10","PM2.5","Sulfur_dioxide",
            "Rainfall","Temperature","Relative_humidity","Global_radiation","Water_height",
            "Snow_height","Wind_speed","Wind_speed_max","Wind_direction","Wind_direction_max",
            "NO2_mean","NO2_max_day","Ozone_max_8h","Ozone_max_day","PM10_mean","PM2.5_mean")
    vv <- vv[vv %in% names(Dataset)]
    fv <- ifelse(vv == "Rainfall" | vv == "Snow_height", "sum", "mean")
  } else {
    vv <- Var_vec
    fv <- Fns_vec
  }

  data_aggr <- switch(Frequency,
                      yearly = {
                        # Aggregation to yearly data
                        Dataset %>%
                          data.frame() %>%
                          dplyr::mutate(Y = lubridate::year(.data$Date)) %>%
                          dplyr::group_by(.data$Y, .data$NameStation, .data$IDStation) %>%
                          Custom_summarise(vv, fv) %>%
                          dplyr::ungroup() %>%
                          dplyr::mutate(Date = lubridate::make_datetime(year = .data$Y)) %>%
                          dplyr::select(-c(.data$Y)) %>%
                          dplyr::relocate(.data$Date,.data$IDStation) %>%
                          dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric),
                                                      ~ ifelse(is.nan(.), NA, .))) %>%
                          dplyr::mutate(dplyr::across(tidyselect::matches(c("Wind_direction","Wind_direction_max")), ~ round(.x,0)))},
                      monthly = {
                        # Aggregation to monthly data
                        Dataset %>%
                          data.frame() %>%
                          dplyr::mutate(Y = lubridate::year(.data$Date),
                                        M = lubridate::month(.data$Date)) %>%
                          dplyr::group_by(.data$Y, .data$M, .data$NameStation, .data$IDStation) %>%
                          Custom_summarise(vv, fv) %>%
                          dplyr::ungroup() %>%
                          dplyr::mutate(Date = lubridate::make_datetime(year = .data$Y,
                                                                        month = .data$M)) %>%
                          dplyr::select(-c(.data$Y,.data$M)) %>%
                          dplyr::relocate(.data$Date,.data$IDStation) %>%
                          dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric),
                                                      ~ ifelse(is.nan(.), NA, .))) %>%
                          dplyr::mutate(dplyr::across(tidyselect::matches(c("Wind_direction","Wind_direction_max")), ~ round(.x,0)))},
                      weekly = {
                        # Aggregation to weekly data
                        Dataset %>%
                          data.frame() %>%
                          dplyr::mutate(Y = lubridate::year(.data$Date),
                                        M = lubridate::month(.data$Date),
                                        D = lubridate::day(.data$Date),
                                        W = aweek::date2week(ISOdate(year = .data$Y,
                                                                     month = .data$M,
                                                                     day = .data$D),factor = T)) %>%
                          dplyr::group_by(.data$W, .data$NameStation, .data$IDStation) %>%
                          Custom_summarise(vv, fv) %>%
                          dplyr::ungroup() %>%
                          dplyr::mutate(Date = aweek::week2date(.data$W),
                                        Date = lubridate::make_datetime(year = lubridate::year(.data$Date),
                                                                        month = lubridate::month(.data$Date),
                                                                        day = lubridate::day(.data$Date))) %>%
                          dplyr::select(-c(.data$W)) %>%
                          dplyr::relocate(.data$Date,.data$IDStation) %>%
                          dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric),
                                                      ~ ifelse(is.nan(.), NA, .))) %>%
                          dplyr::mutate(dplyr::across(tidyselect::matches(c("Wind_direction","Wind_direction_max")), ~ round(.x,0)))},
                      # Aggregation to daily data
                      daily = {
                        Dataset %>%
                          data.frame() %>%
                          dplyr::mutate(Y = lubridate::year(.data$Date),
                                        M = lubridate::month(.data$Date),
                                        D = lubridate::day(.data$Date)) %>%
                          dplyr::group_by(.data$Y, .data$M, .data$D, .data$NameStation,.data$IDStation) %>%
                          Excess_na_converter(verbose=verbose) %>%
                          Custom_summarise(vv, fv) %>%
                          dplyr::ungroup() %>%
                          dplyr::mutate(Date = lubridate::make_datetime(year = .data$Y,
                                                                        month = .data$M,
                                                                        day = .data$D)) %>%
                          dplyr::select(-c(.data$Y,.data$M,.data$D)) %>%
                          dplyr::relocate(.data$Date,.data$IDStation) %>%
                          dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric),
                                                      ~ ifelse(is.nan(.), NA, .))) %>%
                          dplyr::mutate(dplyr::across(tidyselect::matches(c("Wind_direction","Wind_direction_max")), ~ round(.x,0)))},
                      # Aggregation to hourly data
                      hourly = {
                        Dataset %>%
                          data.frame() %>%
                          dplyr::mutate(Y = lubridate::year(.data$Date),
                                        M = lubridate::month(.data$Date),
                                        D = lubridate::day(.data$Date),
                                        H = lubridate::hour(.data$Date)) %>%
                          dplyr::group_by(.data$Y, .data$M, .data$D, .data$H,
                                          .data$NameStation, .data$IDStation) %>%
                          Custom_summarise(vv, fv) %>%
                          dplyr::ungroup() %>%
                          dplyr::mutate(Date = lubridate::make_datetime(year = .data$Y,
                                                                        month = .data$M,
                                                                        day = .data$D,
                                                                        hour = .data$H)) %>%
                          dplyr::select(-c(.data$Y,.data$M,.data$D,.data$H)) %>%
                          dplyr::relocate(.data$Date,.data$IDStation) %>%
                          dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric),
                                                      ~ ifelse(is.nan(.), NA, .))) %>%
                          dplyr::mutate(dplyr::across(tidyselect::matches(c("Wind_direction","Wind_direction_max")), ~ round(.x,0)))})

  attr(data_aggr, "frequency") <- Frequency
  freq_unit <- dplyr::case_when(Frequency == "hourly" ~ "hours",
                                Frequency == "daily" ~ "days",
                                Frequency == "weekly" ~ "weeks",
                                Frequency == "monthly" ~ "months",
                                Frequency == "yearly" ~ "years")
  attr(data_aggr, "units") <- freq_unit

  structure(list(data_aggr = data_aggr))
  if (is_ARPALdf_AQ(Dataset)==TRUE) {
    attr(data_aggr, "class") <- c("ARPALdf","ARPALdf_AQ","tbl_df","tbl","data.frame")
  }
  if (is_ARPALdf_AQ_mun(Dataset)==TRUE) {
    attr(data_aggr, "class") <- c("ARPALdf","ARPALdf_AQ_mun","tbl_df","tbl","data.frame")
  }
  if (is_ARPALdf_W(Dataset)==TRUE) {
    attr(data_aggr, "class") <- c("ARPALdf","ARPALdf_W","tbl_df","tbl","data.frame")
  }

  return(data_aggr)
}
