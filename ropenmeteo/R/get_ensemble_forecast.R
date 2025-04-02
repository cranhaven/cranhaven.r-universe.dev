#' Download point-level ensemble weather forecasting using open-meteo API
#'
#' @param latitude latitude degree north
#' @param longitude longitude degree east
#' @param site_id name of site location (optional, default = NULL)
#' @param forecast_days Number of days in the future for forecast (starts at current day)
#' @param past_days Number of days in the past to include in the data
#' @param model id of forest model https://open-meteo.com/en/docs/ensemble-api
#' @param variables vector of name of variable(s) https://open-meteo.com/en/docs/ensemble-api
#'
#' @returns data frame with the results from the call to the open-meteo API.  The data frame is in a long format and has the following columns: "datetime", "reference_datetime", "site_id", "model_id", "ensemble", "variable", "prediction","unit".
#' @export
#'
#' @examplesIf interactive()
#' get_ensemble_forecast(
#' latitude = 37.30,
#' longitude = -79.83,
#' forecast_days = 7,
#' past_days = 2,
#' model = "gfs_seamless",
#' variables = c("temperature_2m"))
#'
#'
get_ensemble_forecast <- function(latitude,
                                  longitude,
                                  site_id = NULL,
                                  forecast_days,
                                  past_days,
                                  model = "gfs_seamless",
                                  variables = c("relative_humidity_2m",
                                                "precipitation",
                                                "wind_speed_10m",
                                                "cloud_cover",
                                                "temperature_2m",
                                                "shortwave_radiation")){

  if(forecast_days > 35) stop("forecast_days is longer than avialable (max = 35")
  if(past_days > 92) stop("hist_days is longer than avialable (max = 92)")

  latitude <- round(latitude, 2)
  longitude <- round(longitude, 2)

  if(longitude > 180) longitude <- longitude - 360

  if("shortwave_radiation" %in% variables & model == "ecmwf_ifs04"){
    message("shortwave radiation is not aviailable for ecmwf_ifs04 model")
  }

  variables_api <- paste(variables,collapse=",")

  url_base <- "https://ensemble-api.open-meteo.com/v1/ensemble"
  url_path <-  glue::glue(
    "?latitude={latitude}&longitude={longitude}&hourly={variables_api}&windspeed_unit=ms&forecast_days={forecast_days}&past_days={past_days}&models={model}"
  )
  v <- read_url(url_base, url_path)

  units <- dplyr::tibble(variable = stringr::str_split_i(names(v$hourly),"_member",1), unit = unlist(v$hourly_units)) |> dplyr::distinct() |> dplyr::filter(variable != "time")
  df  <- dplyr::as_tibble(v$hourly) |>
    dplyr::mutate(time = lubridate::as_datetime(paste0(time,":00")))  |>
    pivot_ensemble_forecast() |>
    dplyr::rename(datetime = time) |>
    dplyr::mutate(
      model_id = model,
      reference_datetime = min(datetime) + lubridate::days(past_days)
    ) |>
    dplyr::left_join(units, by = "variable") |>
    dplyr::mutate(site_id = ifelse(is.null(site_id), paste0(latitude,"_",longitude), site_id)) |>
    dplyr::select(c("datetime", "reference_datetime", "site_id", "model_id", "ensemble", "variable", "prediction","unit"))


  return(df)
}





