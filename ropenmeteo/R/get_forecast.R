#' Download point-level ensemble weather forecasting using open-meteo API
#'
#' @param latitude latitude degree north
#' @param longitude longitude degree east
#' @param site_id name of site location (optional, default = NULL)
#' @param forecast_days Number of days in the future for forecast (starts at current day)
#' @param past_days Number of days in the past to include in the data
#' @param model id of forest model https://open-meteo.com/en/docs/climate-api. Default = "generic"
#' @param variables vector of name of variable(s) https://open-meteo.com/en/docs/ensemble-api.
#'
#' @return data frame (in long format)
#' @export
#'
#' @examplesIf interactive()
#'get_forecast(latitude = 37.30,
#'             longitude = -79.83,
#'             forecast_days = 7,
#'             past_days = 2,
#'            model = "generic",
#'             variables = c("temperature_2m"))
get_forecast <- function(latitude,
                         longitude,
                         site_id = NULL,
                         forecast_days,
                         past_days,
                         model = "generic",
                         variables = c("temperature_2m")){

  if(forecast_days > 35) stop("forecast_days is longer than avialable (max = 35")
  if(past_days > 92) stop("hist_days is longer than avialable (max = 92)")

  api <- switch(model,
                "generic" = "/v1/forecast",
                "metno" = "/v1/metno",
                "dwd" = "/v1/dwd",
                "gfs" = "/v1/gfs",
                "meteofrance" = "/v1/meteofrance",
                "ecmwf" = "/v1/ecmwf",
                "jma"= "/v1/jma",
                "gem" = "/v1/gem")

  latitude <- round(latitude, 2)
  longitude <- round(longitude, 2)

  if(longitude > 180) longitude <- longitude - 360

  df <- NULL
  units <- NULL
  for (variable in variables) {

    url_base <- "https://api.open-meteo.com"
    url_path <-  glue::glue(
      "{api}?latitude={latitude}&longitude={longitude}&hourly={variable}&windspeed_unit=ms&forecast_days={forecast_days}&past_days={past_days}"
    )
    v <- read_url(url_base, url_path)

    units <- dplyr::bind_rows(units, dplyr::tibble(variable = names(v$hourly)[2], unit = unlist(v$hourly_units[2][1])))
    v1  <- dplyr::as_tibble(v$hourly) |>
      dplyr::mutate(time = lubridate::as_datetime(paste0(time,":00")))
    if (variable != variables[1]) {
      v1 <- dplyr::select(v1, -time)
    }
    df <- dplyr::bind_cols(df, v1)
  }

  df <- df |>
    tidyr::pivot_longer(-time, names_to = "variable", values_to = "prediction") |>
    dplyr::rename(datetime = time) |>
    dplyr::mutate( model_id = model,
                   reference_datetime = min(datetime) + lubridate::days(past_days)) |>
    dplyr::left_join(units, by = "variable") |>
    dplyr::mutate(site_id = ifelse(is.null(site_id), paste0(latitude,"_",longitude), site_id)) |>
    dplyr::select(c("datetime", "reference_datetime", "site_id", "model_id", "variable", "prediction","unit"))

  return(df)
}





