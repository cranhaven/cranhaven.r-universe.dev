#' Download point-level historical weather (ERA5) using open-meteo API
#'
#' @param latitude latitude degree north
#' @param longitude longitude degree east
#' @param site_id name of site location (optional, default = NULL)
#' @param start_date earliest date requested. Must be on or after 1950-01-01
#' @param end_date latest date requested
#' @param variables vector of name of variable(s) https://open-meteo.com/en/docs/ensemble-api
#'
#' @returns data frame with the results from the call to the open-meteo API.  The data frame is in a long format and has the following columns: "datetime", "site_id", "model_id", "variable", "prediction","unit".
#' @export
#' @examplesIf interactive()
#' get_historical_weather(
#' latitude = 37.30,
#' longitude = -79.83,
#' start_date = "2023-01-01",
#' end_date = Sys.Date(),
#' variables = c("temperature_2m"))
#'
get_historical_weather <- function(latitude,
                                  longitude,
                                  site_id = NULL,
                                  start_date,
                                  end_date,
                                  variables = c("relative_humidity_2m",
                                                "precipitation",
                                                "wind_speed_10m",
                                                "cloud_cover",
                                                "temperature_2m",
                                                "shortwave_radiation")){

  if(start_date < "1950-01-01") warning("start date must be on or after 1950-01-01")
  #if(end_date > Sys.Date() - lubridate::days(5))


  latitude <- round(latitude, 2)
  longitude <- round(longitude, 2)

  if(longitude > 180) longitude <- longitude - 360

  df <- NULL
  units <- NULL
  url_base <- "https://archive-api.open-meteo.com/v1/archive"
  for (variable in variables) {

    url_path <-  glue::glue(
      "?latitude={latitude}&longitude={longitude}&start_date={start_date}&end_date={end_date}&hourly={variable}&windspeed_unit=ms"
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

  df <-
    df |> tidyr::pivot_longer(-time, names_to = "variable", values_to = "prediction") |>
    dplyr::rename(datetime = time) |>
    dplyr::mutate(
      model_id = "ERA5") |>
    dplyr::left_join(units, by = "variable") |>
    dplyr::mutate(site_id = ifelse(is.null(site_id), paste0(latitude,"_",longitude), site_id)) |>
    dplyr::select(c("datetime", "site_id", "model_id", "variable", "prediction","unit"))


  return(df)
}





