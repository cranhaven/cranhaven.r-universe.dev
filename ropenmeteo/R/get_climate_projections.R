#' Download point-level climate projections using open-meteo API
#'
#' @param latitude latitude degree north
#' @param longitude  longitude degree east
#' @param site_id name of site location (optional, default = NULL)
#' @param start_date Number of days in the future for forecast (starts at current day)
#' @param end_date Number of days in the past to include in the data
#' @param model id of forest model https://open-meteo.com/en/docs/climate-api
#' @param variables vector of name of variable(s) https://open-meteo.com/en/docs/climate-api
#'
#' @returns data frame with the results from the call to the open-meteo API.  The data frame is in a long format and has the following columns: "datetime", "reference_datetime", "site_id", "model_id", "ensemble", "variable", "prediction","unit".
#' @export
#' @examplesIf interactive()
#'
#' get_climate_projections(
#' latitude = 37.30,
#' longitude = -79.83,
#' start_date = Sys.Date(),
#' end_date = Sys.Date() + lubridate::years(1),
#' model = "EC_Earth3P_HR",
#' variables = c("temperature_2m_mean"))
#'
get_climate_projections <- function(latitude,
                                    longitude,
                                    site_id = NULL,
                                    start_date,
                                    end_date,
                                    model = "EC_Earth3P_HR",
                                    variables = c("temperature_2m_mean")){

  if(start_date < "1950-01-01") warning("start date must be on or after 1950-01-01")
  #if(end_date > Sys.Date() - lubridate::days(5))

  latitude <- round(latitude, 2)
  longitude <- round(longitude, 2)

  if(longitude > 180) longitude <- longitude - 360

  df <- NULL
  units <- NULL
  for (variable in variables) {

    url_base <- "https://climate-api.open-meteo.com/v1/climate"
    url_path <-  glue::glue(
      "?latitude={latitude}&longitude={longitude}&start_date={start_date}&end_date={end_date}&daily={variable}&windspeed_unit=ms&models={model}"
    )
    v <- read_url(url_base, url_path)

    units <- dplyr::bind_rows(units, dplyr::tibble(variable = names(v$daily)[2], unit = unlist(v$daily_units[2][1])))
    v1  <- dplyr::as_tibble(v$daily)
    if (variable != variables[1]) {
      v1 <- dplyr::select(v1, -time)
    }
    df <- dplyr::bind_cols(df, v1)
  }

  df <-
    df |> tidyr::pivot_longer(-time, names_to = "variable", values_to = "prediction") |>
    dplyr::rename(datetime = time) |>
    dplyr::mutate(
      model_id = model) |>
    dplyr::left_join(units, by = "variable") |>
    dplyr::mutate(datetime = lubridate::as_date(datetime)) |>
    dplyr::mutate(site_id = ifelse(is.null(site_id), paste0(latitude,"_",longitude), site_id)) |>
    dplyr::select(c("datetime", "site_id", "model_id", "variable", "prediction","unit"))

  return(df)
}
