#' Convert daily climate projections to hourly time-step
#'
#' @param df data frame output by `get_climate_projections()` that has daily values for the variable
#' @param latitude latitude degree north
#' @param longitude longitude degree east
#'
#' @return data frame with an hourly time step
#' @export
#' @examplesIf interactive()
#' get_climate_projections(
#' latitude = 37.30,
#' longitude = -79.83,
#' start_date = Sys.Date(),
#' end_date = Sys.Date() + lubridate::years(1),
#' model = "EC_Earth3P_HR",
#' variables = glm_variables(product = "climate_projection", time_step = "daily")) |>
#' daily_to_hourly(latitude = 37.30, longitude = -79.83)
#'
daily_to_hourly <- function(df, latitude, longitude){

  variables <- unique(df$variable)

  if(!("shortwave_radiation_sum" %in% variables)) warning("missing shortwave_radiation_sum")
  if(!("temperature_2m_mean" %in% variables)) warning("missing temperature_2m_mean")
  if(!("precipitation_sum" %in% variables)) warning("missing precipitation_sum")
  if(!("wind_speed_10m_mean" %in% variables)) warning("missing wind_speed_10m_mean")
  if(!("relative_humidity_2m_mean" %in% variables)) warning("missing relative_humidity_2m_mean")
  if(!("cloud_cover_mean" %in% variables)) warning("missing cloud_cover_mean")

  units <- dplyr::tibble(variable = c("shortwave_radiation", "temperature_2m", "precipitation",
                                      "wind_speed_10m", "relative_humidity_2m", "cloud_cover"),
                         unit = c("W/m2","C","mm/hr","m/s","%","%"))


  ensemble_maxtime <- df |>
    dplyr::group_by(site_id, model_id) |>
    dplyr::summarise(max_time = max(datetime), .groups = "drop")

  df1 <- df |>
    dplyr::mutate(datetime = lubridate::as_datetime(datetime))


  datetime <- seq(min(df1$datetime), max(df1$datetime), by = "1 hour")
  sites <- unique(df$site_id)
  model_id <- unique(df$model_id)

  full_time <- expand.grid(sites, datetime, model_id) |>
    dplyr::rename(site_id = Var1,
                  datetime = Var2,
                  model_id = Var3) |>
    dplyr::mutate(datetime = lubridate::as_datetime(datetime)) |>
    dplyr::arrange(site_id, model_id, datetime) |>
    dplyr::left_join(ensemble_maxtime, by = c("site_id","model_id")) |>
    dplyr::filter(datetime <= max_time) |>
    dplyr::select(-c("max_time"))

  df2 <- df1 |>
    dplyr::select(-unit) |>
    tidyr::pivot_wider(names_from = variable, values_from = prediction) |>
    dplyr::right_join(full_time, by = c("site_id", "model_id", "datetime")) |>
    dplyr::arrange(site_id, datetime) |>
    dplyr::group_by(site_id)  |>
    tidyr::fill(c("precipitation_sum"), .direction = "down") |>
    tidyr::fill(c("shortwave_radiation_sum"), .direction = "down") |>
    tidyr::fill(c("relative_humidity_2m_mean"), .direction = "down") |>
    tidyr::fill(c("wind_speed_10m_mean"), .direction = "down") |>
    tidyr::fill(c("cloud_cover_mean"), .direction = "down") |>
    tidyr::fill(c("temperature_2m_mean"), .direction = "down") |>
    dplyr::mutate(precipitation_sum = precipitation_sum/24,
                  shortwave_radiation_sum = (shortwave_radiation_sum/86400)*1000000) |>
    dplyr::rename(precipitation = precipitation_sum,
                  shortwave_radiation = shortwave_radiation_sum,
                  relative_humidity_2m = relative_humidity_2m_mean,
                  wind_speed_10m = wind_speed_10m_mean,
                  cloud_cover = cloud_cover_mean,
                  temperature_2m = temperature_2m_mean) |>
    tidyr::pivot_longer(-c("site_id", "model_id","datetime"), names_to = "variable", values_to = "prediction")

  var_order <- names(df2)


  df2 <- df2 |>
    dplyr::mutate(shifted_datetime = datetime - lubridate::hours(1)) |>
    dplyr::mutate(hour = lubridate::hour(datetime),
                  date = lubridate::as_date(datetime),
                  doy = lubridate::yday(datetime) + hour/24,
                  lon = ifelse(longitude < 0, 360 + longitude,longitude),
                  rpot = downscale_solar_geom(doy, lon, latitude)) |>  # hourly sw flux calculated using solar geometry
    dplyr::select(-shifted_datetime) |>
    dplyr::group_by(site_id, date, variable) |>
    dplyr::mutate(avg.rpot = mean(rpot, na.rm = TRUE),
                  avg.SW = mean(prediction, na.rm = TRUE))|> # daily sw mean from solar geometry
    dplyr::ungroup() |>
    dplyr::mutate(prediction = ifelse(variable %in% c("shortwave_radiation","surface_downwelling_shortwave_flux_in_air") & avg.rpot > 0.0, rpot * (avg.SW/avg.rpot),prediction)) |>
    dplyr::select(dplyr::any_of(var_order)) |>
    dplyr::left_join(units, by = "variable")

  return(df2)

}
