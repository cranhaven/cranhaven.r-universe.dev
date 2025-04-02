#' Convert 6 hour seasonal forecast to hourly time-step
#'
#' @param df data frame with 6-hour time step
#' @param latitude latitude degree north
#' @param longitude long longitude degree east
#' @param use_solar_geom use solar geometry to determine hourly solar radiation
#'
#' @returns data frame with an hourly time step
#' @export
#' @examplesIf interactive()
#' get_seasonal_forecast(
#' latitude = 37.30,
#' longitude = -79.83,
#' forecast_days = 30,
#' past_days = 5,
#' variables = glm_variables(product = "seasonal_forecast",
#'                          time_step = "6hourly")) |>
#' six_hourly_to_hourly(
#'     latitude = 37.30,
#'     longitude = -79.83,
#'     use_solar_geom = TRUE)
#'
six_hourly_to_hourly <- function(df, latitude, longitude, use_solar_geom = TRUE){

  variables <- unique(df$variable)

  if(!("shortwave_radiation" %in% variables)) warning("missing shortwave")
  if(!("temperature_2m" %in% variables)) warning("missing temperature")
  if(!("precipitation" %in% variables)) warning("missing precipitation")
  if(!("wind_speed_10m" %in% variables)) warning("missing wind_speed")
  if(!("relative_humidity_2m" %in% variables)) warning("missing relative_humidity")

  df <- df |>
    dplyr::filter(datetime <= max(df$datetime) - lubridate::hours(18)) #remove last day

  units <- df |> dplyr::distinct(variable, unit)

  ensemble_maxtime <- df |>
    dplyr::group_by(site_id, model_id, ensemble, reference_datetime) |>
    dplyr::summarise(max_time = max(datetime), .groups = "drop")

  ensembles <- unique(df$ensemble)
  datetime <- seq(min(df$datetime), max(df$datetime), by = "1 hour")
  reference_datetime <- unique(df$reference_datetime)
  sites <- unique(df$site_id)
  model_id <- unique(df$model_id)

  full_time <- expand.grid(sites, ensembles, datetime, reference_datetime, model_id) |>
    dplyr::rename(site_id = Var1,
                  ensemble = Var2,
                  datetime = Var3,
                  reference_datetime = Var4,
                  model_id = Var5) |>
    dplyr::mutate(datetime = lubridate::as_datetime(datetime)) |>
    dplyr::arrange(site_id, model_id, ensemble, reference_datetime, datetime) |>
    dplyr::left_join(ensemble_maxtime, by = c("site_id","ensemble", "model_id", "reference_datetime")) |>
    dplyr::filter(datetime <= max_time) |>
    dplyr::select(-c("max_time"))

  df1 <- df |>
    dplyr::select(-unit) |>
    tidyr::pivot_wider(names_from = variable, values_from = prediction) |>
    dplyr::right_join(full_time, by = c("site_id", "model_id", "ensemble", "datetime", "reference_datetime")) |>
    dplyr::arrange(site_id, ensemble, datetime) |>
    dplyr::group_by(site_id, ensemble)  |>
    tidyr::fill(c("precipitation"), .direction = "up") |>
    tidyr::fill(c("shortwave_radiation"), .direction = "up") |>
    dplyr::mutate(relative_humidity_2m =  imputeTS::na_interpolation(relative_humidity_2m, option = "linear"),
                  wind_speed_10m =  imputeTS::na_interpolation(wind_speed_10m, option = "linear"),
                  cloud_cover =  imputeTS::na_interpolation(cloud_cover, option = "linear"),
                  temperature_2m =  imputeTS::na_interpolation(temperature_2m, option = "linear"),
                  precipitation = precipitation/6) |>
    tidyr::pivot_longer(-c("site_id", "model_id", "ensemble", "datetime", "reference_datetime"), names_to = "variable", values_to = "prediction")

  #the first time step is the 6 hour sum from the previous day
  df1 <- df1 |>
    dplyr::mutate(prediction = ifelse(variable == "precipitation" & datetime == min(df1$datetime),
                               prediction/6,
                               prediction))

  var_order <- names(df1)

  if(use_solar_geom){

    df1 <- df1 |>
      #dplyr::filter(variable == "shortwave_radiation") |>
      dplyr::mutate(shifted_datetime = datetime - lubridate::hours(1)) |>
      dplyr::mutate(hour = lubridate::hour(shifted_datetime),
                    date = lubridate::as_date(shifted_datetime),
                    doy = lubridate::yday(shifted_datetime) + hour/24,
                    lon = ifelse(longitude < 0, 360 + longitude,longitude),
                    rpot = downscale_solar_geom(doy, lon, latitude)) |>  # hourly sw flux calculated using solar geometry
      dplyr::select(-shifted_datetime) |>
      dplyr::group_by(site_id, ensemble, reference_datetime, date, variable) |>
      dplyr::mutate(avg.rpot = mean(rpot, na.rm = TRUE),
                    avg.SW = mean(prediction, na.rm = TRUE))|> # daily sw mean from solar geometry
      dplyr::ungroup() |>
      dplyr::mutate(prediction = ifelse(variable %in% c("shortwave_radiation","surface_downwelling_shortwave_flux_in_air") & avg.rpot > 0.0, rpot * (avg.SW/avg.rpot),prediction)) |>
      dplyr::select(dplyr::any_of(var_order)) |>
      dplyr::left_join(units, by = "variable")
  }

  return(df1)

}
