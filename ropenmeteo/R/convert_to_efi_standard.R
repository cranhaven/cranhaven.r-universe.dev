#' Convert units and names to CF and Ecological Forecasting Initiative standard
#'
#' Output units:
#' * air_temperature: K
#' * relative_humidity: proportion
#' * surface_downwelling_longwave_flux_in_air: W m-2
#' * surface_downwelling_shortwave_flux_in_air: W m-2
#' * precipitation_flux: kg m-2 s-1
#' * wind_speed: m s-1
#' * air_pressure: Pa
#' * cloud_cover: proportion
#'
#' @param df data frame output by get_ensemble_forecast
#'
#' @return data frame
#' @export
#' @examples
#' file <- system.file("extdata", "test-data.csv", package="ropenmeteo")
#' df <- readr::read_csv(file, show_col_types = FALSE)
#' df  |>
#'   add_longwave() |>
#'   convert_to_efi_standard()
#'
convert_to_efi_standard <- function(df){

  df <- df |>
    dplyr::mutate(variable = ifelse(variable == "temperature_2m", "air_temperature", variable),
           prediction = ifelse(variable == "air_temperature", prediction + 273.15, prediction),
           unit = ifelse(variable == "air_temperature", "K", unit),
           variable = ifelse(variable == "relative_humidity_2m", "relative_humidity", variable),
           prediction = ifelse(variable == "relative_humidity", prediction/100, prediction),
           unit = ifelse(variable == "relative_humidity", "proportion", unit),
           variable = ifelse(variable == "longwave", "surface_downwelling_longwave_flux_in_air", variable),
           variable = ifelse(variable == "shortwave_radiation", "surface_downwelling_shortwave_flux_in_air", variable),
           variable = ifelse(variable == "precipitation", "precipitation_flux", variable),
           prediction = ifelse(variable == "precipitation_flux", prediction/(60 * 60), prediction),
           unit = ifelse(variable == "precipitation_flux", "kg/m2/s", unit),
           variable = ifelse(variable == "wind_speed_10m", "wind_speed", variable),
           variable = ifelse(variable == "surface_pressure", "air_pressure", variable),
           prediction = ifelse(variable == "air_pressure", prediction * 100, prediction),
           unit = ifelse(variable == "air_pressure", "Pa", unit),
           variable = ifelse(variable == "cloud_area_fraction", "cloud_cover", variable),
           prediction = ifelse(variable == "cloud_cover", prediction/100, prediction),
           unit = ifelse(variable == "cloud_cover", "proportion", unit)) |>
    dplyr::mutate(family = "ensemble") |>
    dplyr::select(dplyr::any_of(c("datetime", "reference_datetime", "site_id", "model_id", "family", "ensemble", "parameter", "variable", "prediction")))

  if("ensemble" %in% names(df)) df <- df |> dplyr::rename(parameter = ensemble)

  return(df)
}
