#' Write ensemble forecast dataframe to General Lake Model formatted csv files
#'
#' @param df data frame output by `get_ensemble_forecast()`
#' @param path directory where csv files will be written
#'
#' @export
#' @returns No return value, called to generate csv files in the GLM required format
#' @examples
#'
#' \dontshow{
#' # Hide setting tempfile, since a user would specify a persistent location
#'  path <- tempdir()
#' }
#'
#' file <- system.file("extdata", "test-data.csv", package="ropenmeteo")
#' df <- readr::read_csv(file, show_col_types = FALSE)
#' df |>
#'    add_longwave() |>
#'    write_glm_format(path = path)

#' \dontshow{
#' # tidy
#'  unlink(path)
#' }
write_glm_format <- function(df, path) {

  variables <- unique(df$variable)

  if(!("longwave_radiation" %in% variables)) warning("missing longwave")
  if(!("shortwave_radiation" %in% variables)) warning("missing shortwave")
  if(!("temperature_2m" %in% variables)) warning("missing temperature")
  if(!("precipitation" %in% variables)) warning("missing precipitation")
  if(!("wind_speed_10m" %in% variables)) warning("missing wind_speed")
  if(!("relative_humidity_2m" %in% variables)) warning("missing relative humidity")

  df <- df |> dplyr::mutate(prediction = round(prediction, 2))
  if("ensemble" %in% names(df)){
    ensemble_list <- df |> dplyr::distinct(model_id, ensemble)

  purrr::walk(1:nrow(ensemble_list),
              function(i, ensemble_list, df) {
                df |>
                  dplyr::mutate(datetime = ifelse(variable %in% c("longwave_radiation","shortwave_radiation", "precipitation"),
                                                  datetime - lubridate::hours(1),
                                                  datetime),
                                datetime = lubridate::as_datetime(datetime)) |>
                  dplyr::select(-unit) |>
                  dplyr::filter(model_id == ensemble_list$model_id[i],
                                    ensemble == ensemble_list$ensemble[i]) |>
                  tidyr::pivot_wider(names_from = variable, values_from = prediction) |>
                  dplyr::rename(
                    LongWave = longwave_radiation,
                    ShortWave = shortwave_radiation,
                    AirTemp = temperature_2m,
                    Rain = precipitation,
                    WindSpeed = wind_speed_10m,
                    RelHum = relative_humidity_2m,
                    time = datetime
                  ) |>
                  dplyr::mutate(Rain = Rain * 0.024) |>
                  dplyr::select(-dplyr::any_of(c("ensemble","model_id","site_id", "cloud_cover", "reference_datetime"))) |>
                  dplyr::select(time, AirTemp, ShortWave, LongWave, RelHum, WindSpeed, Rain) |>
                  dplyr::arrange(time) |>
                  dplyr::mutate(time = strftime(time, format = "%Y-%m-%d %H:%M", tz = "UTC")) |>
                  dplyr::slice(-1) |>
                  utils::write.csv(
                    file = file.path(
                      normalizePath(path),
                      paste0(
                        "met_",
                        ensemble_list$model_id[i],
                        "_",
                        ensemble_list$ensemble[i],
                        ".csv"
                      )
                    ),
                    quote = FALSE,
                    row.names = FALSE
                  )

              }, ensemble_list, df)
  }else{
    df |>
      dplyr::mutate(datetime = ifelse(variable %in% c("longwave_radiation","shortwave_radiation", "precipitation") & model_id != "ERA5",
                                      datetime - lubridate::hours(1),
                                      datetime),
                   datetime = lubridate::as_datetime(datetime)) |>
      dplyr::select(-unit) |>
      tidyr::pivot_wider(names_from = variable, values_from = prediction) |>
      dplyr::rename(
        LongWave = longwave_radiation,
        ShortWave = shortwave_radiation,
        AirTemp = temperature_2m,
        Rain = precipitation,
        WindSpeed = wind_speed_10m,
        RelHum = relative_humidity_2m,
        time = datetime) |>
      dplyr::mutate(Rain = Rain * 0.024) |>
      dplyr::select(-dplyr::any_of(c("ensemble","model_id","site_id", "cloud_cover", "reference_datetime"))) |>
      dplyr::select(time, AirTemp, ShortWave, LongWave, RelHum, WindSpeed, Rain) |>
      dplyr::arrange(time) |>
      dplyr::mutate(time = strftime(time, format = "%Y-%m-%d %H:%M", tz = "UTC")) |>
      utils::write.csv( file = file.path(
          normalizePath(path),
          paste0(
            "met.csv"
          )
        ),
        quote = FALSE,
        row.names = FALSE
      )
  }
}
