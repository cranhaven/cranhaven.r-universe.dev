## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>"
)

## ----message = FALSE----------------------------------------------------------
library(ggplot2)
library(dplyr)
library(purrr)
library(tibble)
library(ropenmeteo)
library(lubridate)
library(readr)

## -----------------------------------------------------------------------------
df <- get_forecast(latitude = 37.30,
                   longitude = -79.83,
                   forecast_days = 7, 
                   past_days = 2, 
                   model = "generic",
                   variables = c("temperature_2m"))
head(df)

## -----------------------------------------------------------------------------
df |> 
  mutate(variable = paste(variable, unit)) |> 
  ggplot(aes(x = datetime, y = prediction)) + 
  geom_line(color = "#F8766D") + 
  geom_vline(aes(xintercept = reference_datetime)) + 
  facet_wrap(~variable, scale = "free")

## ----eval = FALSE-------------------------------------------------------------
# df <- get_ensemble_forecast(
#   latitude = 37.30,
#   longitude = -79.83,
#   forecast_days = 7,
#   past_days = 2,
#   model = "gfs_seamless",
#   variables = c("temperature_2m"))
# head(df)

## ----forecast-plot, eval=FALSE------------------------------------------------
# df |>
#   mutate(variable = paste(variable, unit)) |>
#   ggplot(aes(x = datetime, y = prediction, color = ensemble)) +
#   geom_line() +
#   geom_vline(aes(xintercept = reference_datetime)) +
#   facet_wrap(~variable, scale = "free", ncol = 2)

## -----------------------------------------------------------------------------
df <- get_ensemble_forecast(
  latitude = 37.30,
  longitude = -79.83,
  forecast_days = 7,
  past_days = 2,
  model = "gfs_seamless",
  variables = glm_variables(product = "ensemble_forecast", 
                                        time_step = "hourly"))
head(df)

## ----emsemble-plot------------------------------------------------------------
df |> 
  mutate(variable = paste(variable, unit)) |> 
  ggplot(aes(x = datetime, y = prediction, color = ensemble)) + 
  geom_line() + 
  geom_vline(aes(xintercept = reference_datetime)) + 
  facet_wrap(~variable, scale = "free", ncol = 2)

## -----------------------------------------------------------------------------
path <- tempdir()
df |> 
  add_longwave() |>
  write_glm_format(path = path)
head(read_csv(list.files(path = path, full.names = TRUE, pattern = ".csv")[1]))

## -----------------------------------------------------------------------------
df |>
  add_longwave() |>
  convert_to_efi_standard()

## -----------------------------------------------------------------------------
df |>
  add_longwave() |>
  convert_to_efi_standard() |> 
  filter(datetime < reference_datetime)

## -----------------------------------------------------------------------------
df <- get_historical_weather(
  latitude = 37.30,
  longitude = -79.83,
  start_date = "2023-01-01",
  end_date = Sys.Date() - lubridate::days(1),
  variables = c("temperature_2m")) 
tail(df |> na.omit())

## ----hist-plot----------------------------------------------------------------
df |> 
  mutate(variable = paste(variable, unit)) |> 
  ggplot(aes(x = datetime, y = prediction)) + 
  geom_line(color = "#F8766D") + 
  geom_vline(aes(xintercept = lubridate::with_tz(Sys.time(), tzone = "UTC"))) + 
  facet_wrap(~variable, scale = "free")

## -----------------------------------------------------------------------------
df <- get_seasonal_forecast(
  latitude = 37.30,
  longitude = -79.83,
  forecast_days = 274,
  past_days = 5,
  variables = c("temperature_2m"))
head(df)

## ----seasonal-plot------------------------------------------------------------
df |> 
  mutate(variable = paste(variable, unit)) |> 
  ggplot(aes(x = datetime, y = prediction, color = ensemble)) + 
  geom_line() + 
  geom_vline(aes(xintercept = reference_datetime)) +
  facet_wrap(~variable, scale = "free")

## -----------------------------------------------------------------------------
df <- get_seasonal_forecast(
  latitude = 37.30,
  longitude = -79.83,
  forecast_days = 30,
  past_days = 5,
  variables = glm_variables(product = "seasonal_forecast", 
                            time_step = "6hourly"))

## ----downscale-plot-----------------------------------------------------------
df |> 
  six_hourly_to_hourly(latitude = 37.30, longitude = -79.83, use_solar_geom = TRUE) |> 
  mutate(variable = paste(variable, unit)) |> 
  ggplot(aes(x = datetime, y = prediction, color = ensemble)) + 
  geom_line() + 
  geom_vline(aes(xintercept = reference_datetime)) + 
  facet_wrap(~variable, scale = "free", ncol = 2)

## -----------------------------------------------------------------------------
df <- get_climate_projections(
  latitude = 37.30,
  longitude = -79.83,
  start_date = Sys.Date(),
  end_date = Sys.Date() + lubridate::years(1),
  model = "EC_Earth3P_HR",
  variables = c("temperature_2m_mean"))
head(df)

## ----climate-plot-------------------------------------------------------------
df |> 
  mutate(variable = paste(variable, unit)) |> 
  ggplot(aes(x = datetime, y = prediction)) + 
  geom_line(color = "#F8766D") + 
  facet_wrap(~variable, scale = "free")

## -----------------------------------------------------------------------------
models <- c("CMCC_CM2_VHR4","FGOALS_f3_H","HiRAM_SIT_HR","MRI_AGCM3_2_S","EC_Earth3P_HR","MPI_ESM1_2_XR","NICAM16_8S")

df <- map_df(models, function(model){
  get_climate_projections(
    latitude = 37.30,
    longitude = -79.83,
    start_date = Sys.Date(),
    end_date = Sys.Date() + lubridate::years(1),
    model = model,
    variables = c("temperature_2m_mean"))
})


## ----multi-climate-plot-------------------------------------------------------
df |> 
  mutate(variable = paste(variable, unit)) |> 
  ggplot(aes(x = datetime, y = prediction, color = model_id)) + 
  geom_line() +
  facet_wrap(~variable, scale = "free")

## -----------------------------------------------------------------------------
sites <- tibble(site_id = c("fcre", "sunp"),
                latitude = c(37.30, 43.39),
                longitude = c(-79.83, -72.05))

df <- map_df(1:nrow(sites), function(i, sites){
  get_climate_projections(
    latitude = sites$latitude[i],
    longitude = sites$longitude[i],
    site_id = sites$site_id[i],
    start_date = Sys.Date(),
    end_date = Sys.Date() + lubridate::years(1),
    model = "MPI_ESM1_2_XR",
    variables = c("temperature_2m_mean"))
},
sites)
head(df)

## ----multi-site-plot----------------------------------------------------------
df |> 
  mutate(variable = paste(variable, unit)) |> 
  ggplot(aes(x = datetime, y = prediction, color = site_id)) + 
  geom_line() +
  facet_wrap(~variable, scale = "free")

## -----------------------------------------------------------------------------
df <- get_climate_projections(
  latitude = 37.30,
  longitude = -79.83,
  start_date = Sys.Date(),
  end_date = Sys.Date() + lubridate::years(1),
  model = "EC_Earth3P_HR",
  variables = glm_variables(product = "climate_projection", time_step = "daily"))

## ----daily-to-hourly-plot-----------------------------------------------------
df |> 
  daily_to_hourly(latitude = 37.30, longitude = -79.83) |> 
  mutate(variable = paste(variable, unit)) |> 
  ggplot(aes(x = datetime, y = prediction)) + 
  geom_line(color = "#F8766D") + 
  facet_wrap(~variable, scale = "free", ncol = 2)

