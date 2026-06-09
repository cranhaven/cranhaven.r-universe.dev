## ----message=FALSE, warning=FALSE---------------------------------------------
knitr::opts_chunk$set(
  comment = "#>"
)
library(terra) # to read the netCDF files
library(lubridate) # to deal with dates and times
library(dplyr) # to wrangle and tidy the data
library(tidyr) # to wrangle and tidy the data
library(ggplot2) # to make statis maps
library(gganimate) # to make a temporal gif of climate variation
library(rcontroll) # to generate TROLL climate inputs

## ----message=TRUE, warning=TRUE, include=FALSE--------------------------------
if (Sys.info()[["sysname"]] == "Darwin") {
  knitr::opts_chunk$set(
    eval = FALSE
  )
}

## ----eval=FALSE---------------------------------------------------------------
#  library(ecmwfr) # to request data from Copernicus
#  library(osmdata) # to get bounding box from the study area
#  library(lutz) # to get time zone
#  library(nominatimlite) # to get coordinates from the study area
#  library(leaflet) # to make interactive maps
#  library(sf) # to extract coordinates from spatial objects

## ----eval=FALSE---------------------------------------------------------------
#  wf_set_key(
#    user = "******",
#    key = "********-****-****-****-************",
#    service = "cds"
#  )

## ----eval=FALSE---------------------------------------------------------------
#  getbb("French Guiana", format_out = "sf_polygon", limit = 1)$multipolygon %>%
#    leaflet() %>%
#    addTiles() %>%
#    addPolygons()

## ----eval=FALSE---------------------------------------------------------------
#  (coords <- gsub(",", "/", getbb("French Guiana",
#                                  format_out = "string", limit = 1)))

## ----eval=FALSE---------------------------------------------------------------
#  request <- list(
#    "dataset_short_name" = "reanalysis-era5-land-monthly-means",
#    "format" = "netcdf",
#    "product_type" = "monthly_averaged_reanalysis_by_hour_of_day",
#    "variable" = c(
#      "10m_u_component_of_wind",
#      "10m_v_component_of_wind",
#      "2m_dewpoint_temperature",
#      "2m_temperature",
#      "surface_pressure",
#      "total_precipitation",
#      "surface_solar_radiation_downwards"
#    ),
#    "month" = sprintf("%02d", 1:12),
#    "time" = sprintf("%02d:00", 0:23),
#    "year" = as.character(2022),
#    "target" = "ERA5land_hr_Nouragues_2022.nc",
#    "area" = "3.960414/-52.85468/4.160414/-52.65468"
#  )

## ----eval=FALSE---------------------------------------------------------------
#  ncfile <- wf_request(
#    user = "152268",
#    request = request,
#    transfer = TRUE,
#    path = ".",
#    verbose = FALSE
#  )

## ----eval=FALSE---------------------------------------------------------------
#  request <- list(
#    "dataset_short_name" = "reanalysis-era5-land-monthly-means",
#    "format" = "netcdf",
#    "product_type" = "monthly_averaged_reanalysis", "time" = "00:00",
#    "variable" = c(
#      "10m_u_component_of_wind",
#      "10m_v_component_of_wind",
#      "2m_dewpoint_temperature",
#      "2m_temperature",
#      "surface_pressure",
#      "total_precipitation",
#      "surface_solar_radiation_downwards"
#    ),
#    "month" = sprintf("%02d", 1:12),
#    "time" = sprintf("%02d:00", 0:23),
#    "year" = as.character(2021:2022),
#    "target" = "ERA5land_mth_Nouragues_2021_2022.nc",
#    "area" = "3.960414/-52.85468/4.160414/-52.65468"
#  )

## ----eval=FALSE---------------------------------------------------------------
#  ncfile <- wf_request(
#    user = "152268",
#    request = request,
#    transfer = TRUE,
#    path = ".",
#    verbose = FALSE
#  )

## -----------------------------------------------------------------------------
test_r <- suppressWarnings(rast(
  system.file("extdata",
    "ERA5land_mth_Nouragues_2021_2022.nc",
    package = "rcontroll"
  )
))
test <- suppressWarnings(as.data.frame(test_r, xy = TRUE)) %>%
  gather("variable", "value", -x, -y) %>%
  group_by(x, y) %>%
  mutate(date = rep(as_date(terra::time(test_r)))) %>%
  separate(variable, c("variable", "t"), sep = "_(?=\\d)") %>%
  select(-t) %>%
  separate(variable, c("variable", "expver"), sep = "_expver=") %>%
  group_by(x, y, date, variable) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  spread(variable, value) %>%
  arrange(date)

## -----------------------------------------------------------------------------
ggplot(test, aes(date, tp)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  xlab("") +
  ylab("Total precipitation")

## ----eval=FALSE---------------------------------------------------------------
#  geo_lite_sf("Réserve naturelle des nouragues, 97301, Régina") %>%
#    leaflet() %>%
#    addTiles() %>%
#    addPolygons(data = getbb("French Guiana",
#                             format_out = "sf_polygon",
#                             limit = 1)$multipolygon) %>%
#    addCircleMarkers(col = "red")

## ----eval=FALSE---------------------------------------------------------------
#  (coords <- geo_lite_sf("Réserve naturelle des nouragues, 97301, Régina") %>%
#     st_coordinates())

## ----eval=FALSE---------------------------------------------------------------
#  (tz <- tz_lookup_coords(
#    lon = coords[1],
#    lat = coords[2], method = "accurate"
#  ))

## -----------------------------------------------------------------------------
climate <- generate_climate(
  x = -52.75468,
  y = 4.060414,
  tz = "America/Cayenne",
  era5land_hour = system.file("extdata", "ERA5land_hr_Nouragues_2022.nc",
    package = "rcontroll"
  ),
  era5land_month = system.file("extdata", "ERA5land_mth_Nouragues_2021_2022.nc",
    package = "rcontroll"
  )
)

## -----------------------------------------------------------------------------
climate$daytimevar %>% head()

## -----------------------------------------------------------------------------
climate$climatedaytime12 %>% head()

## ----fig.width=6, fig.height=3------------------------------------------------
data("TROLLv3_daytimevar")
list(
  Nouraflux = TROLLv3_daytimevar,
  ERA5 = climate$daytimevar
) %>%
  bind_rows(.id = "origin") %>%
  gather(variable, value, -starttime, -endtime, -origin) %>%
  group_by(origin, variable) %>%
  ggplot(aes(x = starttime, y = value, col = origin)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  theme_bw()

## ----fig.width=12, fig.height=6-----------------------------------------------
data("TROLLv3_climatedaytime12")
list(
  Nouraflux = TROLLv3_climatedaytime12,
  ERA5 = climate$climatedaytime12
) %>%
  bind_rows(.id = "origin") %>%
  group_by(origin) %>%
  mutate(order = 1:12) %>%
  mutate(month = as.character(lubridate::month(1:12, label = TRUE))) %>%
  gather(variable, value, -origin, -month, -order) %>%
  mutate(variable = recode(variable,
    "Temperature" = "Temperature~(degree~C)",
    "DaytimeMeanTemperature" = "DaytimeMeanTemperature~(degree~C)",
    "NightTemperature" = "NightTemperature~(degree~C)",
    "Rainfall" = "Rainfall (cm)",
    "WindSpeed" = "WindSpeed (m~s^{-1})",
    "DaytimeMeanIrradiance" = "DaytimeMeanIrradiance~(W~m^{-2})",
    "MeanIrradiance" = "MeanIrradiance~(W~m^{-2})",
    "SaturatedVapourPressure" = "SaturatedVapourPressure (hPa)",
    "VapourPressure" = "VapourPressure (hPa)",
    "VaporPressureDeficit" = "VaporPressureDeficit (hPa)",
    "DayTimeVapourPressureDeficitVPDbasic" =
      "DayTimeVapourPressureDeficitVPDbasic (hPa)",
    "DaytimeMeanVapourPressureDeficit" =
      "DaytimeMeanVapourPressureDeficit (hPa)"
  )) %>%
  ggplot(aes(
    x = reorder(month, order), y = value,
    col = origin, group = origin
  )) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", labeller = label_parsed) +
  theme_bw() +
  theme(
    axis.title = element_blank(), axis.text.x = element_text(angle = 90),
    legend.position = "bottom"
  )

## ----eval=FALSE---------------------------------------------------------------
#  write_tsv(climate$daytimevar, "ERA5land_daytimevar.txt")
#  write_tsv(climate$climatedaytime12, "ERA5land_climatedaytime12.txt")

