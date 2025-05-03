## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width = 7, fig.height = 5)

## ----library, echo = FALSE----------------------------------------------------
suppressPackageStartupMessages({
  library(AirMonitor)
  
  Camp_Fire <- Camp_Fire
})

## ----Sacramento_2-------------------------------------------------------------
monitor_leaflet(Camp_Fire)

## ----Sacramento_3-------------------------------------------------------------
# create single-monitor Sacramento 
Sacramento <-
  
  # 1) start with Camp_Fire
  Camp_Fire %>%
  
  # 2) select a specific device-deployment
  monitor_select("127e996697f9731c_840060670010")

# review timeseries plot
Sacramento %>%
  monitor_timeseriesPlot(
    shadedNight = TRUE,
    addAQI = TRUE,
    main = "Hourly PM2.5 Concentration in Sacramento"
  )

# add the AQI legend
addAQILegend(cex = 0.8)

## ----Sacramento_4-------------------------------------------------------------
Sacramento_area <-
  
  # 1) start with Camp_Fire
  Camp_Fire %>%
  
  # 2) find all monitors within 50km of Sacramento
  monitor_filterByDistance(
    longitude = Sacramento$meta$longitude,
    latitude = Sacramento$meta$latitude,
    radius = 50000
  )

monitor_leaflet(Sacramento_area)

## ----Sacramento_5-------------------------------------------------------------
Sacramento_area %>%
  monitor_timeseriesPlot(
    shadedNight = TRUE,
    addAQI = TRUE,
    main = "Wildfire Smoke within 30 miles of Sacramento"
  )

addAQILegend(lwd = 1, pch = NA, bg = "white", cex = 0.8)

## ----Sacramento_6-------------------------------------------------------------
# 1) start with Sacramento_area
Sacramento_area %>%
  
  # 2) average together all timeseries hour-by-hour
  monitor_collapse(
    deviceID = "Sacramento_area"
  ) %>%
  
  # 3) calculate the local-time daily average (default)
  monitor_dailyStatistic() %>%
  
  # 4) pull out the $data dataframe
  monitor_getData()

## ----Sacramento_7-------------------------------------------------------------
# 1) start with Sacramento_area
Sacramento_area %>%
  
  # 2) average together all timeseries hour-by-hour
  monitor_collapse() %>%
  
  # 3) create daily barplot
  monitor_dailyBarplot(
    main = "Daily Average PM2.5 in the Sacramento Area"
  )

# add the AQI legend
addAQILegend(pch = 15, bg = "white", cex = 0.8)

