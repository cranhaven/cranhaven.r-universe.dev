## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(cycleTrendR)
library(ggplot2)

## ----example_1----------------------------------------------------------------
set.seed(1)

dates <- as.Date("2020-01-01") + 1:200
signal <- sin(2*pi*(1:200)/30) + rnorm(200, 0, 0.2)

res_date <- adaptive_cycle_trend_analysis(
  signal = signal,
  dates = dates,
  dates_type = "date",
  trendmethod = "loess",
  usefourier = TRUE
)

res_date$Plot$Trend
res_date$Plot$Spectrum

## ----example_2----------------------------------------------------------------
# Example: synthetic POSIXct HR-like signal
t <- as.POSIXct("2020-01-01 00:00:00") + seq(0, by = 60, length.out = 1000)
hr <- 70 + 5*sin(2*pi*(1:1000)/200) + rnorm(1000, 0, 1)

res_posix <- adaptive_cycle_trend_analysis(
  signal = hr,
  dates = t,
  dates_type = "posix",
  trendmethod = "gam",
  usefourier = TRUE
)

res_posix$Plot$Trend
res_posix$Plot$Spectrum

## ----example_3----------------------------------------------------------------
time <- seq(0, 10, length.out = 2000)
spike_rate <- sin(2*pi*time*5) + rnorm(2000, 0, 0.2)

res_num <- adaptive_cycle_trend_analysis(
  signal = spike_rate,
  dates = time,
  dates_type = "numeric",
  trendmethod = "loess",
  usefourier = TRUE
)

res_num$Plot$Trend
res_num$Plot$Spectrum

