## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(cycleTrendR)
set.seed(1)

## -----------------------------------------------------------------------------
dates <- as.Date("2020-01-01") + cumsum(sample(1:3, 300, replace = TRUE))
signal <- sin(2*pi*as.numeric(dates)/20) + rnorm(300, 0, 0.3)

## -----------------------------------------------------------------------------
res_loess <- adaptive_cycle_trend_analysis(
  signal = signal,
  dates = dates,
  trendmethod = "loess",
  usefourier = TRUE,
  auto_fourier_select = TRUE,
  nboot = 50
)

res_loess$Plot$Trend

## -----------------------------------------------------------------------------
res_gam <- adaptive_cycle_trend_analysis(
  signal = signal,
  dates = dates,
  trendmethod = "gam",
  usefourier = TRUE,
  nboot = 50
)

res_gam$Plot$Trend

## -----------------------------------------------------------------------------
group <- rep(letters[1:4], length.out = length(signal))

res_gamm <- adaptive_cycle_trend_analysis(
  signal = signal,
  dates = dates,
  trendmethod = "gam",
  use_gamm = TRUE,
  group_var = "subject",
  group_values = group,
  usefourier = FALSE,
  nboot = 20
)

res_gamm$Plot$Trend

## -----------------------------------------------------------------------------
  res_irreg <- adaptive_cycle_trend_analysis(
  signal = signal,
  dates = dates,
  trendmethod = "loess",
  usefourier = TRUE,
  auto_fourier_select = TRUE,
  nboot = 50
)

res_irreg$Plot$Spectrum

## -----------------------------------------------------------------------------
res_loess$ChangePoints

## -----------------------------------------------------------------------------
head(res_loess$CI$lower)
head(res_loess$CI$upper)

