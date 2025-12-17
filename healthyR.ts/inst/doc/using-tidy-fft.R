## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 5
)

## ----setup--------------------------------------------------------------------
library(healthyR.ts)

## ----example_data-------------------------------------------------------------
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(timetk))

data_tbl <- AirPassengers %>%
    ts_to_tbl() %>%
    select(-index)

## ----data_glimpse-------------------------------------------------------------
glimpse(data_tbl)

## ----ts_plt-------------------------------------------------------------------
suppressPackageStartupMessages(library(timetk))

data_tbl %>%
  plot_time_series(
    .date_var = date_col,
    .value    = value
  )

## ----run_func, message=FALSE, warning=FALSE-----------------------------------
output <- tidy_fft(
  .data = data_tbl,
  .date_col = date_col,
  .value_col = value,
  .harmonics = 8,
  .frequency = 12,
  .upsampling = 5
)

## ----data_data----------------------------------------------------------------
output$data$data %>%
  glimpse()

## ----error_data---------------------------------------------------------------
output$data$error_data %>%
  glimpse()

## ----input_vector-------------------------------------------------------------
output$data$input_vector

## ----max_har_tbl--------------------------------------------------------------
output$data$maximum_harmonic_tbl %>%
  glimpse()

## ----diff_val_tbl-------------------------------------------------------------
output$data$differenced_value_tbl %>%
  glimpse()

## ----dff_tbl------------------------------------------------------------------
output$data$dff_tbl %>%
  glimpse()

## ----ts_obj-------------------------------------------------------------------
output$data$ts_obj

## ----har_plt, message=FALSE, warning=FALSE------------------------------------
output$plots$harmonic_plot

## ----diff_val_plt, message=FALSE, warning=FALSE-------------------------------
output$plots$diff_plot

## ----max_har_plt, message=FALSE, warning=FALSE--------------------------------
output$plots$max_har_plot

## ----har_pltly, message=FALSE, warning=FALSE----------------------------------
output$plots$harmonic_plotly

## ----max_har_pltly, message=FALSE, warning=FALSE------------------------------
output$plots$max_har_plotly

## ----parameters---------------------------------------------------------------
output$parameters

## ----m------------------------------------------------------------------------
output$model$m

## ----harmonic_obj-------------------------------------------------------------
output$model$harmonic_obj %>% head()

## ----har_model----------------------------------------------------------------
output$model$harmonic_model

## ----har_summary--------------------------------------------------------------
output$model$model_summary

