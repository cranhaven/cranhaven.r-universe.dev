## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = FALSE-------------------------------------------------------------
library(knitr)
options(knitr.kable.NA = "-")

## ----results = "hide", message = FALSE----------------------------------------
library(tidystats)
library(dplyr)

# Read the .json file containing the statistics and immediately convert it to
# a data frame
statistics <- read_stats("statistics.json") |>
  tidy_stats_to_data_frame()

# Extract all the p-values
p_values <- filter(statistics, statistic_name == "p")

p_values

## ----echo = FALSE-------------------------------------------------------------
kable(p_values, format = "markdown")

## -----------------------------------------------------------------------------
sig_p_values <- filter(statistics, statistic_name == "p" & value < .05)

## ----example2_eval, echo = FALSE----------------------------------------------
kable(sig_p_values, format = "markdown")

