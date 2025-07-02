## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ARUtools)
library(dplyr)
library(purrr)
library(tidyr)
library(glue)
library(lubridate)

## -----------------------------------------------------------------------------
s <- clean_site_index(example_sites_clean,
  name_date = c("date_time_start", "date_time_end")
)
m <- clean_metadata(project_files = example_files) |>
  add_sites(s) |>
  calc_sun() |>
  mutate(
    time_period = if_else(hour(date_time) < 6, "early", "late"),
    year = year(date)
  )
m

## -----------------------------------------------------------------------------
p <- list(
  "early" = sim_selection_weights(min_range = c(-70, 240)),
  "late" = sim_selection_weights(min_range = c(100, 300), min_mean = 200)
)
p

## -----------------------------------------------------------------------------
w <- m |>
  nest(data = c(-time_period, -year)) |>
  mutate(
    params = p,
    sel = map2(data, params, calc_selection_weights)
  ) |>
  unnest(sel) |>
  select(-"data", -"params") |>
  mutate(selection_group = glue("{site_id}_{year}_{time_period}"))
w

## -----------------------------------------------------------------------------
n <- w |>
  summarize(n_recordings = n(), .by = c("selection_group", "time_period")) |>
  mutate(
    n = if_else(time_period == "early", 5, 2),
    n_os = if_else(time_period == "early", floor(n * 1 / 3), floor(n * 1 / 4)),
    n_os = pmax(0, pmin(n_recordings - n, round(n / 3))),
    n = pmin(n, n_recordings)
  )
n

## -----------------------------------------------------------------------------
g <- sample_recordings(w, n,
  col_site_id = selection_group,
  col_sel_weights = psel_normalized
)
g

## -----------------------------------------------------------------------------
g$sites_base

