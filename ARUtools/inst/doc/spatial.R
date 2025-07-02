## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ARUtools)
library(sf)
library(dplyr)

## -----------------------------------------------------------------------------
m <- clean_metadata(project_files = example_files)
m

## -----------------------------------------------------------------------------
s <- st_as_sf(example_sites, coords = c("lon", "lat"), crs = 4326)
s

## -----------------------------------------------------------------------------
sites <- clean_site_index(s,
  name_aru_id = "ARU",
  name_site_id = "Sites",
  name_date_time = c("Date_set_out", "Date_removed")
)
sites

## -----------------------------------------------------------------------------
m <- add_sites(m, sites)
m

## -----------------------------------------------------------------------------
m <- calc_sun(m)
m

## -----------------------------------------------------------------------------
m <- clean_metadata(project_files = example_files)

sites <- st_as_sf(example_sites, coords = c("lon", "lat"), crs = 4326) |>
  clean_site_index(
    name_aru_id = "ARU",
    name_site_id = "Sites",
    name_date_time = c("Date_set_out", "Date_removed")
  )

sites <- sites[-1, ] # Omit that first site

m <- add_sites(m, sites)
m

## -----------------------------------------------------------------------------
m <- clean_metadata(project_files = example_files) |>
  filter(date > "2020-05-03") # Filter out recordings that don't match a site

m <- add_sites(m, sites)
m

