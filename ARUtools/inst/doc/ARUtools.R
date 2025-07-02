## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tibble.print_min = 4L, tibble.print_max = 4L)

## -----------------------------------------------------------------------------
library(ARUtools)

## -----------------------------------------------------------------------------
head(example_files)

## -----------------------------------------------------------------------------
m <- clean_metadata(project_files = example_files)

## -----------------------------------------------------------------------------
m

## ----eval=FALSE---------------------------------------------------------------
# base_directory <- "/path/to/project/files/"
# m <- clean_metadata(project_dir = base_directory)

## -----------------------------------------------------------------------------
example_sites

## ----error = TRUE-------------------------------------------------------------
try({
sites <- clean_site_index(example_sites)
})

## -----------------------------------------------------------------------------
sites <- clean_site_index(example_sites,
  name_aru_id = "ARU",
  name_site_id = "Sites",
  name_date_time = c("Date_set_out", "Date_removed"),
  name_coords = c("lon", "lat")
)

## -----------------------------------------------------------------------------
sites

## -----------------------------------------------------------------------------
sites <- clean_site_index(example_sites,
  name_aru_id = "ARU",
  name_site_id = "Sites",
  name_date_time = c("Date_set_out", "Date_removed"),
  name_coords = c("lon", "lat"),
  name_extra = c("Plots", "Subplot")
)
sites

## -----------------------------------------------------------------------------
sites <- clean_site_index(example_sites,
  name_aru_id = "ARU",
  name_site_id = "Sites",
  name_date_time = c("Date_set_out", "Date_removed"),
  name_coords = c("lon", "lat"),
  name_extra = c("plot" = "Plots", "subplot" = "Subplot")
)
sites

## -----------------------------------------------------------------------------
m <- add_sites(m, sites)
m

## -----------------------------------------------------------------------------
m <- calc_sun(m)
dplyr::glimpse(m)

