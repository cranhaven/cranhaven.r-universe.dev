## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ARUtools)
library(lubridate) # For working with date/times
library(dplyr) # For manipulating data

## -----------------------------------------------------------------------------
s <- example_sites_clean

# Force to a non-UTC timezones
s$date_time_start <- force_tz(s$date_time_start, "America/Toronto")
s$date_time_end <- force_tz(s$date_time_end, "America/Toronto")

## -----------------------------------------------------------------------------
m <- clean_metadata(project_files = example_files)
m <- add_sites(m, s)

## -----------------------------------------------------------------------------
tz(m$date_time)

## -----------------------------------------------------------------------------
m_est <- calc_sun(m, aru_tz = "America/Toronto")

## -----------------------------------------------------------------------------
m_local <- calc_sun(m, aru_tz = "local")

## -----------------------------------------------------------------------------
# Split by timezone
m1 <- filter(m, site_id %in% c("P06_1", "P09_1")) # Get P06_1 and P09_1
m2 <- filter(m, !site_id %in% c("P06_1", "P09_1")) # Get all except the above

# Calculate time to sunrise/sunset individually
m1_cst <- calc_sun(m1, aru_tz = "America/Winnipeg")
m2_est <- calc_sun(m2, aru_tz = "America/Toronto")

# Join them back in
m_joint <- bind_rows(m1_cst, m2_est)

## -----------------------------------------------------------------------------
m_mini <- filter(m, site_id %in% c("P01_1", "P06_1")) |>
  select(aru_id, site_id, longitude, latitude) |>
  distinct() |>
  cross_join(data.frame(date_time = c(
    "2020-05-02 05:00:00",
    "2020-05-02 06:00:00",
    "2020-05-02 07:00:00"
  ))) |>
  mutate(
    date = as_date(date_time),
    path = paste0(aru_id, "_", site_id, "_", hour(date_time), ".csv")
  )

## -----------------------------------------------------------------------------
calc_sun(m_mini, aru_tz = "America/Toronto") |>
  arrange(date_time)

## -----------------------------------------------------------------------------
calc_sun(m_mini, aru_tz = "local") |>
  arrange(date_time)

