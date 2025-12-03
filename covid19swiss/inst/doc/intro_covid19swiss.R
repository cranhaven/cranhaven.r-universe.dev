## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  message=FALSE, 
  warning=FALSE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(covid19swiss)

head(covid19swiss)

## -----------------------------------------------------------------------------
library(tidyr)

covid19swiss_wide <- covid19swiss %>% 
  pivot_wider(names_from = data_type, values_from = value)

head(covid19swiss_wide)

## -----------------------------------------------------------------------------
library(dplyr)

covid19swiss %>%
  filter(date == as.Date("2020-09-08"),
         data_type %in% c("cases_total", "recovered_total", "death_total")) %>%
  select(location, value, data_type) %>%
  pivot_wider(names_from = data_type, values_from = value) %>%
  arrange(-cases_total)

## -----------------------------------------------------------------------------
covid19swiss %>% dplyr::filter(location == "GE",
                               date == as.Date("2020-04-10")) %>%
  dplyr::select(data_type, value) %>%
  tidyr::pivot_wider(names_from = data_type, values_from = value) %>%
  dplyr::mutate(positive_tested = round(100 * cases_total / tested_total, 2),
                death_rate = round(100 * deaths_total / cases_total, 2),
                recovery_rate = round(100 * recovered_total / cases_total, 2)) %>%
  dplyr::select(positive_tested, recovery_rate, death_rate) 

## -----------------------------------------------------------------------------
switzerland <- covid19swiss %>% filter(location != "FL")

head(switzerland)

liechtenstein <- covid19swiss %>% filter(location == "FL")

head(liechtenstein)

