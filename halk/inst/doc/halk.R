## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = FALSE------------------------------------------------------
library(halk)

## ----wb_spp_data_example, echo = FALSE----------------------------------------
head(wb_spp_laa_data)

## ----spp_county_wb_alk--------------------------------------------------------
spp_county_wb_alk <- make_halk(
  wb_spp_laa_data, 
  levels = c("spp", "county", "waterbody")
)
head(spp_county_wb_alk)

## ----specific_alk_example-----------------------------------------------------
# Bluegill ALK for lake_a in county_A, from row #1 above
head(spp_county_wb_alk$alk[[1]])

## ----assign_ages_example------------------------------------------------------
est_ages <- assign_ages(wb_spp_length_data, spp_county_wb_alk)
head(est_ages)

## ----missing_lake_example-----------------------------------------------------
head(est_ages[est_ages$waterbody == "lake_x", ])

