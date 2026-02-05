## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  # eval = FALSE,
  fig.width = 8,
  fig.height = 6,
  out.width = "70%"
)

## ----setup--------------------------------------------------------------------
library(EcoCleanR)
library(rgbif)
library(robis)
library(ridigbio)
library(dplyr)

## -----------------------------------------------------------------------------
species_name <- "Mexacanthina lugubris"
taxonkey <- name_backbone(species_name)$usageKey

## -----------------------------------------------------------------------------
attribute_list <- c("source", "catalogNumber", "basisOfRecord", "occurrenceStatus", "institutionCode", "verbatimEventDate", "scientificName", "individualCount", "organismQuantity", "abundance", "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters", "locality", "verbatimLocality", "municipality", "county", "stateProvince", "country", "countryCode")

## -----------------------------------------------------------------------------
gbif.occ <- occ_data(taxonKey = taxonkey, occurrenceStatus = NULL, limit = 10000L)$data

# refer article/cite_data.Rmd for instructions on how to cite the data from gbif- data providers

## additional field added to know the source
gbif.occ$source <- "gbif"
for (field in attribute_list) {
  if (!field %in% names(gbif.occ)) {
    gbif.occ[[field]] <- NA # Add the missing field as NA
  }
}

## we are making one column called abundance which should have values from individual count and organism Quantity
gbif.occ$abundance <- ifelse(is.na(as.numeric(gbif.occ$individualCount)), as.numeric(gbif.occ$organismQuantity), as.numeric(gbif.occ$individualCount))
## additional field added to know the source
gbif.occ$source <- "gbif"
gbif.occ_temp <- gbif.occ[, attribute_list]
str(gbif.occ_temp[, 1:3])

## -----------------------------------------------------------------------------
obis.occ <- occurrence(species_name)
for (field in attribute_list) {
  if (!field %in% names(obis.occ)) {
    obis.occ[[field]] <- NA # Add the missing field as NA
  }
}
obis.occ$abundance <- ifelse(is.na(as.numeric(obis.occ$individualCount)), as.numeric(obis.occ$organismQuantity), as.numeric(obis.occ$individualCount))
obis.occ$source <- "obis"
obis.occ$municipality <- ""
obis.occ_temp <- obis.occ[, attribute_list]
str(obis.occ_temp[, 1:3])

## -----------------------------------------------------------------------------
idig.occ <- idig_search_records(
  type = "records",
  rq = list("scientificname" = species_name),
  field = "all",
  max_items = 10000L,
  limit = 10000L,
  offset = 0
)

idig.occ <- idig.occ %>%
  mutate(
    abundance = as.numeric(individualcount),
    source = "idigbio",
    occurrenceStatus = "",
    organismQuantity = ""
  ) %>%
  rename(
    decimalLatitude = geopoint.lat,
    decimalLongitude = geopoint.lon,
    basisOfRecord = basisofrecord,
    catalogNumber = catalognumber,
    scientificName = scientificname,
    stateProvince = stateprovince,
    coordinateUncertaintyInMeters = coordinateuncertainty,
    individualCount = individualcount,
    institutionCode = institutioncode,
    verbatimLocality = verbatimlocality,
    verbatimEventDate = verbatimeventdate,
    countryCode = countrycode
  )

idig.occ_temp <- idig.occ[, attribute_list]
str(idig.occ_temp[, 1:3])

## -----------------------------------------------------------------------------
sym.occ <- example_sp_invertebase
sym.occ$abundance <- as.numeric(sym.occ$individualCount)

for (field in attribute_list) {
  if (!field %in% names(sym.occ)) {
    sym.occ[[field]] <- NA # Add the missing field as NA
  }
}

str(sym.occ[, 1:3])

## -----------------------------------------------------------------------------
db_list <- list(gbif.occ_temp, obis.occ_temp, idig.occ_temp, sym.occ)
Mixdb.occ <- ec_db_merge(db_list = db_list, datatype = "modern")

str(Mixdb.occ[, 1:3])
ec_geographic_map(Mixdb.occ, "decimalLatitude", longitude = "decimalLongitude") # display records those has coordinate values

