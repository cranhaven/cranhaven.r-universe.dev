## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(wdi2)

## -----------------------------------------------------------------------------
#  install.packages("wdi2")

## -----------------------------------------------------------------------------
#  pak::pak("tidy-intelligence/r-wdi2")

## -----------------------------------------------------------------------------
# Retrieve the list of supported indicators
list_supported_indicators()

## -----------------------------------------------------------------------------
# Retrieve the list of supported indicators with details
list_supported_indicators(include_details = TRUE)

## -----------------------------------------------------------------------------
# Retrieve indicators in Spanish
list_supported_indicators(language = "es", include_details = TRUE, progress = FALSE)

## -----------------------------------------------------------------------------
# List supported languages
list_supported_languages()

## -----------------------------------------------------------------------------
# Retrieve the list of supported countries
list_supported_countries()

## -----------------------------------------------------------------------------
# Download specific indicators for selected countries
download_indicators(countries = c("MX", "CA", "US"), indicators = c("NY.GDP.PCAP.KD", "SP.POP.TOTL"))

