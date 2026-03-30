## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(dialvalidator)

## -----------------------------------------------------------------------------
# International format -- region inferred from country code
phone_valid("+64211234567")

# National format -- supply the region
phone_valid("021 123 4567", default_region = "NZ")

# Invalid: too short
phone_valid("+6421")

# Vectorised
phone_valid(c("+64211234567", "+12125551234", "not a number", "+61412345678"))

## -----------------------------------------------------------------------------
# E.164: compact, no spaces, machine-readable
phone_format("+64211234567", "E164")

# National: how you'd dial it locally
phone_format("+64211234567", "NATIONAL")

# International: with country code and spacing
phone_format("+64211234567", "INTERNATIONAL")

## -----------------------------------------------------------------------------
phone_format("+12125551234", "NATIONAL")
phone_format("+442071234567", "NATIONAL")
phone_format("+61412345678", "NATIONAL")

## -----------------------------------------------------------------------------
# NZ mobile
phone_type("+64211234567")

# US toll-free
phone_type("+18005551234")

## -----------------------------------------------------------------------------
phone_country(c("+64211234567", "+12125551234", "+14165551234", "+442071234567"))

## -----------------------------------------------------------------------------
phone_info(c("+64211234567", "+12125551234", "+61412345678", "+81312345678"))

## -----------------------------------------------------------------------------
nz_numbers <- c("021 123 4567", "09 300 1234", "0800 123 456")
phone_info(nz_numbers, default_region = "NZ")

## -----------------------------------------------------------------------------
nz <- dv_territory("NZ")
nz$country_code
nz$national_prefix
nz$mobile$example

## ----eval = FALSE-------------------------------------------------------------
# dv_update_metadata()

