## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(httr2)

## ----install , eval = FALSE---------------------------------------------------
#  install.packages("dtmapi")

## ----setup--------------------------------------------------------------------
library(dtmapi)

## ----get_country--------------------------------------------------------------
# Fetch all countries
countries_df <- get_all_countries()

# Display the first few rows of the data frame
head(countries_df)

## ----get_operations-----------------------------------------------------------
# Fetch all operations
operations_df <- get_all_operations()

# Display the first few rows of the data frame
head(operations_df)

## ----get_idp_admin0-----------------------------------------------------------
# Fetch IDP data at Admin Level 0
idp_admin0_df <- get_idp_admin0_data(CountryName='Ethiopia', FromRoundNumber=1, ToRoundNumber=10)

# Display the first few rows of the data frame
head(idp_admin0_df)

## ----get_idp_admin1-----------------------------------------------------------
# Fetch IDP data at Admin Level 1
idp_admin1_df <- get_idp_admin1_data(CountryName='Sudan', Admin1Name="Blue Nile", FromReportingDate='2020-01-01', ToReportingDate='2024-08-15')

# Display the first few rows of the data frame
head(idp_admin1_df)

## ----get_idp_admin2-----------------------------------------------------------
# Fetch IDP data at Admin Level 2
idp_admin2_df <- get_idp_admin2_data(Operation="Displacement due to conflict", CountryName='Lebanon')

# Display the first few rows of the data frame
head(idp_admin2_df)

