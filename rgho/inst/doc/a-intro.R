## ----echo=FALSE, include=FALSE------------------------------------------------
library(rgho)
library(dplyr)

## -----------------------------------------------------------------------------
get_gho_dimensions()

## -----------------------------------------------------------------------------
get_gho_values(dimension = "COUNTRY")
get_gho_values(dimension = "GHO")

## -----------------------------------------------------------------------------
search_dimensions("region")
search_values("neonatal", dimension = "GHO")

## -----------------------------------------------------------------------------
result <- get_gho_values(dimension = "REGION")
search_gho(result, "asia")

## -----------------------------------------------------------------------------
result <- get_gho_data(
  code = "MDG_0000000001"
)

print(result)

## -----------------------------------------------------------------------------
result <- get_gho_data(
  code = "MDG_0000000001",
  filter = list(
    REGION = "EUR",
    YEAR = 2015
  )
)

print(result)

