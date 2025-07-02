## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = FALSE-------------------------------------------------------------
# install.packages("flightsbr")

## ----eval=FALSE---------------------------------------------------------------
# library(flightsbr)

## ----eval=FALSE---------------------------------------------------------------
# # in a given **month* of a given **year** (yyyymm)
# df_201506 <- read_flights(date = 201506, showProgress = FALSE)
# 
# # in a given year (yyyy)
# df_2015 <- read_flights(date = 2015, showProgress = FALSE)
# 

## ----eval=FALSE---------------------------------------------------------------
# airports_all <- flightsbr::read_airports(type = 'all', showProgress = FALSE)
# 
# airports_prv <- flightsbr::read_airports(type = 'private', showProgress = FALSE)
# 
# airports_pbl <- flightsbr::read_airports(type = 'public', showProgress = FALSE)
# 
# 

## ----eval=FALSE---------------------------------------------------------------
# aircraft <- flightsbr::read_aircraft(date = 2024, showProgress = FALSE)
# 
# head(aircraft)

## ----eval=FALSE---------------------------------------------------------------
# airport_mov <- flightsbr::read_airport_movements(date = 201901)
# 
# head(airport_mov)

## ----eval=FALSE---------------------------------------------------------------
# airfares <- flightsbr::read_airfares(date = 202003, domestic = TRUE)
# 
# head(airfares)

