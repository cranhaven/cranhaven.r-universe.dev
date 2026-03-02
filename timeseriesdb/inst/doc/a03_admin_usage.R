## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(timeseriesdb)

## ---- eval=FALSE--------------------------------------------------------------
#  library(kofdata)
#  arr <- get_time_series("ch.zrh_airport.arrival.total")
#  dep <- get_time_series("ch.zrh_airport.departure.total")
#  
#  db_ts_store(con, c(arr,dep), schema = "tsdb_test")
#  
#  db_dataset_create(con, "ch.zrh_airport",
#                    set_description = "Arrivals and Departures ZH Airport",
#                    schema = "tsdb_test")
#  
#  # see, the new dataset is there...
#  db_dataset_list(con, schema = "tsdb_test")
#  

## ---- eval=FALSE--------------------------------------------------------------
#  db_dataset_delete(con, "dataset_name", schema = "tsdb_test")

## ---- eval=FALSE--------------------------------------------------------------
#  db_release_create(con,
#                    id = "jul_2020_air",
#                    datasets = "ch.zrh_airport",
#                    title = "Airport Data from Zurich Airport",
#                    release_date = "2020-07-08",
#                    target_year = 2020,
#                    target_period = 7,
#                    target_frequency = 12,
#                    note = "Just a test release",
#                    schema = "tsdb_test"
#                    )

## ----eval=FALSE---------------------------------------------------------------
#  db_access_level_create(con,
#                         access_level_name =  "management",
#                         access_level_description = "series only available",
#                         access_level_default = TRUE,
#                         schema = "tsdb_test"
#                         )

