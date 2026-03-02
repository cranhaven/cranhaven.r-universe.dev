## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(timeseriesdb)

## ---- eval=FALSE--------------------------------------------------------------
#  data("kof_ts")
#  db_ts_store(con,
#                    list(ch.kof.barometer = kof_ts$baro_2019m10),
#                    valid_from="2019-10-30",
#                    schema = "tsdb_test")
#  
#  db_ts_store(con,
#                    list(ch.kof.barometer = kof_ts$baro_2019m11),
#                    valid_from="2019-11-30",
#                    schema = "tsdb_test")
#  

## ---- eval=FALSE--------------------------------------------------------------
#  db_ts_read(con, "ch.kof.barometer",
#                   schema = "tsdb_test")
#  
#  

## ----eval=FALSE---------------------------------------------------------------
#  tsl <- db_ts_read_history(con,"ch.kof.barometer",
#                                  schema = "tsdb_test")

## ---- eval=FALSE--------------------------------------------------------------
#  db_ts_assign_dataset(con,
#                    ts_keys = c("ch.zrh_airport.arrival.total",
#                                "ch.zrh_airport.departure.total"),
#                    set_name = "ch.zrh_airport",
#                    schema = "tsdb_test")
#  

## ---- eval=FALSE--------------------------------------------------------------
#  db_get_dataset_keys(con, "ch.zrh_airport", schema = "tsdb_test")

## ---- eval=FALSE--------------------------------------------------------------
#  db_get_dataset_id(con, "ch.zrh_airport.arrival.total",
#                    schema = "tsdb_test")

## ---- eval=FALSE--------------------------------------------------------------
#  db_list_releases(con, include_past = T, schema = "tsdb_test")

## ---- eval=FALSE--------------------------------------------------------------
#  db_get_latest_release_for_set(con, "ch.zrh_airport",schema = "tsdb_test")

## ---- eval = FALSE------------------------------------------------------------
#  db_collection_add(con,
#                    collection_name = "demo_collect",
#                    keys = c("ch.zrh_airport.arrival.total",
#                             "AirPassengers"),
#                    description = "Flying Around Now and Then",
#                    schema = "tsdb_test")

## ---- eval=FALSE--------------------------------------------------------------
#  
#  de <- create_tsmeta(
#    ch.zrh_airport.arrival.total = list(
#      provider = "Flughafen Zürich",
#      description = "Eine deutschsprachige Beschreibung"
#    )
#  )
#  
#  en <- create_tsmeta(
#    ch.zrh_airport.arrival.total = list(
#      provider = "Zurich Airport",
#      description = "An English speaking description"
#    )
#  )
#  
#  
#  
#  db_store_ts_metadata(con,
#                       metadata = de,
#                       valid_from = Sys.Date(),
#                       locale = "de",
#                       schema = "tsdb_test")
#  
#  db_store_ts_metadata(con,
#                       metadata = en,
#                       valid_from = Sys.Date(),
#                       locale = "en",
#                       schema = "tsdb_test")
#  
#  
#  

## ---- eval=FALSE--------------------------------------------------------------
#  
#  db_read_ts_metadata(con, "ch.zrh_airport.arrival.total",
#                      locale = "en",
#                      valid_on = Sys.Date(),
#                      schema = "tsdb_test")
#  
#  

## ---- eval=FALSE--------------------------------------------------------------
#  db_get_metadata_validity(con, "ch.zrh_airport.arrival.total",
#                           locale = "de",
#                           schema = "tsdb_test")

