## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(timeseriesdb)

## ---- eval=FALSE--------------------------------------------------------------
#  install.packages("timeseriesdb")

## ---- eval=FALSE--------------------------------------------------------------
#  remotes::install_github("mbannert/timeseriesdb")

## ---- eval=FALSE--------------------------------------------------------------
#  library(timeseriesdb)
#  con <- db_connection_create("yourdbname",
#                              user = "tsdb_admin",
#                              host = "localhost")
#  install_timeseriesdb(con)

## ---- eval=FALSE--------------------------------------------------------------
#  tsl <- list(AirPassengers = AirPassengers)
#  db_ts_store(con, tsl)

## ---- eval=FALSE--------------------------------------------------------------
#  system.file(package = "timeseriesdb")

## ---- eval=FALSE--------------------------------------------------------------
#  con <- db_connection_create(
#    dbname = "postgres",
#    user = "dev_admin",
#    host = "localhost",
#    passwd = "dev_admin",
#    port = 1111
#  )

