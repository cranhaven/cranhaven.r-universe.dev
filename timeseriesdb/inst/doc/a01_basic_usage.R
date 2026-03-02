## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(timeseriesdb)

## ---- eval=FALSE--------------------------------------------------------------
#  con <- db_connection_create(
#    dbname = "postgres",
#    user = "dev_admin",
#    host = "localhost",
#    passwd = "dev_admin",
#    port = 1111
#  )

## ---- eval=FALSE--------------------------------------------------------------
#  tsl <- list(
#    AirPassengers = AirPassengers,
#    JohnsonJohnson = JohnsonJohnson
#  )
#  db_ts_store(con, tsl, "main",
#    schema = "tsdb_test"
#  )

## ---- eval=FALSE--------------------------------------------------------------
#  
#  out <- db_ts_read(con, c(
#    "JohnsonJohnson",
#    "AirPassengers"
#  ),
#  schema = "tsdb_test"
#  )
#  
#  library(dygraphs)
#  library(xts)
#  xts_for_viz <- do.call("cbind",lapply(out,as.xts))
#  dygraph(xts_for_viz) %>%
#    dySeries("JohnsonJohnson", axis="y2")

## ----echo=FALSE,message=FALSE, warning=FALSE----------------------------------
out <- list(
  AirPassengers = AirPassengers,
  JohnsonJohnson = JohnsonJohnson
)
library(dygraphs)
library(xts)
xts_for_viz <- do.call("cbind",lapply(out,as.xts))
dygraph(xts_for_viz) %>% 
  dySeries("JohnsonJohnson", axis="y2")

## -----------------------------------------------------------------------------
library(timeseriesdb)
tsm <- create_tsmeta(
  JohnsonJohnson = list(
    title = "Historical Quarterly earnings (dollars) per Johnson & Johnson share",
    source = "Shumway, R. H. and Stoffer, D. S. (2000) Time Series Analysis and its Applications. Second Edition. Springer. Example 1.1"
  ),
  AirPassengers = list(
    title = "The classic Box & Jenkins airline data. Monthly totals of international airline passengers",
    source = "Box, G. E. P., Jenkins, G. M. and Reinsel, G. C. (1976) Time Series Analysis, Forecasting and Control. Third Edition. Holden-Day. Series G."
  )
)

tsm

## ---- eval=FALSE--------------------------------------------------------------
#  
#  db_store_ts_metadata(con,
#    tsm,
#    valid_from = Sys.Date(),
#    schema = "tsdb_test"
#  )

## ---- eval=FALSE--------------------------------------------------------------
#  tsm <- db_read_ts_metadata(con, c(
#    "JohnsonJohnson",
#    "AirPassengers"
#  ),
#  schema = "tsdb_test"
#  )

