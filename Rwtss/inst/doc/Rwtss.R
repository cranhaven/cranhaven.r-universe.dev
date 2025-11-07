## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- echo = FALSE, fig.align="center", fig.height=3, fig.width=5, fig.cap="A Normalized Difference Vegetation Index (NDVI) time series"----
knitr::include_graphics(system.file("extdata/markdown/figures", "arrays.jpg", package = "Rwtss"))

## ---- echo = FALSE, fig.align="center", fig.height=3, fig.width=5, fig.cap="Events associated to a time series"----
knitr::include_graphics(system.file("extdata/markdown/figures", "time_series.png", package = "Rwtss"))

## ---- eval = FALSE------------------------------------------------------------
#  # Connect to the WTSS server at INPE Brazil
#  wtss_inpe <-  "https://brazildatacube.dpi.inpe.br/wtss/"

## ---- eval = FALSE------------------------------------------------------------
#  # Connect to the WTSS server at INPE Brazil
#  Rwtss::list_coverages(wtss_inpe)

## ---- echo = FALSE------------------------------------------------------------
paste(c("MOD13Q1-6", "MYD13Q1-6"))

## ---- eval = FALSE------------------------------------------------------------
#  # Connect to the WTSS server at INPE Brazil
#  desc <- Rwtss::describe_coverage(wtss_inpe, name = "MOD13Q1-6")

## ---- echo = FALSE------------------------------------------------------------
# Coverage description available in the wtss object
cat("---------------------------------------------------------------------")
cat("WTSS server URL = https://brazildatacube.dpi.inpe.br/wtss")
cat("Cube (coverage) = MOD13Q1-6")
cat("Timeline - 508 time steps")
cat("start_date: 2000-02-18 end_date: 2022-03-22")
cat("---------------------------------------------------------------------")

## ---- eval = FALSE------------------------------------------------------------
#  # Request a time series from the "MOD13Q1" coverage
#  ndvi_ts   <- Rwtss::time_series(
#    URL = wtss_inpe,
#    name = "MOD13Q1",
#    attributes = c("NDVI","EVI"),
#    longitude = -45.00,
#    latitude  = -12.00,
#    start_date = "2000-02-18",
#    end_date = "2016-12-18",
#    token = "YOUR-BDC-TOKEN"
#  )

## ---- echo = FALSE------------------------------------------------------------
ndvi_ts <- readRDS(file = system.file("extdata/ndvi_ts.rds", package = "Rwtss"))
class(ndvi_ts) <- c("wtss", class(ndvi_ts))

## -----------------------------------------------------------------------------
ndvi_ts

## -----------------------------------------------------------------------------
# Showing the contents of a time series
ndvi_ts$time_series[[1]]

