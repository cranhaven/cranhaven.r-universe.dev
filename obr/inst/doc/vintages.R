## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>"
)
NOT_CRAN <- identical(Sys.getenv("NOT_CRAN"), "true")

## -----------------------------------------------------------------------------
library(obr)

efos <- obr_efo_vintages()
nrow(efos)
head(efos, 3)
tail(efos, 3)

## -----------------------------------------------------------------------------
obr_as_of("2024-12-15")
obr_as_of("2010-05-01") |> tryCatch(error = function(e) conditionMessage(e))

## -----------------------------------------------------------------------------
obr_pin("October 2024")
obr_pinned()
obr_unpin()
obr_pinned()

## ----eval = FALSE-------------------------------------------------------------
# # Pull two vintages of the same fiscal table to compare forecast revisions
# oct24 <- get_efo_fiscal(vintage = "October 2024")
# mar26 <- get_efo_fiscal(vintage = "March 2026")
# 
# # Net borrowing forecast for 2027-28 from each vintage. v0.4.0 schema
# # uses `period` for the time column (with `period_type = "fiscal_year"`).
# oct24[oct24$series == "Net borrowing" & oct24$period == "2027-28", ]
# mar26[mar26$series == "Net borrowing" & mar26$period == "2027-28", ]

## ----eval = FALSE-------------------------------------------------------------
# obr_provenance(get_efo_fiscal(vintage = "October 2024"))
# # $publication: "EFO"
# # $vintage:     "October 2024"
# # $source_url:  https://obr.uk/download/october-2024-economic-and-fiscal-outlook-...
# # $retrieved:   timestamp of download
# # $file_md5:    MD5 fingerprint of the underlying spreadsheet
# # $package_version: obr version that produced the object

## ----eval = FALSE-------------------------------------------------------------
# fc  <- get_forecasts("PSNB")
# fc[fc$forecast_date == "October 2024", ]

