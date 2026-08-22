## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>"
)

## ----eval = FALSE-------------------------------------------------------------
# library(obr)
# 
# inf <- get_efo_economy("inflation")
# table(inf$metric_type, inf$unit)
# #         index  pct
# #   index   372    0
# #   yoy_pct   0 1844

## ----eval = FALSE-------------------------------------------------------------
# inf_yoy <- inf[inf$metric_type == "yoy_pct", ]
# head(inf_yoy[inf_yoy$series == "CPI", ])

## ----eval = FALSE-------------------------------------------------------------
# inf_idx <- inf[inf$metric_type == "index", ]

## ----eval = FALSE-------------------------------------------------------------
# forecast <- get_efo_fiscal()                # 5-year forecast, gbp_bn
# forecast <- forecast[forecast$series == "Net borrowing", ]
# 
# outturn <- get_psnb()                       # historical outturn, series = "PSNB", gbp_bn
# outturn <- outturn[outturn$period >= "2020-21", ]
# 
# # Both have period (fiscal_year), value (gbp_bn), unit. Stack them.
# combined <- rbind(
#   data.frame(source = "outturn",  outturn[,  c("period", "value", "unit")]),
#   data.frame(source = "forecast", forecast[, c("period", "value", "unit")])
# )

## ----eval = FALSE-------------------------------------------------------------
# oct24 <- get_efo_fiscal(vintage = "October 2024")
# mar26 <- get_efo_fiscal(vintage = "March 2026")
# 
# # Net borrowing forecast for 2027-28 from each vintage
# oct24[oct24$series == "Net borrowing" & oct24$period == "2027-28", "value"]
# mar26[mar26$series == "Net borrowing" & mar26$period == "2027-28", "value"]

## ----eval = FALSE-------------------------------------------------------------
# obr_provenance(get_efo_fiscal())
# # $publication: "EFO"
# # $vintage:     "March 2026"
# # $source_url:  ...
# # $retrieved:   timestamp
# # $file_md5:    fingerprint of the underlying spreadsheet
# # $package_version: obr version

