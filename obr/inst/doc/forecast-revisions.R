## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>"
)

## ----eval = FALSE-------------------------------------------------------------
# library(obr)
# 
# rev <- get_forecast_revisions(unit = "gbp_bn")
# unique(rev$component)
# # "Total"
# # "Policy"
# # "of which: receipts"
# # "of which: spending"
# # "Classifications and one-offs"
# # "Underlying"
# # "of which: receipts"
# # "of which: debt interest"
# # "of which: non-interest spending"

## ----eval = FALSE-------------------------------------------------------------
# top <- rev[rev$component %in% c(
#   "Total", "Policy",
#   "Classifications and one-offs", "Underlying"
# ), ]
# 
# # Most recent vintage's revision for each fiscal year
# latest <- max(top$forecast_date)
# top[top$forecast_date == latest, ]

## ----eval = FALSE-------------------------------------------------------------
# rev_pct <- get_forecast_revisions(unit = "pct_gdp")

## ----eval = FALSE-------------------------------------------------------------
# panel <- obr_forecast_panel("PSNB")
# # Every PSNB forecast for 2027-28
# panel[, c("forecast_date", "2027-28")]

## ----eval = FALSE-------------------------------------------------------------
# panel <- obr_forecast_panel("PSNB")
# 
# # Forecast value for FY 2024-25 across every vintage
# psnb_2425 <- panel[, c("forecast_date", "2024-25")]
# psnb_2425 <- psnb_2425[!is.na(psnb_2425[["2024-25"]]), ]
# psnb_2425

## ----eval = FALSE-------------------------------------------------------------
# prov <- obr_provenance(rev)
# sprintf("OBR Forecast Revisions Database, vintage %s. File MD5: %s. Retrieved %s.",
#         prov$vintage,
#         prov$file_md5,
#         format(prov$retrieved, "%Y-%m-%d"))

