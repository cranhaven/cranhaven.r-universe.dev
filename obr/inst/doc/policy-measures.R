## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>"
)

## ----eval = FALSE-------------------------------------------------------------
# library(obr)
# 
# pmd <- get_policy_measures()      # tax + spending, all years
# obr_provenance(pmd)$vintage       # which PMD vintage was downloaded

## ----eval = FALSE-------------------------------------------------------------
# # Every tax measure scored at the October 2024 Budget,
# # ordered by 2025-26 effect
# oct24 <- get_policy_measures(type = "tax", since = "2025-26")
# oct24 <- oct24[grepl("October 2024", oct24$event) &
#                oct24$fiscal_year == "2025-26", ]
# oct24 <- oct24[order(-oct24$value_mn), ]
# head(oct24[, c("measure", "head", "value_mn")])

## ----eval = FALSE-------------------------------------------------------------
# # Every alcohol-duty measure since 2010
# alc <- get_policy_measures(type = "tax", search = "alcohol", since = "2010-11")
# unique(alc$event)

## ----eval = FALSE-------------------------------------------------------------
# pm  <- get_policy_measures(type = "tax", since = "2024-25")
# agg <- policy_measures_summary(pm)
# agg[agg$event == "Budget October 2024", ]

## ----eval = FALSE-------------------------------------------------------------
# obr_provenance(agg)$vintage

## ----eval = FALSE-------------------------------------------------------------
# pm   <- get_policy_measures(type = "tax")
# parl <- pm[pm$fiscal_year == "2027-28" &
#            grepl("(October 2024|2025|2026)", pm$event), ]
# agg  <- policy_measures_summary(parl)
# agg[order(agg$event), c("event", "value_mn")]

