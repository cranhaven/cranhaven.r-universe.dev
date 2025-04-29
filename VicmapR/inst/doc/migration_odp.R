## ---- include = FALSE---------------------------------------------------------
eval_check <- all(VicmapR::check_geoserver(quiet = TRUE), !testthat:::on_cran(), sf::sf_extSoftVersion()[["GDAL"]] > 3)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = eval_check
)

## ----setup, eval = eval_check-------------------------------------------------
#  library(VicmapR)
#  library(kableExtra)

## ----old_code, eval = eval_check----------------------------------------------
#  
#  # Old code for obtaining freshwater wetlands in glenelg plain
#  evc_18_1_old <- vicmap_query("datavic:FLORAFAUNA1_NV2005_EVCBCS_18_1") %>%
#    filter(BIOREGION == "Glenelg Plain" & X_EVCNAME %in% c("Deep Freshwater Marsh", "Aquatic Herbland")) %>%
#    select(X_EVCNAME) %>%
#    head(10)
#  
#  collect(evc_18_1_old) %>%
#    kbl() %>%
#    kable_styling()

## ----new_code, eval = eval_check----------------------------------------------
#  # New code for obtaining freshwater wetlands in glenelg plain
#  evc_18_1_new <- vicmap_query("open-data-platform:nv2005_evcbcs") %>%
#    filter(x_subgroupname == "Freshwater" &
#             bioregion == "Glenelg Plain" &
#             x_evcname %in% c("Deep Freshwater Marsh", "Aquatic Herbland")) %>%
#    select(x_evcname) %>%
#    head(10)
#  
#  collect(evc_18_1_new) %>%
#    kbl() %>%
#    kable_styling()

## ----compare_queries, eval = eval_check---------------------------------------
#  cat("Old query (translated):\n")
#  show_query(evc_18_1_old)
#  cat("New query:\n")
#  show_query(evc_18_1_new)

## ---- eval = eval_check-------------------------------------------------------
#  try({
#  vicmap_query(layer = "datavic:FLORAFUANA1_SWOOPING_BIRD")
#  })

