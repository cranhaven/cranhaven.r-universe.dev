## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
knitr::opts_chunk$set(eval = FALSE)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
#  library(pct)
#  library(dplyr)
#  library(sf)
#  library(tmap)

## -----------------------------------------------------------------------------
#  region_name = "wales"
#  zones_all = get_pct_zones(region_name)

## ----include=TRUE-------------------------------------------------------------
#  unique(zones_all$lad_name)

## -----------------------------------------------------------------------------
#  la_name = "Cardiff"

## ----include=TRUE, fig.align='center', fig.width = 7, fig.height = 6----------
#  zones = zones_all %>%
#    filter(lad_name == la_name)
#  plot(zones["bicycle"])

## -----------------------------------------------------------------------------
#  rnet_all = pct::get_pct_rnet(region_name)
#  rnet = rnet_all[zones, ]

## ----include=TRUE, fig.align='center', fig.width = 7, fig.height = 6----------
#  plot(zones$geometry)
#  plot(rnet["dutch_slc"], add = TRUE)

## -----------------------------------------------------------------------------
#  pct_zones_rnet = function(la_name, region_name = "devon") {
#    zones_all = pct::get_pct_zones(region_name)
#    zones = zones_all %>%
#    filter(lad_name == la_name)
#    plot(zones["bicycle"])
#    rnet_all = pct::get_pct_rnet(region_name)
#    rnet = rnet_all[zones, ]
#    plot(zones$geometry)
#    plot(rnet["dutch_slc"], add = TRUE)
#    list(zones = zones, rnet = rnet)
#  }
#  
#  plymouth_results = pct_zones_rnet(la_name = "Plymouth")
#  exeter_results = pct_zones_rnet(la_name = "Exeter")

## -----------------------------------------------------------------------------
#  tmap_mode("view")
#  b = c(0, 100, 200, 500, 1000)
#  m1 = tm_shape(plymouth_results$rnet) +
#    tm_lines("dutch_slc", breaks = b, palette = "viridis", lwd = 2) +
#    tm_scale_bar()
#  m2 = tm_shape(exeter_results$rnet) +
#    tm_lines("dutch_slc", breaks = b, palette = "viridis", lwd = 2) +
#    tm_scale_bar()
#  tmap_arrange(m1, m2, ncol = 2)

