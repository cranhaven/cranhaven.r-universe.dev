## ---- echo = FALSE------------------------------------------------------------
geoserver_connected <- VicmapR::check_geoserver(quiet = TRUE)

knitr::opts_chunk$set(
  collapse = TRUE,
  echo = TRUE,
  comment = "#>", 
  eval = all(geoserver_connected, !testthat:::on_cran()),
  purl = FALSE
)

