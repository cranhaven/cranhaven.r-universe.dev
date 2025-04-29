## ---- echo = FALSE------------------------------------------------------------
geoserver_connected <- VicmapR::check_geoserver(quiet = TRUE)

knitr::opts_chunk$set(
  collapse = TRUE,
  echo = TRUE,
  comment = "#>",
  eval = all(VicmapR::check_geoserver(quiet = TRUE), !testthat:::on_cran(), sf::sf_extSoftVersion()[["GDAL"]] > 3),
  purl = FALSE
)

