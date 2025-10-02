## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  library(dafishr)
#  
#  vms_download(year = 2019, destination.folder = getwd())

## ----eval=FALSE---------------------------------------------------------------
#  library(dafishr)
#  data("sample_dataset")
#  vms_cleaned <- vms_clean(sample_dataset)
#  

## ----eval=FALSE---------------------------------------------------------------
#  data("mx_inland") # Shapefile of inland Mexico area
#  vms_cleaned_land <- clean_land_points(vms_cleaned, mx_inland)
#  

## ----eval=FALSE, message=FALSE, warning=FALSE---------------------------------
#  data("mx_ports")
#  
#  # If you are just testing, it is a good idea to subsample...
#  # it takes a while on the full data!
#  
#  vms_subset <- dplyr::sample_n(vms_cleaned, 1000)
#  with_ports <- join_ports_locations(vms_subset)
#  

## ----eval=FALSE---------------------------------------------------------------
#  with_ports_sf <- sf::st_as_sf(with_ports,
#                                coords = c("longitude", "latitude"),
#                                crs = 4326)
#  
#  data("mx_shape")
#  
#  library(ggplot2)
#  ggplot2::ggplot(mx_shape) +
#    geom_sf(col = "gray90") +
#    geom_sf(data = with_ports_sf, aes(col = location)) +
#    facet_wrap(~ location) +
#    theme_bw()

