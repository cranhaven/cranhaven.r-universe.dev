## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
tryCatch({
  Sys.setlocale("LC_ALL", "English")
})
eval_chunks <-
  CopernicusMarine:::has_blosc &&
  curl::has_internet() &&
  CopernicusMarine::cms_get_password() != "" &&
  sf::st_drivers("raster", "^HDF5$")$vsi

## ----create_proxy, eval=eval_chunks-------------------------------------------
library(CopernicusMarine) |> suppressMessages()
library(stars) |> suppressMessages()

my_proxy_gc <- cms_zarr_proxy(
  product       = "GLOBAL_ANALYSISFORECAST_PHY_001_024",
  layer         = "cmems_mod_glo_phy-thetao_anfc_0.083deg_P1D-m",
  asset         = "geoChunked")

my_proxy_tc <- cms_zarr_proxy(
  product       = "GLOBAL_ANALYSISFORECAST_PHY_001_024",
  layer         = "cmems_mod_glo_phy-thetao_anfc_0.083deg_P1D-m",
  asset         = "timeChunked")

print(my_proxy_tc)

## ----slice_time, eval=eval_chunks---------------------------------------------
time_slice <- my_proxy_gc[,2000, 1000, 48, 1:400]
show(time_slice)

## ----set_par, results='hide', echo=FALSE--------------------------------------
opar <- par(mar = c(3, 3, 0.5, 0.5), mgp = c(2, .8, 0))

## ----plot_time_slice, eval=eval_chunks----------------------------------------
time_vals <- st_get_dimension_values(time_slice, "time")
time_slice <- st_as_stars(time_slice)
plot(time_vals, time_slice$thetao,
     xlab = "date", ylab = "temperature", type = "l")

## ----reset_par, results='hide', echo=FALSE------------------------------------
par(opar)

## ----slice_area, eval=eval_chunks---------------------------------------------
geo_slice <- my_proxy_tc[,2000:2500, 1500:1750, 50, 500]
plot(geo_slice, col = hcl.colors(10))

