## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  eval = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
#  library(rsat)
#  set_credentials("rsat.package","UpnaSSG.2021")

## ----search_review------------------------------------------------------------
#  ip <- st_sf(st_as_sfc(st_bbox(c(
#    xmin = -9.755859,
#    xmax =  4.746094,
#    ymin = 35.91557,
#    ymax = 44.02201
#  ), crs = 4326)))
#  toi <- seq(as.Date("2021-01-10"),as.Date("2021-01-15"),1)

## -----------------------------------------------------------------------------
#  db.path <- file.path(tempdir(),"database")
#  ds.path <- file.path(tempdir(),"datasets")
#  dir.create(db.path)
#  dir.create(ds.path)

## -----------------------------------------------------------------------------
#  filomena <- new_rtoi(name = "filomena",
#                       region = ip,
#                       db_path = db.path,
#                       rtoi_path = ds.path)

## -----------------------------------------------------------------------------
#  rsat_search(region = filomena, product = c("mod09ga"), dates = toi)

## -----------------------------------------------------------------------------
#  rsat_download(filomena)

## ---- eval=FALSE--------------------------------------------------------------
#  rsat_mosaic(filomena)

## ---- eval=FALSE--------------------------------------------------------------
#  list.files(file.path(ds.path, "filomena", "Modis/mod09ga/mosaic"), full.name = TRUE)

## -----------------------------------------------------------------------------
#  plot(filomena, as.Date("2021-01-11"))

## -----------------------------------------------------------------------------
#  plot(filomena, as.Date("2021-01-11"),xsize = 500, ysize = 500)

## -----------------------------------------------------------------------------
#  plot(filomena,
#       as.Date("2021-01-11"),
#       xsize = 500,
#       ysize = 500,
#       band_name = c("swir1", "nir", "blue"))

## ----basic_ndsi, eval = FALSE-------------------------------------------------
#  NDSI = function(green, swir1){
#    ndsi <- (green - swir1)/(green + swir1)
#    return(ndsi)
#  }

## ----basic_variables----------------------------------------------------------
#  show_variables()

## ----basic_derive, eval = FALSE-----------------------------------------------
#  rsat_derive(filomena, product = "mod09ga", variable = "ndsi", fun = NDSI)

## -----------------------------------------------------------------------------
#  plot(filomena,
#       as.Date("2021-01-11"),
#       variable = "ndsi",
#       xsize = 500,
#       ysize = 500,
#       zlim = c(-1,1))

## ----basic_cloud, eval=FALSE--------------------------------------------------
#  rsat_cloudMask(filomena)

## ----basic_ndsi_import, eval = FALSE------------------------------------------
#  ndsi.img <- rsat_get_raster(filomena, "mod09ga", "ndsi")
#  ndsi.img <- clamp(ndsi.img, -1, 1)

## ----basic_mask, eval = FALSE-------------------------------------------------
#  clds.msk <- rsat_get_raster(filomena, "mod09ga", "CloudMask")

## ----basic_mask_resample------------------------------------------------------
#  clds.msk <- resample(clds.msk, ndsi.img, method = "ngb")

## ----basic_mask_apply---------------------------------------------------------
#  ndsi.filt <- ndsi.img * clds.msk
#  names(ndsi.filt) <- names(clds.msk) # keep the names

## ----basic_composite----------------------------------------------------------
#  snow.spain <- calc(ndsi.filt, max, na.rm = TRUE)

## ----basic_ndsi_map-----------------------------------------------------------
#  library(tmap)
#  tm_shape(snow.spain) + tm_raster(style = "cont")

