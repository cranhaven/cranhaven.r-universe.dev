## ----setup, include = FALSE---------------------------------------------------
ev = unname(Sys.info()["user"]) == "marius"
knitr::opts_chunk$set(
  collapse = TRUE,
  eval = ev
)

## ----data_download, echo=TRUE, results='hide'---------------------------------
options(timeout = max(1800, getOption("timeout")))
dest_dir = tempdir()
download.file("https://hs-bochum.sciebo.de/s/8mKf1Ye1z9SRUr8/download", destfile=file.path(dest_dir, "MOD11A2.zip"),mode = "wb")
unzip(file.path(dest_dir, "MOD11A2.zip"), exdir = file.path(dest_dir,"MOD11A2"))
unlink(file.path(dest_dir, "MOD11A2.zip"))

## ----eval=TRUE----------------------------------------------------------------
library(gdalcubes)
collection_formats()

## -----------------------------------------------------------------------------
files = list.files(file.path(dest_dir,"MOD11A2"), pattern=".hdf$", full.names = TRUE)
MODIS.collection = create_image_collection(files, "MxD11A2")

## -----------------------------------------------------------------------------
v = cube_view(extent=MODIS.collection, srs = "EPSG:3857", dx = 5000, dy = 5000, dt = "P1M")
MODIS.cube = raster_cube(MODIS.collection, v)
MODIS.cube

names(MODIS.cube)
dim(MODIS.cube)

## ----fig.align="center", fig.dim=c(4.5,4.5)-----------------------------------
MOD11A2.bandselect = select_bands(MODIS.cube, c("LST_DAY","LST_NIGHT"))
MOD11A2.daynight_difference = 
  apply_pixel(MOD11A2.bandselect, "0.02*(LST_DAY-LST_NIGHT)",names = "LST_difference")
MOD11A2.reduce = reduce_time(MOD11A2.daynight_difference, "median(LST_difference)")
plot(MOD11A2.reduce, col=heat.colors, key.pos=1)

## ----fig.dim=c(7,3.5),fig.align="center"--------------------------------------
# update v by changing temporal extent only
v1 = cube_view(view=v, extent=list(t0="2018-06", t1="2018-09")) 
raster_cube(MODIS.collection, v1)  |>
  select_bands(c("LST_DAY")) |>
  apply_pixel("LST_DAY * 0.02") |> # apply scale
  window_time(kernel=c(-1,1), window=c(1,0)) |>
  plot(col=terrain.colors, key.pos=1, zlim=c(-25,25), t = 2:4, ncol = 3)

