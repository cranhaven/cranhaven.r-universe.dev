## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE)
Sys.setlocale("LC_ALL","en_GB")

## ----libraries, message = FALSE-----------------------------------------------
library(raster)
library(sf)
library(tidyverse)
library(telemac)

## ----read_dem, message = FALSE, fig.width = 6, fig.height = 6-----------------
dem_rast <- raster(system.file("dem/dem_merit_lagos.tif", package = "telemac"))
NAvalue(dem_rast) <- -999 # I defined -999 to be NA when creating the dem file
plot(dem_rast, col = colorRampPalette(c("blue", "green", "red"))(255))

## ----read_bnd-----------------------------------------------------------------
bnd <- st_read(system.file("dem/boundary_lagos.gpkg", package = "telemac"))

## ----triangulate--------------------------------------------------------------
tin_obj <- tin(list(boundary = bnd), s = 90, a = 100^2, q = 30)

## ----tin_obj------------------------------------------------------------------
str(tin_obj)

## ----tin_obj_plot, fig.width = 6, fig.height = 6------------------------------
tin_obj
plot(tin_obj, pch = ".")

## ----geo_obj------------------------------------------------------------------
geo_obj <- geo(tin_obj, dem = dem_rast, title = "title", fname = "geometry.slf",
               n = 5, idp = 2)

## ----geo_object_mod-----------------------------------------------------------
str(geo_obj)
geo_obj$header$title
geo_obj$header$title <- "Merit, RTriangle, 90m"
geo_obj

## ----geo_obj_plot, fig.width = 6, fig.height = 6------------------------------
plot(geo_obj, s = 30, col = colorRampPalette(c("blue", "green", "red"))(255))

## ----cli_object---------------------------------------------------------------
cli_obj <- cli(geo_obj, fname = "boundary.cli")
cli_obj

## ----cli_object_mod-----------------------------------------------------------
str(cli_obj)
cli_obj <- cli_obj %>%
  # open boundary with free depth and velocities
  mutate(lihbor = 4, liubor = 4, livbor = 4)

## ----cas_obj------------------------------------------------------------------
cas_obj <- cas(fname = "steering.cas")
cas_obj

## ----cas_object_mod-----------------------------------------------------------
str(cas_obj)
cas_obj[["VARIABLES FOR GRAPHIC PRINTOUTS"]] <- "H"

## ----t2d_obj------------------------------------------------------------------
t2d_obj <- t2d(title = "Test setup", wdir = "t2d_test",
               cas = cas_obj, geo = geo_obj, cli = cli_obj)
t2d_obj

## ----pre_project, eval = FALSE------------------------------------------------
#  write_t2d(t2d_obj)

## ----simulation, eval = FALSE-------------------------------------------------
#  t2d_obj <- simulate_t2d(t2d_obj, log = "test_run.log", res = "test_results.slf",
#                          vars = "water depth", exec = "telemac2d.py")

## ----import_results-----------------------------------------------------------
t2d_obj$res <- results(system.file("telemac/basics/results.slf", package = "telemac"),
                       log = system.file("telemac/basics/test_run.log", package = "telemac"),
                       vars = "water depth", times = 3600 * c(0,1,4,12,24))

## ----results------------------------------------------------------------------
t2d_obj$res
str(t2d_obj$res)

## ----results_export, eval = FALSE---------------------------------------------
#  # change associated file (otherwise the original file would be overwritten)
#  t2d_obj$res <- results(t2d_obj$res, fname = "t2d_test/results_updated.slf")
#  t2d_obj$res <- write_results(t2d_obj)
#  t2d_obj$res

## ----results_plot, fig.width = 6, fig.height = 6------------------------------
plot(t2d_obj$res, s = 30, v = "water depth", t = 4*3600,
     col = c('#eff3ff','#c6dbef','#9ecae1','#6baed6','#3182bd','#08519c'))

