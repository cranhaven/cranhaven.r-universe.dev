## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  fig.width=8, 
  fig.height=6
)
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(purl = NOT_CRAN)

## ----load-smapr----------------------------------------------------------
library(smapr)
library(sp)
library(raster)

## ----set-creds, eval = FALSE---------------------------------------------
#  set_smap_credentials("myusername", "mypassword")

## ----find-data, eval = NOT_CRAN------------------------------------------
available_data <- find_smap(id = 'SPL4SMAU', dates = '2018-06-01', version = 4)

## ----head-data, eval = NOT_CRAN------------------------------------------
str(available_data)

## ----download-data, eval = NOT_CRAN--------------------------------------
local_files <- download_smap(available_data, overwrite = FALSE, verbose = FALSE)

## ----print-filenames, eval = NOT_CRAN------------------------------------
local_files$name[1:2]

## ----list-smap, eval = NOT_CRAN------------------------------------------
list_smap(local_files[1, ])

## ----list-more-smap, eval = NOT_CRAN-------------------------------------
list_smap(local_files[1, ], all = TRUE)

## ----extract-data, eval = NOT_CRAN---------------------------------------
sm_raster <- extract_smap(local_files, '/Analysis_Data/sm_rootzone_analysis')

## ----print-raster, eval = NOT_CRAN---------------------------------------
sm_raster

## ----plot-raster, eval = NOT_CRAN----------------------------------------
plot(sm_raster)

## ----create-extent, eval = NOT_CRAN--------------------------------------
co_extent <- extent(c(-109, -102, 37, 41))
co_extent <- as(co_extent, "SpatialPolygons")
sp::proj4string(co_extent) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
co_extent

## ----project-extent, eval = NOT_CRAN-------------------------------------
proj_co_extent <- spTransform(co_extent, crs(sm_raster))

## ----crop-raster, eval = NOT_CRAN----------------------------------------
co_soil_moisture <- crop(sm_raster, proj_co_extent)
plot(co_soil_moisture)

## ----mask-raster, eval = NOT_CRAN----------------------------------------
plot(mask(sm_raster[[1]], proj_co_extent))

## ----inverse-mask, eval = NOT_CRAN---------------------------------------
plot(mask(sm_raster[[1]], proj_co_extent, inverse = TRUE))

## ----get-mean, eval = NOT_CRAN-------------------------------------------
mean_sm <- calc(sm_raster, fun = mean)
plot(mean_sm, main = 'Mean soil moisture')

## ----surface-vs-rootzone, eval = NOT_CRAN--------------------------------
surface_raster <- extract_smap(local_files, 
                               name = '/Analysis_Data/sm_surface_analysis')

# compute mean
mean_surface_sm <- calc(surface_raster, fun = mean)

# compare values
plot(values(mean_sm), values(mean_surface_sm), col = 'dodgerblue', cex = .1, 
     xlab = 'Rootzone soil moisture', ylab = 'Surface soil moisture', bty = 'n')
abline(0, 1, lty = 2)

