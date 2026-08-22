## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  fig.width = 6.5, 
  fig.height = 5.5
)

## ----setup--------------------------------------------------------------------
library(roads)
library(terra)
library(dplyr)
library(sf)

# prep the terra rasters for use
dem_example <- prepExData(dem_example)

## ----elev---------------------------------------------------------------------
plot(dem_example$ex_elev)

## ----water--------------------------------------------------------------------
plot(dem_example$ex_wat)

## -----------------------------------------------------------------------------
# set water to NA when a cell is > 50% water
wat_use <- classify(dem_example$ex_wat, matrix(c(0.5, 1, NA), nrow = 1))

# set elev to NA when water is NA
elev_use <- mask(dem_example$ex_elev, wat_use)

# Now change water to NA when it is < 1% water
wat_use <- mask(wat_use, wat_use < 0.01, maskvalue = TRUE)

wat_use <- (wat_use *100) * -504 - 16178

plot(wat_use)

## -----------------------------------------------------------------------------
# set elev to 0 when wat is NA
elev_use <- mask(elev_use, wat_use, inverse = TRUE, updatevalue = 0)

# add wat_use to elev when not NA
wt_rast <- sum(elev_use, wat_use, na.rm = TRUE)

par(mar = c(0,0,0,0.25))

plot(wt_rast, breaks = c(-40000, -30000, -20000, -16178, 0, 1:10*300, NA), 
     col = c(terra::map.pal("blues", 5) %>% rev(), terra::map.pal("oranges", 10)), 
     colNA = "grey50", mar = c(2, 2, 2, 6.5))

## -----------------------------------------------------------------------------
# Get landing points

for_area <- is.na(wat_use) & !is.na(elev_use) & elev_use < 2000

names(for_area) <- "forest"

# set seed to make repeatable
set.seed(1235)

lnds <- spatSample(for_area, 20, method = "stratified", as.points = TRUE,
                   ext = ext(for_area)-0.001) %>%
  st_as_sf() %>% 
  filter(forest == 1) %>% 
  mutate(id = 1:n())

plot(wt_rast, breaks = c(-40000, -30000, -20000, -16178, 0, 1:10*300), 
     col = c(terra::map.pal("blues", 5) %>% rev(), terra::map.pal("oranges", 10)), 
     colNA = "grey50", mar = c(2, 2, 2, 6.5))

plot(lnds, add = TRUE, col = "red")


## -----------------------------------------------------------------------------
# Get starting road

# create line interactively
# line <- draw("line")
# 
# line <- st_as_sf(line)

# get line non-interactively
rd_in <- structure(list(
  geometry = structure(list(
    structure(c(-118.103238840217, -118.103238840217, -118.112765313949, -118.115940805193, 
                -118.115940805193, -118.106414331461, -118.106414331461, -118.100063348973,
                -118.077834910265, -118.074659419021, 
                49.5276240233455, 49.5785159559446, 49.6090511155041, 
                49.6355149204556, 49.6945495622705, 49.6965852395745,
                49.7108349807022, 49.7637625906053, 49.780048009037,
                49.8614751011955), 
              dim = c(10L, 2L), 
              class = c("XY", "LINESTRING", "sfg"))),
    n_empty = 0L, class = c("sfc_LINESTRING", "sfc"),
    precision = 0, 
    bbox = structure(c(xmin = -118.115940805193, 
                       ymin = 49.5276240233455, 
                       xmax = -118.074659419021, 
                       ymax = 49.8614751011955), class = "bbox"),
    crs = structure(list(input = NA_character_, wkt = NA_character_),
                    class = "crs"))), 
  row.names = 1L, sf_column = "geometry",
  agr = structure(integer(0), class = "factor", 
                  levels = c("constant", "aggregate","identity"), 
                  names = character(0)), class = c("sf", "data.frame")) %>% 
  st_set_crs(st_crs(lnds))


## ----error=TRUE---------------------------------------------------------------
try({
rd_proj <- projectRoads(lnds, wt_rast, rd_in, weightFunction = gradePenaltyFn, 
                        roadsInWeight = FALSE, roadMethod = "ilcp")
})

## ----proj-rds-----------------------------------------------------------------
rd_proj <- projectRoads(lnds, wt_rast, rd_in, weightFunction = gradePenaltyFn, 
                        roadsInWeight = FALSE, roadMethod = "ilcp", limit = 30)

plotRoads(rd_proj, breaks = c(-40000, -30000, -20000, -16178, 0, 1:10*300), 
     col = c(terra::map.pal("blues", 5) %>% rev(), terra::map.pal("oranges", 10)), 
     colNA = "grey50", mar = c(2, 2, 2, 6.5))

rd_proj3 <- projectRoads(lnds, wt_rast, rd_in, weightFunction = gradePenaltyFn, 
                        roadsInWeight = FALSE, roadMethod = "ilcp", limitWeight = 40000^2)

plotRoads(rd_proj3, breaks = c(-40000, -30000, -20000, -16178, 0, 1:10*300), 
     col = c(terra::map.pal("blues", 5) %>% rev(), terra::map.pal("oranges", 10)), 
     colNA = "grey50", mar = c(2, 2, 2, 6.5))

## ----error = TRUE-------------------------------------------------------------
try({
# set > 10% water to NA 
wat_10 <- classify(dem_example$ex_wat, matrix(c(0.1, 1, NA), nrow = 1))

wt_rast <- mask(wt_rast, wat_10)

plot(wt_rast, breaks = c(-40000, -30000, -20000, -16178, 0, 1:10*300), 
     col = c(terra::map.pal("blues", 5) %>% rev(), terra::map.pal("oranges", 10)), 
     colNA = "grey50", mar = c(2, 2, 2, 6.5))

projectRoads(lnds, wt_rast, rd_in, weightFunction = gradePenaltyFn,
             roadsInWeight = FALSE, roadMethod = "ilcp", limit = 30)

})

## -----------------------------------------------------------------------------
wt_rast <- subst(wt_rast, from = NA, to = -40000^2)

rd_proj2 <- projectRoads(lnds, wt_rast, rd_in, weightFunction = gradePenaltyFn,
             roadsInWeight = FALSE, roadMethod = "ilcp", limit = 30)


plotRoads(rd_proj2, breaks = c(-40000^2, -40000, -30000, -20000, -16178, 0, 1:10*300), 
     col = c(terra::map.pal("blues", 6) %>% rev(), terra::map.pal("oranges", 10)), 
     colNA = "grey50", mar = c(2, 2, 2, 8.5))

