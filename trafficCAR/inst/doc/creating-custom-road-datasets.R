## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4,
  dpi = 72,
  fig.retina = 1
)

## ----eval = FALSE-------------------------------------------------------------
# # install.packages(c("osmdata", "sf"))
# library(osmdata)
# library(sf)

## ----eval = FALSE-------------------------------------------------------------
# city <- "College Station, Texas, USA"
# 
# q <- opq(city) |>
#   add_osm_feature(key = "highway")
# 
# osm <- osmdata_sf(q)

## ----eval = FALSE-------------------------------------------------------------
# roads <- osm$osm_lines

## ----eval = FALSE-------------------------------------------------------------
# roads <- roads[, c("highway", "name", "geometry")]
# roads <- sf::st_make_valid(roads)
# 
# # Optional: simplify to reduce object size.
# # Note: dTolerance is in the units of your coordinate reference system (CRS).
# # For best results, consider transforming to a projected CRS with meter units first.
# roads <- sf::st_simplify(roads, dTolerance = 10)
# 
# unique(sf::st_geometry_type(roads))

## ----eval = FALSE-------------------------------------------------------------
# sf::st_write(
#   roads,
#   "roads_cstat.geojson",
#   delete_dsn = TRUE,
#   quiet = TRUE
# )

## ----eval = FALSE-------------------------------------------------------------
# roads_cstat <- sf::st_read("roads_cstat.geojson", quiet = TRUE)
# 
# saveRDS(roads_cstat, "roads_cstat.rds")
# roads_cstat <- readRDS("roads_cstat.rds")

## ----eval = FALSE-------------------------------------------------------------
# library(usethis)
# 
# roads_cstat <- sf::st_read("roads_cstat.geojson", quiet = TRUE)
# use_data(roads_cstat, overwrite = TRUE, compress = "xz")

## ----eval = FALSE-------------------------------------------------------------
# library(trafficCAR)
# 
# data(roads_small, package = "trafficCAR")
# roads_cstat <- roads_small
# 
# segs <- roads_to_segments(
#   roads_cstat,
#   split_at_intersections = TRUE
# )
# 
# adj <- build_adjacency(segs)

## ----eval=FALSE---------------------------------------------------------------
# library(sf)
# library(usethis)
# 
# roads_cstat_small <- sf::st_read(
#   "data-raw/roads_cstat_small.geojson",
#   quiet = TRUE
# )
# 
# use_data(roads_cstat_small, overwrite = TRUE, compress = "xz")

## ----eval=FALSE---------------------------------------------------------------
# library(sf)
# library(trafficCAR)
# 
# roads <- sf::st_read("roads_cstat.geojson", quiet = TRUE)
# segs <- roads_to_segments(roads)

