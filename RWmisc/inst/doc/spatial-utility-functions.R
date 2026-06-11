## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 3, 
  fig.align = 'center'
)

## old graphical parameters
oldpar <- par(mfrow = c(1,2))

## ----setup, message = FALSE, warning = FALSE, results = 'hide'----------------
library(sf)
library(raster)
library(RWmisc)

## ----overlap-weight-----------------------------------------------------------
## create three overlapping squares
polys_t <- st_sfc(list(st_polygon(list(rbind(c(2,2), c(2,6), c(6,6),
                                             c(6,2), c(2, 2)))),
                       st_polygon(list(rbind(c(8,8), c(4,8), c(4,4),
                                             c(8,4), c(8,8)))),
                       st_polygon(list(rbind(c(3,3), c(3,7), c(7,7),
                                             c(7,3), c(3,3))))),
                  crs = st_crs('OGC:CRS84'))

## create raster
raster_t <- raster(nrows = 6, ncols = 6, xmn = 2, xmx = 8, ymn = 2, ymx = 8,
                   vals = 1:36, crs = CRS(st_crs(polys_t)$proj4string))

## set plotting parameters
par(mfrow = c(1, 3))

## plot raw raster values
plot(raster_t, main = 'raw')
plot(polys_t, add = TRUE)

## plot count of overlapping polygons
plot(overlap.weight(raster_t, polys_t, count = TRUE), main = "count")
plot(polys_t, add = TRUE)

## plot overlap-weighted raster values
plot(overlap.weight(raster_t, polys_t), main = "weighted")
plot(polys_t, add = TRUE)

## ----projectUTM---------------------------------------------------------------
## read in North Carolina shapefile
nc <- st_read(system.file("shape/nc.shp", package="sf"))

## transform crs to WGS84 and inspect CRS
nc <- st_transform(nc, st_crs('OGC:CRS84'))
st_crs(nc)

## project to UTM and inspect CRS
st_crs(projectUTM(nc))

## ----projectUTM-plot----------------------------------------------------------
## set plotting parameters
par(mfrow = c(1, 2), mar = rep(0, 4))

## plot WGS84 and UTM projected North Carolina
plot(nc$geometry)
plot(projectUTM(nc)$geometry)

## ----point-poly-dist----------------------------------------------------------
## create north carolina centroids
nc_centroids <- st_centroid(nc)

## calculate maximum distance
point.poly.dist(nc_centroids[53,]$geometry, nc[53,]$geometry, max = TRUE)

## ----point-poly-dist-illus----------------------------------------------------
nc_points <- st_geometry(nc[53,]) %>%
  st_cast('POINT')

farthest_ind <- st_distance(nc_points, nc_centroids[53,]) %>%
  which.max()

farthest_point <- rbind(st_coordinates(nc_points[farthest_ind]),
                        st_coordinates(nc_centroids[53,])) %>% 
  st_linestring()

nearest_ind <- st_distance(nc_points, nc_centroids[53,]) %>%
  which.min()

nearest_point <- rbind(st_coordinates(nc_points[nearest_ind]),
                        st_coordinates(nc_centroids[53,])) %>% 
  st_linestring()

## plot
par(mar = rep(0,4))
plot(nc[53,]$geometry)
plot(nc_centroids[53,]$geometry, add = TRUE)
plot(farthest_point, add = TRUE, col = 'red')
plot(nearest_point, add = TRUE, col = 'blue')

## ----point-poly-dist-benchmark------------------------------------------------
microbenchmark::microbenchmark(pk = point.poly.dist(nc_centroids[53,]$geometry,
                                                    nc[53,]$geometry, max = TRUE),
                               sf = st_distance(st_cast(st_geometry(nc[53,]),
                                                        'POINT')[farthest_ind],
                                                nc_centroids[53,]),
                               times = 100)

## ----oldpar, echo = FALSE-----------------------------------------------------
par(oldpar)

