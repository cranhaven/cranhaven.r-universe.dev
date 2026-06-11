#' Project to UTM
#'
#' Project an object in latitude/longitude to UTM.
#'
#' @name projectUTM
#' @param x An `sf` or `sp` object in latitude-longitude CRS.
#'
#' @return An `sf` or `sp` object projected to UTM CRS.
#' @export
#'
#' @examples
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#' st_crs(projectUTM(nc))

projectUTM <- function(x) {

  UseMethod('projectUTM', x)

}

#' @rdname projectUTM
#'
#' @export
projectUTM.sf <- function(x) {

  ## if not long-lat, transform to WGS84
  if (!st_is_longlat(x)) x <- st_transform(x, st_crs('OGC:CRS84'))

  ## find average UTM zone using longitude(s) of sf object
  zone <- chooseUTM(mean(st_coordinates(x)[, 1]))

  ## save latitude mean to determine if majority of features fall in southern hemisphere
  lat.mean <- mean(st_coordinates(x)[, 2])

  ## if average of latitude values is negative, add +south the coordinate reference system
  if (lat.mean >= 0) {

    ## create coordinate reference system object to project spatial object
    zone <- st_crs(paste('+proj=utm +zone=', zone, sep = ''))

  } else {

    ## create coordinate reference system object to project spatial object
    zone <- st_crs(paste('+proj=utm +south +zone=', zone, sep = ''))

  }

  ## project spatial object
  x <- st_transform(x, zone)

  ## return projected spatial object
  x

}

#' @rdname projectUTM
#'
#' @export
projectUTM.sfc <- function(x) {

  ## if not long-lat, transform to WGS84
  if (!st_is_longlat(x)) x <- st_transform(x, st_crs('OGC:CRS84'))

  ## find average UTM zone using longitude(s) of sf object
  zone <- chooseUTM(mean(st_coordinates(x)[, 1]))

  ## save latitude mean to determine if majority of features fall in southern hemisphere
  lat.mean <- mean(st_coordinates(x)[, 2])

  ## if average of latitude values is negative, add +south the coordinate reference system
  if (lat.mean >= 0) {

    ## create coordinate reference system object to project spatial object
    zone <- st_crs(paste('+proj=utm +zone=', zone, sep = ''))

  } else {

    ## create coordinate reference system object to project spatial object
    zone <- st_crs(paste('+proj=utm +south +zone=', zone, sep = ''))

  }

  ## project spatial object
  x <- st_transform(x, zone)

  ## return projected spatial object
  x

}

#' @rdname projectUTM
#'
#' @export
projectUTM.SpatialPointsDataFrame <- function(x) {

  ## if not long-lat, transform to WGS84
  if (!st_is_longlat(x)) x <- st_transform(x, st_crs('OGC:CRS84'))

  ## find average UTM zone using longitude(s) of SpatialPoints object
  zone <- chooseUTM(x@coords[, 1])

  ## save latitude mean to determine if features falls in southern hemisphere
  lat.mean <- mean(x@coords[, 2])

  ## if average of latitude values is negative, add +south the coordinate reference system
  if (lat.mean >= 0) {

    ## create coordinate reference system object to project spatial object
    zone <- CRS(paste('+proj=utm +zone=', zone, sep = ''))
  } else {

    ## create coordinate reference system object to project spatial object
    zone <- CRS(paste('+proj=utm +south +zone=', zone, sep = ''))
  }

  ## project spatial object
  x <- spTransform(x, zone)

  ## return projected spatial object
  x

}

#' @rdname projectUTM
#'
#' @export
projectUTM.SpatialPoints <- function(x) {

  ## if not long-lat, transform to WGS84
  if (is.projected(x)) x <- spTransform(x, st_crs('OGC:CRS84')$proj4string)

  ## find average UTM zone using longitude(s) of SpatialPoints object
  zone <- chooseUTM(x@coords[, 1])

  ## save latitude mean to determine if features falls in southern hemisphere
  lat.mean <- mean(x@coords[, 2])

  ## if average of latitude values is negative, add +south the coordinate reference system
  if (lat.mean >= 0) {

    ## create coordinate reference system object to project spatial object
    zone <- CRS(paste('+proj=utm +zone=', zone, sep = ''))
  } else {

    ## create coordinate reference system object to project spatial object
    zone <- CRS(paste('+proj=utm +south +zone=', zone, sep = ''))
  }

  ## project spatial object
  x <- spTransform(x, zone)

  ## return projected spatial object
  x

}

#' @rdname projectUTM
#'
#' @export
projectUTM.SpatialPolygonsDataFrame <- function(x) {

  ## if not long-lat, transform to WGS84
  if (is.projected(x)) x <- spTransform(x, st_crs('OGC:CRS84')$proj4string)

  ## find average UTM zone using longitude(s) of SpatialPolygons object
  zone <- chooseUTM(polycoords(x)[, 1])

  ## save latitude mean to determine if features falls in southern hemisphere
  lat.mean <- mean(polycoords(x)[, 2])

  ## if average of latitude values is negative, add +south the coordinate reference system
  if (lat.mean >= 0) {

    ## create coordinate reference system object to project spatial object
    zone <- CRS(paste('+proj=utm +zone=', zone, sep = ''))
  } else {

    ## create coordinate reference system object to project spatial object
    zone <- CRS(paste('+proj=utm +south +zone=', zone, sep = ''))
  }

  ## project spatial object
  x <- spTransform(x, zone)

  ## return projected spatial object
  x

}

#' @rdname projectUTM
#'
#' @export
projectUTM.SpatialPolygons <- function(x) {

  ## if not long-lat, transform to WGS84
  if (is.projected(x)) x <- spTransform(x, st_crs('OGC:CRS84')$proj4string)

  ## find average UTM zone using longitude(s) of SpatialPolygons object
  zone <- chooseUTM(polycoords(x)[, 1])

  ## save latitude mean to determine if features falls in southern hemisphere
  lat.mean <- mean(polycoords(x)[, 2])

  ## if average of latitude values is negative, add +south the coordinate reference system
  if (lat.mean >= 0) {

    ## create coordinate reference system object to project spatial object
    zone <- CRS(paste('+proj=utm +zone=', zone, sep = ''))
  } else {

    ## create coordinate reference system object to project spatial object
    zone <- CRS(paste('+proj=utm +south +zone=', zone, sep = ''))
  }

  ## project spatial object
  x <- spTransform(x, zone)

  ## return projected spatial object
  x

}

## extract coordinates from a SpatialPolygons object from:
## https://stat.ethz.ch/pipermail/r-sig-geo/2010-June/008520.html
polycoords <- function(sp.poly) {
  coords <- NULL
  for(i in 1:length(sp.poly@polygons)) {
    pp <- sp.poly@polygons[[i]]@Polygons
    for (j in 1:length(pp))
      coords <- rbind(coords, coordinates(pp[[j]]))
  }
  coords
}
