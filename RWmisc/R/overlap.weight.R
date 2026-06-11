#' Weight Raster Cells by Overlapping Polygons
#'
#' Weight raster cells by overlapping polygons to avoid over-counting when aggregating by polygons
#'
#' @param raster a RasterLayer object.
#' @param polygons a SpatialPolygons, SpatialPolygonsDataFrame, or simple feature
#' collection with at least two features. The function will still work with only
#' one polygon, but values will be unchanged, and the result will be equivalent
#' to `mask(raster, polygons)`.
#' @param count a logical indicating whether to return a raster with the count of
#' polygons intersecting each cell, or a raster with original values weighted by
#' 1/number of intersecting polygons.
#' @param warn include warnings? Most common is that the returned raster will be
#' an intersection of the raster and the polygons. Default `TRUE`.
#'
#' @details  This function takes a raster and a set of polygons as arguments.
#' It counts the number of polygons that intersect each raster cell. It can
#' return either a raster with the count of the number of intersecting polygons
#' as cell values or the original raster with cell values weighted by 1 / the
#' number of intersecting polygons (the default behavior). Cells that do not
#' intersect any polygons will receive a value of `NA`. If the extent of the
#' polygons is less than the extent of the raster, then the function will warn
#' that it is cropping the raster to the polygons' extent.
#'
#' @return a RasterLayer object.
#' @export
#'
#' @import raster
#' @import sp
#' @import sf
#'
#' @examples
#' library(sf)
#' library(raster)
#' polys_t <- st_sfc(list(st_polygon(list(rbind(c(2,2), c(2,6),
#'                                              c(6,6), c(6,2),
#'                                              c(2, 2)))),
#'                        st_polygon(list(rbind(c(8,8), c(4,8),
#'                                              c(4,4), c(8,4),
#'                                              c(8,8))))),
#'                   crs = st_crs('OGC:CRS84'))
#' raster_t <- raster(nrows = 10, ncols = 10, xmn = 0,
#'                    xmx = 10, ymn = 0, ymx = 10,
#'                    vals = 1:100,
#'                    crs = CRS(st_crs(polys_t)$proj4string))
#' overlap.weight(raster_t, polys_t)

overlap.weight <- function(raster, polygons, count = FALSE, warn = TRUE) {

  polygons <- poly.conv(polygons, raster)

  ## create list for raster from each polygon
  rasters <- list()

  ## loop through polygons
  for (i in 1:length(polygons)) {

    ## get polygon j
    polygon <- polygons[i, ]

    ## crop raster to polygon
    raster_poly <- crop(raster, polygon)

    ## set all cell values to 1 to represent polygon presence
    raster_poly[] <- 1

    ## mask out cells outside polygon
    raster_poly <- mask(raster_poly, polygon)

    ## append to list
    rasters[[i]] <- raster_poly

  }

  ## set function to mosaic rasters
  rasters$fun <- sum

  ## mosaic rasters, summing overlapping cells
  raster_poly_count <- do.call(mosaic, rasters)

  ## return raster of overlapping counts
  if (count) return(raster_poly_count)

  ## divide by polygon count
  raster_poly_count <- 1 / raster_poly_count

  ## return the weighted original raster
  if (warn) {
    return(raster * raster_poly_count)
  } else {
    suppressWarnings(return(raster * raster_poly_count))
  }

}

poly.conv <- function(x, y) {
  UseMethod('poly.conv', x)
}

poly.conv.SpatialPolygons <- poly.conv.SpatialPolygonsDataFrame <- function(x, y) {
  if (identicalCRS(x, y)) {
    return(x)
  } else {
    stop('raster and polygons do not share CRS')
  }
}

poly.conv.sfc_POLYGON <- poly.conv.sfc_MULTIPOLYGON <- function(x, y) {
  if (st_crs(x) == st_crs(y)) {
    return(as_Spatial(x))
  } else {
    stop('raster and polygons do not share CRS')
  }
}

poly.conv.sf <- function(x, y) {
  if (st_crs(x) == st_crs(y)) {
    if (inherits(st_geometry(x), c('sfc_POLYGON', 'sfc_MULTIPOLYGON'))) {
      return(as_Spatial(x))
    } else {
      stop('sfc_POLYGON or sfc_MULTIPOLYGON geometries required')
    }
  } else {
    stop('raster and polygons do not share CRS')
  }

}
