# profile — aboveR
# Terrain profiling along transects and boundaries

#' Extract a Terrain Profile Along a Line
#'
#' Samples elevation values from a DEM at regular intervals along a
#' transect line and returns a data frame of distance vs. elevation.
#'
#' @param dem A [terra::SpatRaster] representing the terrain surface.
#' @param line An [sf] object containing a single LINESTRING geometry
#'   defining the transect. Or a path to a GeoPackage/shapefile.
#' @param spacing Numeric. Distance between sample points along the
#'   line, in the CRS units of `dem`. Default `NULL` uses 1 cell width.
#'
#' @returns A data frame with columns:
#'   - `distance`: distance along the profile from the start point
#'   - `elevation`: sampled elevation value
#'   - `x`, `y`: coordinates of each sample point
#'
#' @export
#'
#' @examples
#' dem <- terra::rast(system.file("extdata/dem_before.tif", package = "aboveR"))
#' line <- sf::st_read(
#'   system.file("extdata/profile_line.gpkg", package = "aboveR"),
#'   quiet = TRUE
#' )
#' prof <- terrain_profile(dem, line)
#' plot(prof$distance, prof$elevation, type = "l",
#'      xlab = "Distance", ylab = "Elevation")
terrain_profile <- function(dem, line, spacing = NULL) {
  validate_raster(dem, "dem")

  if (is.character(line)) {
    line <- sf::st_read(line, quiet = TRUE)
  }
  validate_sf(line, "line")

  # Reproject line to DEM CRS if needed
  if (!identical(sf::st_crs(line)$wkt, terra::crs(dem))) {
    line <- sf::st_transform(line, terra::crs(dem))
  }

  geom <- sf::st_geometry(line)
  # Union to single line if multiple features
  geom <- sf::st_union(geom)
  geom <- sf::st_cast(geom, "LINESTRING")

  if (is.null(spacing)) {
    spacing <- terra::res(dem)[1]
  }

  total_length <- as.numeric(sf::st_length(geom))
  n_pts <- max(2L, floor(total_length / spacing) + 1L)
  fracs <- seq(0, 1, length.out = n_pts)

  pts <- sf::st_line_sample(geom, sample = fracs)
  pts <- sf::st_cast(pts, "POINT")

  coords <- sf::st_coordinates(pts)
  extracted <- terra::extract(dem, coords)
  elevs <- extracted[[names(dem)[1]]]

  data.frame(
    distance  = fracs * total_length,
    elevation = elevs,
    x         = coords[, 1],
    y         = coords[, 2]
  )
}

#' Extract Terrain Profile Along a Polygon Boundary
#'
#' Converts a polygon boundary to a closed linestring and samples
#' elevation values around the perimeter. Useful for inspecting
#' highwall edges, dam crests, or property boundaries.
#'
#' @param dem A [terra::SpatRaster] representing the terrain surface.
#' @param boundary An [sf] polygon whose boundary (exterior ring) will
#'   be profiled.
#' @param spacing Numeric. Distance between sample points, in CRS units
#'   of `dem`. Default `NULL` uses 1 cell width.
#'
#' @returns A data frame with columns:
#'   - `distance`: distance along the boundary perimeter
#'   - `elevation`: sampled elevation value
#'   - `x`, `y`: coordinates of each sample point
#'
#' @export
#'
#' @examples
#' dem <- terra::rast(system.file("extdata/dem_before.tif", package = "aboveR"))
#' boundary <- sf::st_read(
#'   system.file("extdata/boundary.gpkg", package = "aboveR"),
#'   quiet = TRUE
#' )
#' bprof <- boundary_terrain_profile(dem, boundary)
#' plot(bprof$distance, bprof$elevation, type = "l",
#'      xlab = "Perimeter Distance", ylab = "Elevation")
boundary_terrain_profile <- function(dem, boundary, spacing = NULL) {
  validate_raster(dem, "dem")
  validate_sf(boundary, "boundary", "POLYGON")

  # Cast polygon to linestring (exterior ring)
  ring <- sf::st_cast(sf::st_geometry(boundary), "LINESTRING")
  ring_sf <- sf::st_sf(geometry = ring)

  terrain_profile(dem, ring_sf, spacing = spacing)
}
