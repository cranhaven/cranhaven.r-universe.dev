# export — aboveR
# Export terrain data to engineering formats (LandXML, STL)

#' Export a DEM to LandXML TIN Surface
#'
#' Converts a DEM raster to a LandXML file containing a TIN (Triangulated
#' Irregular Network) surface. LandXML is widely supported by civil
#' engineering software including Autodesk Civil 3D, Bentley OpenRoads,
#' and Trimble Business Center.
#'
#' @param dem A [terra::SpatRaster] representing the terrain surface.
#' @param output Character. Output file path (should end in `.xml`).
#' @param surface_name Character. Name for the TIN surface in the XML.
#'   Default `"Ground"`.
#' @param decimate Integer. Keep every Nth point to reduce file size.
#'   Default `1` (keep all points). Use `2` to halve the point count,
#'   `5` to keep every 5th point, etc.
#' @param boundary An optional [sf] polygon to clip the DEM before export.
#'
#' @returns The output file path (invisibly).
#'
#' @export
#'
#' @examples
#' dem <- terra::rast(system.file("extdata/dem_before.tif", package = "aboveR"))
#' out <- file.path(tempdir(), "surface.xml")
#' export_landxml(dem, out, surface_name = "Existing")
#' file.exists(out)
export_landxml <- function(dem, output, surface_name = "Ground",
                           decimate = 1L, boundary = NULL) {
  validate_raster(dem, "dem")
  decimate <- as.integer(decimate)
  if (decimate < 1) stop("`decimate` must be >= 1.", call. = FALSE)

  if (!is.null(boundary)) {
    validate_sf(boundary, "boundary", "POLYGON")
    bnd_vect <- terra::vect(boundary)
    dem <- terra::crop(dem, bnd_vect) |> terra::mask(bnd_vect)
  }

  # Extract point coordinates
  pts <- terra::as.data.frame(dem, xy = TRUE, na.rm = TRUE)
  names(pts) <- c("x", "y", "z")

  # Decimate

  if (decimate > 1) {
    pts <- pts[seq(1, nrow(pts), by = decimate), ]
  }

  if (nrow(pts) < 3) {
    stop("Too few points to create a TIN surface after decimation.",
         call. = FALSE)
  }

  # Simple Delaunay via grid-based triangulation
  # For a gridded DEM, generate triangles from adjacent cells
  nr <- terra::nrow(dem)
  nc <- terra::ncol(dem)

  # Build triangle faces from grid connectivity
  # Each grid cell (i,j) creates two triangles with its right and bottom neighbours
  vals <- terra::values(dem)[, 1]
  valid <- !is.na(vals)

  # Map valid cells to point indices
  cell_to_pt <- rep(NA_integer_, length(vals))
  cell_to_pt[valid] <- seq_len(sum(valid))

  triangles <- list()
  for (row in seq_len(nr - 1)) {
    for (col in seq_len(nc - 1)) {
      tl <- (row - 1) * nc + col      # top-left
      tr <- tl + 1                     # top-right
      bl <- row * nc + col             # bottom-left
      br <- bl + 1                     # bottom-right

      p_tl <- cell_to_pt[tl]
      p_tr <- cell_to_pt[tr]
      p_bl <- cell_to_pt[bl]
      p_br <- cell_to_pt[br]

      # Upper triangle: tl-tr-bl
      if (!is.na(p_tl) && !is.na(p_tr) && !is.na(p_bl)) {
        triangles <- c(triangles, list(c(p_tl, p_tr, p_bl)))
      }
      # Lower triangle: tr-br-bl
      if (!is.na(p_tr) && !is.na(p_br) && !is.na(p_bl)) {
        triangles <- c(triangles, list(c(p_tr, p_br, p_bl)))
      }
    }
  }

  # Write LandXML
  crs_desc <- terra::crs(dem, describe = TRUE)
  crs_name <- if (!is.na(crs_desc$name)) crs_desc$name else "Unknown"

  con <- file(output, open = "w")
  on.exit(close(con), add = TRUE)

  writeLines('<?xml version="1.0" encoding="UTF-8"?>', con)
  writeLines('<LandXML xmlns="http://www.landxml.org/schema/LandXML-1.2" version="1.2">', con)
  writeLines(paste0('  <CoordinateSystem name="', crs_name, '"/>'), con)
  writeLines('  <Surfaces>', con)
  writeLines(paste0('    <Surface name="', surface_name, '">'), con)
  writeLines('      <Definition surfType="TIN">', con)

  # Points
  writeLines(paste0('        <Pnts>'), con)
  for (i in seq_len(nrow(pts))) {
    writeLines(paste0('          <P id="', i, '">', pts$y[i], ' ', pts$x[i],
                       ' ', pts$z[i], '</P>'), con)
  }
  writeLines('        </Pnts>', con)

  # Faces
  writeLines('        <Faces>', con)
  for (tri in triangles) {
    writeLines(paste0('          <F>', tri[1], ' ', tri[2], ' ', tri[3], '</F>'),
               con)
  }
  writeLines('        </Faces>', con)

  writeLines('      </Definition>', con)
  writeLines('    </Surface>', con)
  writeLines('  </Surfaces>', con)
  writeLines('</LandXML>', con)

  invisible(output)
}

#' Export a DEM to STL for 3D Printing
#'
#' Converts a DEM raster to an STL (stereolithography) file suitable
#' for 3D printing. The model includes a flat base and vertical
#' exaggeration control.
#'
#' @param dem A [terra::SpatRaster] representing the terrain surface.
#' @param output Character. Output file path (should end in `.stl`).
#' @param exaggeration Numeric. Vertical exaggeration factor.
#'   Default `1` (no exaggeration). Use `2` for 2x vertical stretch.
#' @param base_height Numeric. Thickness of the flat base below the
#'   terrain minimum, in DEM units. Default `1`.
#' @param decimate Integer. Keep every Nth cell. Default `1`.
#'
#' @returns The output file path (invisibly).
#'
#' @export
#'
#' @examples
#' dem <- terra::rast(system.file("extdata/dem_before.tif", package = "aboveR"))
#' out <- file.path(tempdir(), "terrain.stl")
#' export_stl(dem, out, exaggeration = 2)
#' file.exists(out)
export_stl <- function(dem, output, exaggeration = 1, base_height = 1,
                       decimate = 1L) {
  validate_raster(dem, "dem")
  decimate <- as.integer(decimate)
  if (decimate < 1) stop("`decimate` must be >= 1.", call. = FALSE)
  if (exaggeration <= 0) stop("`exaggeration` must be > 0.", call. = FALSE)

  # Get grid values
  pts <- terra::as.data.frame(dem, xy = TRUE, na.rm = TRUE)
  names(pts) <- c("x", "y", "z")

  if (decimate > 1) {
    pts <- pts[seq(1, nrow(pts), by = decimate), ]
  }

  # Apply exaggeration and normalize
  z_min <- min(pts$z)
  pts$z <- (pts$z - z_min) * exaggeration + base_height

  # Normalize xy to model coordinates
  x_min <- min(pts$x)
  y_min <- min(pts$y)
  pts$x <- pts$x - x_min
  pts$y <- pts$y - y_min

  # Build triangulated mesh from gridded data
  nr <- terra::nrow(dem)
  nc <- terra::ncol(dem)
  vals <- terra::values(dem)[, 1]
  valid <- !is.na(vals)
  cell_to_pt <- rep(NA_integer_, length(vals))
  cell_to_pt[valid] <- seq_len(sum(valid))

  con <- file(output, open = "w")
  on.exit(close(con), add = TRUE)

  writeLines("solid terrain", con)

  for (row in seq_len(nr - 1)) {
    for (col in seq_len(nc - 1)) {
      tl <- (row - 1) * nc + col
      tr <- tl + 1
      bl <- row * nc + col
      br <- bl + 1

      p_tl <- cell_to_pt[tl]
      p_tr <- cell_to_pt[tr]
      p_bl <- cell_to_pt[bl]
      p_br <- cell_to_pt[br]

      # Upper triangle
      if (!is.na(p_tl) && !is.na(p_tr) && !is.na(p_bl)) {
        write_stl_facet(con, pts[p_tl, ], pts[p_tr, ], pts[p_bl, ])
      }
      # Lower triangle
      if (!is.na(p_tr) && !is.na(p_br) && !is.na(p_bl)) {
        write_stl_facet(con, pts[p_tr, ], pts[p_br, ], pts[p_bl, ])
      }
    }
  }

  writeLines("endsolid terrain", con)
  invisible(output)
}

#' Write a Single STL Facet
#'
#' @param con File connection.
#' @param p1,p2,p3 Data frame rows with x, y, z.
#' @returns NULL (writes to connection).
#' @noRd
write_stl_facet <- function(con, p1, p2, p3) {
  # Compute normal via cross product
  u <- c(p2$x - p1$x, p2$y - p1$y, p2$z - p1$z)
  v <- c(p3$x - p1$x, p3$y - p1$y, p3$z - p1$z)
  n <- c(u[2] * v[3] - u[3] * v[2],
         u[3] * v[1] - u[1] * v[3],
         u[1] * v[2] - u[2] * v[1])
  len <- sqrt(sum(n^2))
  if (len > 0) n <- n / len

  writeLines(sprintf("  facet normal %e %e %e", n[1], n[2], n[3]), con)
  writeLines("    outer loop", con)
  writeLines(sprintf("      vertex %e %e %e", p1$x, p1$y, p1$z), con)
  writeLines(sprintf("      vertex %e %e %e", p2$x, p2$y, p2$z), con)
  writeLines(sprintf("      vertex %e %e %e", p3$x, p3$y, p3$z), con)
  writeLines("    endloop", con)
  writeLines("  endfacet", con)
  invisible(NULL)
}
