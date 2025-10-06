\name{detangles}
\docType{methods}
\alias{detangles}
\title{Revert anonymised point patterns or raster objects}
\description{
Reverses the spatial anonymisation applied by the \code{\link{tangles}} function, restoring original XY coordinates or raster data. Requires a matching detangler object containing the transformation sequence and hash key. The restoration process preserves spatial relationships and supports both point and raster inputs.
}

\usage{
detangles(
  data = NULL,
  tanglerInfo = NULL,
  raster_object = FALSE,
  stub = NULL,
  hash_key = NULL,
  saveTangles = FALSE,
  exportShapefile = FALSE,
  path = NULL
)
}

\arguments{
  \item{data}{A 2-column \code{matrix} or \code{data.frame} of spatial coordinates, an \code{sf} POINT object, or a \code{terra::SpatRaster} object.} 

  \item{tanglerInfo}{A detangler object returned by \code{tangles()}, containing the anonymisation steps and associated hash key.}

  \item{raster_object}{Logical; set to \code{TRUE} if the input is a raster object. This enables value restoration for all raster cells.}

  \item{stub}{Optional character string used in output file naming. Helps distinguish saved files.}

  \item{hash_key}{Character string representing the hash key to verify identity of the detangler. Must match \code{tanglerInfo\$hash}.}

  \item{saveTangles}{Logical; if \code{TRUE}, saves untangled data as \code{.rds} (and optionally raster \code{.tif}) files to \code{path}.}

  \item{exportShapefile}{Logical; if \code{TRUE} and input is point-based, a shapefile of the untangled coordinates is written (with undefined CRS).}

  \item{path}{Directory to save output files if \code{saveTangles} or \code{exportShapefile} is enabled. Defaults \code{tempdir()}.}
}

\value{
A restored \code{data.frame} of spatial XY coordinates or a \code{terra::SpatRaster} object, depending on input type.

If \code{saveTangles = TRUE}, corresponding files are saved to \code{path}, including:

\itemize{
  \item \code{detangledXY_<stub>_<hash>.rds}
  \item \code{detangledXY_raster_<name>.tif} (for raster input)
  \item \code{detangledXY_<stub>_<hash>.shp} (for point shapefile export, if \code{exportShapefile = TRUE})
}
}

\note{
If the \code{hash_key} provided does not match the one in the detangler object, the function stops to prevent incorrect spatial restoration.

When writing shapefiles, no CRS is assigned. This ensures that untangled data does not imply real-world georeferencing but retains relative spatial relationships.

Outputs can be safely saved and shared using the embedded hash key and transformation record.
}

\author{Brendan Malone}

\references{
  \itemize{
    \item CM O’Keefe, S Otorepec, M Elliot, E Mackey, and K O’Hara (2017) The De-Identification Decision Making Framework. CSIRO Reports EP173122 and EP175702. \doi{10.4225/08/59c169433efd4}
  }
}

\examples{
\donttest{
## EXAMPLE 1: Untangle point data.frame and export shapefile
library(sf)
set.seed(1)

# Simulate XY data
pts <- data.frame(X = runif(100), Y = runif(100))

# Anonymise
tangled <- tangles(data = pts, depth = 4, saveTangles = FALSE)

# Restore points
restored_pts <- detangles(
  data = tangled[[1]],
  tanglerInfo = tangled[[2]],
  raster_object = FALSE,
  stub = "points",
  hash_key = tangled[[2]]$hash,
  exportShapefile = TRUE
)

## EXAMPLE 2: Untangle from sf POINT input
sf_pts <- st_as_sf(pts, coords = c("X", "Y"))

# Anonymise sf object
tangled_sf <- tangles(data = sf_pts, depth = 3)

# Restore using sf input
restored_from_sf <- detangles(
  data = tangled_sf[[1]],
  tanglerInfo = tangled_sf[[2]],
  stub = "sf_restore",
  hash_key = tangled_sf[[2]]$hash,
  exportShapefile = TRUE
)

## EXAMPLE 3: Untangle raster data (terra)
library(terra)
ext_path <- system.file("extdata", package = "tangles")
rast.files <- list.files(path = ext_path, full.names = TRUE)
rasters <- terra::rast(rast.files)

# Anonymise raster
tangled_rast <- tangles(data = rasters, depth = 3, rasterdata = TRUE, raster_object = TRUE)

# Restore raster
restored_rast <- detangles(
  data = tangled_rast[[1]],
  tanglerInfo = tangled_rast[[2]],
  raster_object = TRUE,
  stub = "raster_demo",
  hash_key = tangled_rast[[2]]$hash,
  saveTangles = TRUE
)
}}
\keyword{methods}
\keyword{spatial}
\keyword{privacy}
