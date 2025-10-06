\name{tangler}
\docType{methods}
\alias{tangler}
\title{Re-apply anonymisation using a stored detangler object}
\description{
For a given \code{detangler} object (from \code{\link{tangles}}), spatial coordinates or raster data can be re-anonymised using the same transformation sequence. This allows consistent tangling of related datasets using a fixed spatial masking.
}

\usage{
tangler(
  data = NULL,
  tanglerInfo = NULL,
  raster_object = FALSE,
  stub = NULL,
  saveTangles = FALSE,
  exportShapefile = FALSE,
  path = NULL
)
}

\arguments{
  \item{data}{A 2-column \code{matrix}, \code{data.frame} of coordinates, an \code{sf} POINT object, or a \code{terra::SpatRaster} object.}

  \item{tanglerInfo}{A detangler object returned by \code{\link{tangles}}, which encodes the anonymisation steps and hash key.}

  \item{raster_object}{Logical; set to \code{TRUE} if input is a raster object. This ensures the output is reconstructed as a \code{terra::SpatRaster}.}

  \item{stub}{Optional character string for file naming. Combined with the hash key for saved outputs.}

  \item{saveTangles}{Logical; if \code{TRUE}, the tangler output is saved as an \code{.rds} object, and raster layers as \code{.tif}.}

  \item{exportShapefile}{Logical; if \code{TRUE} and input is point-based, a shapefile of the tangled data is also exported (no CRS assigned).}

  \item{path}{Directory to write output files if saving is enabled. Defaults \code{tempdir()}.}
}

\value{
Returns either:
\itemize{
  \item A \code{data.frame} of spatial coordinates (for point-based data), or
  \item A \code{terra::SpatRaster} (for raster input)
}
If \code{saveTangles = TRUE}, corresponding outputs are written to \code{.rds}, and optionally \code{.tif} (for raster) or shapefile (for point).
}

\note{
This function re-applies a saved transformation sequence. It does not generate new spatial shifts.

When working with raster data, only 90°, 180°, or 270° rotations are supported. If the stored detangler contains unsupported rotation angles, the function will stop to avoid corrupting the raster structure.

Shapefiles are written without CRS metadata to preserve anonymity. Coordinates retain spatial structure but not geolocation accuracy.
}

\author{Brendan Malone}

\references{
  \itemize{
    \item CM O’Keefe, S Otorepec, M Elliot, E Mackey, and K O’Hara (2017) The De-Identification Decision Making Framework. CSIRO Reports EP173122 and EP175702. \doi{10.4225/08/59c169433efd4}
  }
}

\examples{
\donttest{
## Example 1: Tangling a point data.frame
library(digest)
set.seed(123)
pts <- data.frame(X = runif(100), Y = runif(100))

# Anonymise original points
t1 <- tangles(data = pts, depth = 4)

# Tangling a second dataset using the same detangler
tangled_again <- tangler(
  data = pts,
  tanglerInfo = t1[[2]],
  stub = "pt_demo",
  saveTangles = TRUE,
  exportShapefile = TRUE
)

## Example 2: Tangling an sf POINT object
library(sf)
sf_pts <- st_as_sf(pts, coords = c("X", "Y"))

tangled_sf <- tangler(
  data = sf_pts,
  tanglerInfo = t1[[2]],
  stub = "sf_demo",
  saveTangles = TRUE,
  exportShapefile = TRUE
)

## Example 3: Tangling a raster using stored detangler
library(terra)
ext_path <- system.file("extdata", package = "tangles")
rast.files <- list.files(path = ext_path, full.names = TRUE)
rasters <- terra::rast(rast.files)

# Must use a detangler with 90°/180°/270° rotations for raster compatibility
t2 <- tangles(data = rasters, depth = 3, rasterdata = TRUE, raster_object = TRUE)

tangled_rast <- tangler(
  data = rasters,
  tanglerInfo = t2[[2]],
  raster_object = TRUE,
  stub = "r_demo",
  saveTangles = TRUE
)
}}

\keyword{methods}
\keyword{spatial}
\keyword{privacy}
