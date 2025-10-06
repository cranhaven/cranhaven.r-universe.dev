\name{tangles}
\docType{methods}
\alias{tangles}
\title{Anonymise spatial point patterns and raster objects}
\description{
Performs spatial anonymisation ("tangling") of coordinates through randomized transformation sequences, preserving relative spatial relationships while obscuring true locations. Three transformation types are used: X shift, Y shift, and rotation around a random origin. The sequence and parameters used for anonymisation are recorded and returned for later disentanglement.
}

\usage{
tangles(data = NULL, depth = 3, rasterdata = FALSE, raster_object = FALSE, 
saveTangles = FALSE, exportShapefile = FALSE, path = NULL)
}

\arguments{
  \item{data}{Either a two-column \code{matrix} or \code{data.frame} of coordinates, an \code{sf} POINT object, or a \code{terra::SpatRaster} object.}
  
  \item{depth}{Integer. Number of transformation steps to apply (default is 3).}
  
  \item{rasterdata}{Logical. If \code{TRUE}, rotations are limited to 90, 180, or 270 degrees to preserve axis alignment in raster data.}
  
  \item{raster_object}{Logical. Set \code{TRUE} if input is a \code{terra::SpatRaster} object. All raster values will be mapped to their transformed coordinates.}
  
  \item{saveTangles}{Logical. If \code{TRUE}, writes both the transformed data and detangler metadata to the specified \code{path}.}
  
  \item{exportShapefile}{Logical. If \code{TRUE} and input is a point-based dataset (not a raster), the anonymised coordinates are written to a shapefile. No CRS is assigned.}
  
  \item{path}{Character. Path to directory where outputs will be saved. Defaults \code{tempdir()}.}
}

\value{
A \code{list} with two elements:
\itemize{
  \item The transformed coordinates (if point input) or a \code{terra::SpatRaster} (if raster input)
  \item A \code{detangler} list containing the transformation log (\code{unpicker}) and a unique hash
}
If \code{saveTangles = TRUE}, the following files are written:
\itemize{
  \item \code{tangledXY_<hash>.rds} or \code{tangledXY_raster_<hash>.rds} — the transformed data
  \item \code{detangler_<hash>.rds} — the metadata required for reverse transformation
  \item Optional: shapefile output if \code{exportShapefile = TRUE}
}
}

\note{
For raster input, both \code{rasterdata = TRUE} and \code{raster_object = TRUE} are usually recommended. This ensures rotation steps align with raster grid expectations.

The detangler object is the critical output. It allows the same transformation sequence to be reversed or applied to related datasets.

Coordinate reference systems are intentionally ignored in this function. Anonymised outputs do not have spatial meaning, but retain topological properties of the input.

If writing shapefiles, no CRS is assigned and no \code{.prj} file is written.
}

\author{Brendan Malone}

\references{
  \itemize{
    \item CM O’Keefe, S Otorepec, M Elliot, E Mackey, and K O’Hara (2017) The De-Identification Decision Making Framework. CSIRO Reports EP173122 and EP175702. \doi{10.4225/08/59c169433efd4}
  }
}


\examples{
\donttest{
## Example 1: Using point data.frame
library(digest)
set.seed(1)
pts <- data.frame(X = runif(100), Y = runif(100))
res <- tangles(data = pts, depth = 4)
str(res)

## Example 2: Using sf object
library(sf)
sf_pts <- st_as_sf(pts, coords = c("X", "Y"))
res_sf <- tangles(data = sf_pts, depth = 3, exportShapefile = TRUE)

## Example 3: Using terra raster
library(terra)
ext_path <- system.file("extdata", package = "tangles")
rast.files <- list.files(path = ext_path, full.names = TRUE)
rasters <- terra::rast(rast.files)
res_r <- tangles(data = rasters, depth = 3, rasterdata = TRUE, raster_object = TRUE)
str(res_r)
}}

\keyword{spatial}
\keyword{Anonymisation}
\keyword{methods}
