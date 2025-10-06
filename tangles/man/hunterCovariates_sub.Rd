\name{hunterCovariates_sub}
\docType{data}
\alias{hunterCovariates_sub}
\title{Environmental Covariate Rasters for a Subset of the Lower Hunter Valley, NSW}
\description{
A suite of GeoTIFF rasters representing environmental covariates for a subset of the Lower Hunter Valley in New South Wales, Australia. These covariates are used in digital soil mapping and terrain analysis.
}
\format{
The dataset consists of multiple raster layers (GeoTIFF format) located in the \code{inst/extdata/} directory of the package, each with a spatial resolution of 25 m × 25 m and a CRS of WGS 84 UTM Zone 56.

Available files are prefixed with \code{hunterCovariates_sub_} and include:

\describe{
  \item{\code{hunterCovariates_sub_Terrain_Ruggedness_Index.tif}}{Topographic ruggedness index (TRI).}
  \item{\code{hunterCovariates_sub_AACN.tif}}{Elevation above channel network base level.}
  \item{\code{hunterCovariates_sub_Landsat_Band1.tif}}{Landsat 7 ETM+ Band 1 reflectance (0.45–0.52 µm).}
  \item{\code{hunterCovariates_sub_Elevation.tif}}{Elevation in meters above sea level, derived from a DEM.}
  \item{\code{hunterCovariates_sub_Hillshading.tif}}{Hillshade raster generated from the DEM using a fixed sun angle.}
  \item{\code{hunterCovariates_sub_Light_insolation.tif}}{Potential solar radiation calculated over a calendar year at 5-day intervals.}
  \item{\code{hunterCovariates_sub_Mid_Slope_Position.tif}}{Slope position classification for crest/valley context.}
  \item{\code{hunterCovariates_sub_MRVBF.tif}}{Multi-resolution valley bottom flatness index.}
  \item{\code{hunterCovariates_sub_NDVI.tif}}{Normalized Difference Vegetation Index based on Landsat 7.}
  \item{\code{hunterCovariates_sub_TWI.tif}}{Topographic Wetness Index (TWI).}
  \item{\code{hunterCovariates_sub_Slope.tif}}{Slope angle in degrees.}
}
}
\details{
The subset area corresponds to the Hunter Wine Country Private Irrigation District (HWCPID) in the Lower Hunter Valley (approx. 32.83°S, 151.35°E), located ~140 km north of Sydney. The HWCPID covers around 220 km² and supports viticulture and dryland grazing under a temperate, humid climate with ~750 mm annual rainfall.

These covariates are used in spatial prediction tasks, terrain classification, and training examples in digital soil mapping.
}

\references{
  \itemize{
    \item Gallant, J.C., Dowling, T.I. (2003). A multiresolution index of valley bottom flatness for mapping depositional areas. \emph{Water Resources Research}, 39(12), 1347. \doi{10.1029/2002WR001426}
    \item Malone, B.P., Hughes, P., McBratney, A.B., Minasny, B. (2014). A model for the identification of terrons in the Lower Hunter Valley, Australia. \emph{Geoderma Regional}, 1, 31–47. \doi{10.1016/j.geodrs.2014.08.001}
  }
}


\examples{
library(terra)

# Load and plot the elevation raster
elevation <- rast(system.file("extdata/hunterCovariates_sub_Elevation.tif", package = "tangles"))
plot(elevation, main = "Hunter Valley Subset - Elevation")
}
\keyword{datasets}
