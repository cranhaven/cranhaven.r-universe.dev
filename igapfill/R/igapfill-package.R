#' Interface for filling missing values of Earth Observation Datasets
#' 
#' By making extensive use of parallel computing, the functions of this package facilitate the application of the spatio-temporal
#' gap-filling method \code{\link[gapfill]{Gapfill}} to time series of satellite images (TSSI).
#' 
#' @details 
#' 
#' Only \emph{GeoTiff} files are allowed. In passing, some of the functions
#' of this package can be construed as independent builders of arguments used by \code{\link[gapfill]{Gapfill}} functions.
#' 
#' @name igapfill-package
#' @author Tecuapetla-Gómez, I. \email{itecuapetla@@conabio.gob.mx}
#' 
#' @section Datasets: 
#' 
#' Spatial subsets of the MOD13Q1 v061 NDVI product, in \emph{.tif} format, covering Cerro
#' de Garnica National Park (https://simec.conanp.gob.mx/ficha.php?anp=66&reg=11) located at 
#' Michoacan, Mexico. These subsets were collected from February 16, 2000 to December 16, 2024. 
#' The spatial and temporal resolutions of these images are 250m and 16 days, respectively. 
#' 
#' \tabular{ll}{
#'  \code{garnica_250m_16_days_NDVI.tif}\tab 572 layers of NDVI \cr
#'  \code{garnica_250m_16_days_pixel_reliability.tif}\tab Pixel reliability layers corresponding to\cr 
#'  \tab garnica_250m_16_days_NDVI.tif \cr
#' } 
#' 
#' @section Quality assessment summary: 
#' 
#' The following functions allow to compute the amount of missing values in a TSSI and 
#' to determine a sub-set of images to which apply the workflow of this package.
#' 
#' \tabular{ll}{
#'  \code{\link{mvSieve}}\tab Computes amount of missing values in a TSSI \cr
#'\code{\link{minmaxBlock}}\tab Determines sub-set of images with minimal (or maximal) missing values \cr
#' } 
#' 
#' @section Workflow:
#' 
#' The following functions allow to define the required directory/folders
#' structure employed by this package. Some of these functions are also useful
#' for better data handling.
#' 
#' \tabular{ll}{
#'   \code{\link{create_dirs}}\tab Sets up directory tree \cr
#' \code{\link{dimsReport}}\tab Summary of dimensions of images to process \cr
#' \code{\link{sort_split}}\tab Split large TSSI into smaller spatio-temporal chunks \cr
#' \code{\link{waysToSplit}}\tab Briefing of ways to divide rows and columns of images \cr
#' }
#' 
#' @section Interface:
#' 
#' These are the functions of this package that allow for filling missing values
#' of spatio-temporal subsets of satellite images using \code{\link[gapfill]{Gapfill}}.
#' 
#' \tabular{ll}{
#'   \code{\link{applyGapfill}}\tab Parallel computing-based application of \code{\link[gapfill]{Gapfill}} \cr
#'\code{\link{parallel_mosaic}}\tab Parallel rasterization and mosaicking (when required) of output of \code{\link[igapfill]{applyGapfill}} \cr
#'\code{\link{igapfill}}\tab Console-based wrap-up of \code{\link[igapfill]{applyGapfill}} and \code{\link[igapfill]{parallel_mosaic}} \cr
#' }
#' 
#' @section Miscellaneous:
#' 
#' These functions can be used to obtain some of the arguments required by \code{\link[igapfill]{applyGapfill}}.
#' In addition to this, these functions can also be employed to define some arguments of 
#' \code{\link[gapfill]{Gapfill}}.
#' 
#' \tabular{ll}{
#'   \code{\link{get_3Darray}}\tab Assambles 3D array \cr
#'   \code{\link{get_4Darray}}\tab Assambles 4D array \cr
#'   \code{\link{get_LAT}}\tab Gets latitude information of RasterStack \cr
#'   \code{\link{get_LON}}\tab Gets longitude information of RasterStack \cr
#' }
#' 
#' @references Gerber, F., de Jong, R., Schaepman, M.E., Schaepman-Strub, G., Furrer, R. (2018). 
#' \emph{Predicting missing values in spatio-temporal remote sensing data}, IEEE Transactions on Geoscience 
#' and Remote Sensing, 1--13.
#' 
#' @keywords package
"_PACKAGE"