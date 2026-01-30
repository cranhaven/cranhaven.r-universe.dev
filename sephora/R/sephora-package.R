#' Statistical Estimation of Phenological Parameters
#' 
#' Estimates phenological dates of satellite imagery time series. Originally
#' conceived to handle \href{https://modis.gsfc.nasa.gov/}{MODIS} time series (especifically
#' \href{https://lpdaac.usgs.gov/products/mod13q1v006/}{MOD13Q1}), this package can handle 
#' Earth Observation time series from any satellite mission.
#' 
#' @details 
#' 
#' The main function of this package, \code{\link[sephora]{phenopar}}, allows a \code{numeric} 
#' vector containing satellite-based measurements (preferably, vegetation indices for better results). 
#' These observations can be construed as realizations of an underlying periodic stochastic process that has been recorded 
#' from the first day of the year (DoY) of \code{startYear} to the last DoY of \code{endYear}. 
#' Thus, each numeric vector can be assembled as a matrix whose number of rows and columns equal
#' to \code{length(startYear:endYear)} and \code{frequency}, respectively, see \code{\link[sephora]{get_metadata_years}}. Moreover,
#' each row of this matrix can be thought as the realization of the periodic stochastic
#' process throughout a season. Thus, having multiple measurements of such a process, functional 
#' principal component methods are employed to extract an underlying idealized (vegetation index) curve. 
#' 
#' The phenological dates that can be estimated with sephora are:
#' 
#' \itemize{
#'     \item \bold{Green Up (GU)}.
#'     
#'     \item \bold{Start of Season (SoS)}.
#'     
#'     \item \bold{Maturity (Mat)}.
#'     
#'     \item \bold{Senescence (Sen)}.
#'     
#'     \item \bold{End of Season (EoS)}.
#'     
#'     \item \bold{Dormancy (Dor)}.
#' } 
#' 
#' @name sephora-package 
#' @author Tecuapetla-Gómez, I. \email{itecuapetla@@conabio.gob.mx}
#' 
#' @section Data handling:
#' The following functions allow to access numeric vectors of time series satellite 
#' imagery, in particular, MOD13Q1 time series starting at February 18, 2000. 
#' 
#' \tabular{ll}{
#'   \code{\link{fill_initialgap_MOD13Q1}}\tab Fill first 3 MOD13Q1 observations \cr
#'   \code{\link{vecFromData}}\tab Get numeric vector from an RData file \cr
#'   \code{\link{vecToMatrix}}\tab Set numeric vector as a matrix \cr
#'   \code{\link{get_metadata_years}}\tab Get metadata useful in certain visualizations \cr
#' }
#' 
#' @section Modeling:
#' The following functions allow to smooth out and fit a regression model based on 
#' Functional Principal Components. Applications of these functions allow to estimate 
#' phenological parameters of numeric vectors of Earth Observation time series:
#' 
#' \tabular{ll}{
#'   \code{\link{ndvi_derivatives}}\tab Derivatives of idealized NDVI curve \cr
#'   \code{\link{phenopar}}\tab Estimate phenological dates \cr
#'   \code{\link{phenopar_polygon}}\tab Estimate phenological dates (parallel processing) \cr
#' }
#' 
#' @section Plotting:
#' Plot methods for numeric and \code{sephora} objects:
#' 
#' \tabular{ll}{
#'   \code{\link{getSpiralPlot}}\tab Spiral plot of polygon-based phenological date estimates \cr
#'   \code{\link{plot.sephora}}\tab Plot methods for \code{\link[sephora]{sephora-class}} object \cr
#' }
#' 
#' @section Miscellaneous:
#' 
#' \tabular{ll}{
#'   \code{\link{datesToDoY}}\tab Maps estimated phenological dates to days of a year \cr
#'   \code{\link{getDist_phenoParam}}\tab Access to vectors of phenological date estimates from a list \cr
#'   \code{\link{global_min_max}}\tab Global critical points of a curve on a closed interval \cr
#'   \code{\link{local_min_max}}\tab Local critical points of a curve on a union of open intervals \cr
#' } 
#' 
#' @keywords package
#' @docType package
NULL