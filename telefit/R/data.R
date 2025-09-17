#' Standardized anomalies of CO Precipitation
#'
#' A dataset containing sample spatially-aggregated climate data from the 
#' ERA-Interim and PRISM datasets.  The response comes from PRISM, average 
#' monthly precipitation in a DJF winter.  The covariates come from ERA-Interim, 
#' Colorado and Pacific Ocean (sea) surface temperatures.  All data has been
#' converted to standardized anomalies.
#'
#' @format A stData object with 3 years of observations
#' \describe{
#'   \item{tLabs}{year labels for data columns}
#'   \item{coords.s}{centers of grid cells for Colorado data}
#'   \item{coords.r}{centers of grid cells for Pacific Ocean data}
#'   \item{X}{Array of design matrices for Colorado covariates}
#'   \item{Y}{Matrix of precipitation observations}
#'   \item{Z}{Matrix of Pacific Ocean data}
#'   \item{X.lab}{Label for covariate data, used by plotting functions}
#'   \item{Y.lab}{Label for response data, used by plotting functions}
#'   \item{Z.lab}{Label for covariate data, used by plotting functions}
#' }
#' 
#' @source \url{http://prism.oregonstate.edu}
#' @source \url{https://rda.ucar.edu/datasets/ds627.0/}
#' 
#' @examples
#' 
#' data("coprecip")
#' str(coprecip)
#' 
"coprecip"

#' Sample MCMC output for the RESP model
#'
#' An example stFit object containing output from a short run of the MCMC 
#' sampler that fits the RESP model to data.
#' 
#'
#' @format An stFit object, which is a list of several objects
#' \describe{
#'   \item{parameters}{MCMC samples of model parameters}
#'   \item{priors}{description of priors used to fit model}
#'   \item{miles}{TRUE or FALSE to specify whether the spatial distances 
#'       used to estimate spatial covariance parameters were in units of miles 
#'       (TRUE) or kilometers (FALSE)}
#'   \item{localOnly}{TRUE if remote covariates were not estimated}
#'   \item{remoteOnly}{TRUE if local covariates were not estimated}
#'   \item{varying}{(deprecated) TRUE if local covariates were estimated as a 
#'       spatially-varying field}
#'   \item{coords.knots}{coordinates of remote knot locations}
#' }
#' 
#' @examples
#' 
#' data("coprecip.fit")
#' str(coprecip.fit)
#' 
"coprecip.fit"

#' Sample composition sampling output for the RESP model
#'
#' An example stPredict object containing predictions from a short run of the 
#' MCMC composition sampler.  The output also contains teleconnection estimates.
#' 
#'
#' @format An stPredict object, which is a list of several objects
#' \describe{
#'   \item{pred}{A list containing summaries of posterior predictions}
#'   \item{samples}{Posterior samples for predictions}
#'   \item{coords.s}{centers of grid cells for Colorado data}
#'   \item{localOnly}{TRUE if remote covariates were not estimated}
#'   \item{varying}{(deprecated) TRUE if local covariates were estimated as a 
#'       spatially-varying field}
#'   \item{tLabs}{year labels for prediction timepoints}
#'   \item{Y.lab}{Label for response data, used by plotting functions}
#'   \item{cat.probs}{vector of probabilities for using posterior samples to 
#'     return categorical predictions from the posterior prediction samples}
#'   \item{category.breaks}{Breakpoints used to discretize posterior predictive
#'     distribution at each coordinate in coords.s during composition sampling.}
#'   \item{alpha_knots}{Summaries of posterior estimates of teleconnection 
#'     effects}
#'   \item{eof_alpha_knots}{Summaries of posterior estimates of teleconnection 
#'     effects after spatial basis function transformation}
#' }
#' 
#' @examples 
#' 
#' data("coprecip.predict")
#' str(coprecip.predict)
#' 
"coprecip.predict"