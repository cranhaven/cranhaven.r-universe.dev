#' Simulates an incident population according to a specific incidence model
#' 
#' This method defines the main behaviour of an incidence model and must be
#' implemented for any class to be used in \code{prevalence}.
#' 
#' @param object The incidence model.
#' @param data The original registry data frame passed into \code{prevalence}. It
#'   is supplied in this method so that individual level attributes can be
#'   sampled from the prior distribution.
#' @param timeframe The amount of time in days in which to simulate incidence
#'   for.
#' @param covars Any patient level covariates that must be included in the
#'   new simulated incident population, as a character vector. These will 
#'   correspond to columns in \code{data}.
#'   
#' @return A data frame where each row corresponds to a simulate incident
#'   patient. The first column must be incidence time (in days). This
#'   will be relative to an unspecified baseline. All covariates specified
#'   in \code{covars} must be present in this data frame.
#' 
#' @export
draw_incident_population <- function(object, data, timeframe, covars) UseMethod("draw_incident_population")
