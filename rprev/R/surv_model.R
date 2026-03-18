#' Predicts survival probability for given individuals at specific times.
#' 
#' This generic method is required for any survival object used in the main
#' \code{prevalence} function.
#' 
#' @param object The survival object itself
#' @param newdata Simulated incident individuals, with the same attributes specified in
#'   \code{extract_covars} and found in the supplied registry data to \code{prevalence}.
#' @param times The time at which to estimate the survival probability of the individual
#'   in the corresponding row of \code{newdata}. Must have as many times as \code{newdata}
#'   has rows.
#' @return A vector with the same length as \code{times} providing the survival probability
#'   estimates.
#' 
#' @export
predict_survival_probability <- function(object, newdata, times) UseMethod("predict_survival_probability")

#' Returns the name of the covariates in the registry data set that are required by
#' the survival model.
#' 
#' Used in \code{prevalence} to determine which covariates should be sampled when 
#' simulating an incident population. This should provide a character vector containing
#' column names of the original registry data set input to \code{prevalence} that
#' are used by the survival model.
#' 
#' @param object The survival object itself
#' @return A character vector holding the column names. Can be NULL if no 
#'   covariates are used i.e. the survival model is built based on population 
#'   level data rather than individual level data.
#'   
#' @export
extract_covars <- function(object) UseMethod("extract_covars")
