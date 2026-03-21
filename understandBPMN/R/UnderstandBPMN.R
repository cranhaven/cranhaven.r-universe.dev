#' @title understandBPMN - understandability metrics for BPMN models
#'
#' @description This package provides the implementation of several comprehensibility and complexity metrics for BPMN models
#'
#' @docType package
#' @name understandBPMN
#'
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import XML
#' @import tibble
#' @import devtools
#' @importFrom R.utils TimeoutException
#' @importFrom R.utils withTimeout
#' @importFrom Rcpp evalCpp
#' @importFrom utils combn
#' @importFrom usethis use_data
#' @exportPattern ("^[[:alpha:]]+")
#' @useDynLib understandBPMN, .registration=TRUE

globalVariables(c("."))
NULL
