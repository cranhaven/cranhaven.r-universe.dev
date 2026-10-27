#' timesift: Temporal Climate Resolution for Ecological Prediction
#'
#' Builds model-ready representations of climate records at a chosen temporal grain and
#' locates the grain at which predictive skill saturates. The representation layer is
#' response-agnostic; the fitting and scoring layers default to presence-absence with a joint
#' multi-label head scored by the true skill statistic.
#'
#' @keywords internal
#' @useDynLib timesift, .registration = TRUE
"_PACKAGE"
