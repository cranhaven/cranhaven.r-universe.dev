#' @title Re-weighting Point Processes Based on Additive Hazard Models
#' @docType package
#' @name ahw
#' @description Estimates continuous time weights for performing causal survival
#' analysis. For instance, weighted Nelson-Aalen or Kaplan-Meier
#' estimates can be given a causal interpretation.
#' @importFrom methods is
#' @importFrom data.table data.table as.data.table := .N
#' @importFrom timereg aalen predict.aalen
#' @importFrom graphics lines
#' @importFrom stats predict runif weights
#' @importFrom plyr .
NULL
