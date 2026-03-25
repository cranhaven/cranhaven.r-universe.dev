#' @name wheat_var
#' @title Network of experiment to evaluate Wheat varieties on one single year.
#' @description
#' The data set consists of measurements of wheat yields from a network of field experiments
#' designed to evaluate and compare performances of 10 different varieties (v1 to v10).
#' It contains 5 different experiments (experiment 1 to 5) corresponding to five different places.The choice of places
#' was made so that they are representative of the variability observed in the study area.
#' All the experiments took place in the same year.
#' The experimental designs used are complete random block designs.
#' The number of repetitions in each experiment is equal to three (block 1 to 3).
#' @docType data
#' @usage wheat_var
#' @format a \code{RangedData} instance, 1 row per measurement. experimentation : experiment name, variete : variety, bloc : block of the design, rendement : Wheat Yield (ton/hectare)
#' @source Arvalis - institut du vegetal, real data, but anonymized (place and variety)
#' @examples
#' summary(wheat_var)
NULL
