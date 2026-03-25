#' @name wheat_var_soil
#' @title Network of experiment to evaluate Wheat varieties on two contrasted soils.
#' @description
#' The data set consists of measurements of wheat yields from a network of field experiments
#' designed to evaluate and compare performances of different varieties or varietal mixtures.
#' This extract consists of 10 different experiments where 6 varieties or varietal mixtures where compared.
#' TODO : CHECK The experimental designs used are complete random block designs  TODO : CHECK
#' The number of repetitions for a variety in each experiment varies from 3 to 4 (n_blocs).
#' @docType data
#' @usage wheat_var_soil
#' @format a \code{RangedData} instance, 1 row per measurement. 
#' variete: variety, experimentation:experiment, rendement:Wheat Yield (ton/hectare), MSE:, ddl: ,n_blocs:number of repetition       sol
#' @source Arvalis - institut du vegetal, real data, but anonymized (variety and experiment).
#' @examples
#' summary(wheat_var_soil)
NULL
