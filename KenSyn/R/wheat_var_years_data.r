#' @name wheat_var_years
#' @title Network of experiment to evaluate Wheat varieties on several years (2005-2010).
#' @description
#' The data set consists of measurements of wheat yields from a network of field experiments
#' designed to evaluate and compare performances of different varieties or varietal mixtures.
#' This extract consists of 113 different experiments corresponding to 54 different places and 6 years (2005 to 2010).
#' 80 varieties (v1 to v80) or varietal mixtures where compared.
#' TODO : CHECK The experimental designs used are complete random block designs  TODO : CHECK
#' The number of repetitions for a variety in each experiment varies from 2 to 6 (n_blocs).
#' @docType data
#' @usage wheat_var_itk
#' @format a \code{RangedData} instance, 1 row per measurement. 
#' rendement: Wheat Yield (ton/hectare), variete: variety, experimentation: experiment name, lieu: place, annee: year, Vmoyj:???
#' @source Arvalis - institut du vegetal, real data, but anonymized (variety).
#' @examples
#' summary(wheat_var_years)
NULL
