#' Results of different GWAS.
#'
#' A dataset containing the results of 10 different genetic association studies testing the association between a set of 25,436 markers and the grain yield.
#' The data are extrated from: Drops Amaizing available on the https://doi.org/10.15454/6TL2N4 website.
#
#' This dataset were obtained thanks to the *metaGE.collect* function.
#'
#' @format A data frame with 25,436 rows and 35 variables:
#'
#' * CHR: chromosome of the marker
#' * POS: position of the marker
#' * MARKER: name of the marker
#' * FREQ.env: maf of the marker in the environment env
#' * EFFECT.env: regression coefficient of the marker in the environment env
#' * EFFECT_SE.env: standard error of the regression coefficient of the marker in the environment env
#' * PVAL.env: pvalue of the marker in the environment env
#' * WEIGHT.env: weight of the marker in the environment env
#' * ALLELE0: allele0
#' * ALLELE1: allele1
#'
"metaData"


#' Description of the environments.
#'
#' A dataset containing variables describing the 22 environments.
#'
#'
#' @format A data frame with 22 rows and 3 variables:
#'
#' * FileName: environment name
#' * Temp: temperature
#' * Water: water condition
"envDesc"



