#' @name wheatyield
#' @title Wheat Yield at a regional scale (fake data).
#' @description
#' Wheat yield.
#' The data set consists of 45 measurements of wheat yields from 15 experimental sites (one year) in a fictive agricultural region.
#' In this dataset, a study represents a site-year with 2 to 4 blocks.
#' Each block corresponds to an experimental plot on which the yield of the crop (expressed in t.ha-1) was measured.
#' Thus, each study contains 2 to 4 measurements of yields
#' @docType data
#' @usage wheatyield
#' @format a \code{RangedData} instance, 1 row per measurement. Site : experimental site, Rdt : Wheat Yield (ton/hectare)
#' @source fake data
#' @examples
#' summary(wheatyield)
#' # NOT RUN example of analysis with mixted model on this dataset
#' # demo(package="KenSyn", "ch01_mixedmodel_nlme")
NULL
