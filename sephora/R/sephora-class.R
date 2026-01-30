#' @title class sephora
#' 
#' @description Definition of the \code{sephora} class 
#'
#' @slot x Original time series (as a numeric vector)
#' @slot startYear Beginning of time series
#' @slot endYear End of time series
#' @slot freq Number of observations per season
#' @slot sigma Variability estimate
#' @slot m_aug_smooth Samples of smoothed version of \code{x}, in matricial form
#' @slot clustering An object of class \code{\link[dtwclust]{HierarchicalTSClusters}}
#' @slot fpca Numeric, FPCA-based regression fit
#' @slot fpca_harmfit_params a list, harmonic fit
#' @slot fpca_fun_0der Function fpca fit
#' @slot fpca_fun_1der Function fpca fit first derivative
#' @slot fpca_fun_2der Function fpca fit second derivative
#' @slot fpca_fun_3der Function fpca fit third derivative
#' @slot fpca_fun_4der Function fpca fit fourth derivative
#' @slot phenoparams Phenological dates estimate
#' @slot status Character, was phenopar estimation successful?
#'
#' @seealso \code{\link[sephora]{sephora-methods}}
#'
setClass("sephora",
         representation(
           x="numeric",
           startYear="numeric",
           endYear="numeric",
           freq="numeric",
           sigma="numeric",
           m_aug_smooth="numeric",
           # clustering="HierarchicalTSClusters",
           fpca="numeric",
           fpca_harmfit_params="list",
           # fpca_fun_0der="function",
           # fpca_fun_1der="function",
           # fpca_fun_2der="function",
           # fpca_fun_3der="function",
           # fpca_fun_4der="function",
           phenoparams="numeric",
           status="character"
         ),
         prototype(
           x=0,
           startYear=2000,
           endYear=2021,
           freq=23,
           sigma=NULL,
           m_aug_smooth=0,
           # clustering=HierarchicalTSClusters(),
           fpca=0,
           fpca_harmfit_params=list(),
           # fpca_fun_0der=list(),
           # fpca_fun_1der=list(),
           # fpca_fun_2der=list(),
           # fpca_fun_3der=list(),
           # fpca_fun_4der=list(),
           phenoparams=0,
           status=""
         )
)