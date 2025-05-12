#' Dynamic prediction in functional concurrent regression with sparse functional covariates
#'
#' @docType package
#' @name fcr-package
#' @aliases dynfcr
#'
#' @importFrom fields image.plot
#'
#' @import mgcv
#' @import face
#'
#' @description
#'
#' This package contains the functions for fitting dynamic functional concurrent regression
#' with sparse data.
#'
#' @section Notation:
#'
#' Let $y_{ij}$ denote some outcome measured at
#' $t_{ij}$ on the functional domain (e.g. time) for subject $i$ at observation
#' $j$.
#' We focus on fitting models of the form
#'
#' \deqn{
#'  y_{ij} = f_0(t_{ij}) + f_1(t_{ij})X_{ij} + \cdots + b_i(t_{ij}) + \epsilon_{ij}
#' }
#'
#' @section Estimation:
#'
#' Estimation is performed using an iterative procedure described in Leroux et. al (2017).
#' Initially, a model is fit without $b_i(t_{ij})$. Using the residuals from this initial fit,
#' the covariance function is estimated. The model is then re-fit using this covariance function.
#' This procedure can be iterated as many times as desired.
#'
#' @references
#'
#' Leroux A, Xiao L, Crainiceanu C, Checkley W (2017).
#' Dynamic prediction in functional concurrent regression with an application to child growth.
#'
NULL


#' Example dataset
#'
#' @description Simulated data from the CONTENT dataset. Data contains information on child growth
#' as measured by WHO defined Z-scores
#' as well as gender.
#'
#'
#' @name content
#'
#' @format A dataframe with 8 variables:
#' \describe{
#'   \item{Y}{Observed HAZ score}
#'   \item{Ytrue}{True HAZ score}
#'   \item{waz.true}{True WAZ score}
#'   \item{waz}{Observed WAZ score}
#'   \item{Male}{Sex. 1 if male, 0 if female.}
#'   \item{argvals}{time of observations standardized to be in the interval [0,1]}
#'   \item{subj}{Subject ID}
#'   \item{include}{Indicator for out of sample prediction used in the vignette}
#' }
#'
#' @references
#'
#' Jaganath D, Saito M Giman RH Queirox DM, Rocha GA, Cama V, Cabrera L, Kelleher D, Windle HJ,
#' Crabtree JE, Jean E, Checkley W. First Detected Helicobacter pylori Infection in Infancy Modifies the
#' Association Between Diarrheal Disease and Childhood Growth in Peru. Helicobacter (2014); 19:272-297.
#'
#'
#'
NULL
