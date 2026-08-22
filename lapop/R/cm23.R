#' cm23: Single-country Multi-year Dataset
#'
#' A dataset containing the AmericasBarometer Brazil  Country Merge up to 2023.
#'
#' @format A data frame
#' \describe{
#'  \item{ing4}{Support for Democracy}
#'  \item{b13}{Trust in the National Legislature}
#'  \item{b21}{Trust in Political Parties}
#'  \item{b31}{Trust in the Supreme Court of Justice}
#'  \item{wave}{Survey round year for regional or multi-country data}
#'  \item{pais}{Country of survey}
#'  \item{year}{Survey round year for single-country data}
#'  \item{upm}{Primary Sampling Unit}
#'  \item{strata}{Stratification}
#'  \item{weight1500}{Cross-country and cross-time weight}
#' }
#' @source LAPOP AmericasBarometer (https://www.vanderbilt.edu/lapop/)
#' @export
cm23 <- load("data/cm23.rda")
