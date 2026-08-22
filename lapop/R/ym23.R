#' ym23: Multi-country Single-year Dataset
#'
#' A dataset containing the AmericasBarometer Year Merge of 2023.
#'
#' @format A data frame
#' \describe{
#' \item{b12}{Trust in Armed Forces}
#' \item{b18}{Trust in National Police}
#' \item{ing4}{Support for Democracy}
#' \item{pn4}{Satisfaction with Democracy}
#' \item{vb21n}{Influence Political Change}
#' \item{q14f}{Migration Intentions}
#' \item{edre}{Education}
#' \item{wealth}{Wealth}
#' \item{q1tc_r}{Gender}
#' \item{upm}{Primary Sampling Unit}
#' \item{strata}{Stratification}
#' \item{wave}{Survey round year for regional or multi-country data}
#' \item{pais}{Country of survey}
#' \item{year}{Survey round year for single-country data}
#' \item{weight1500}{Cross-country and cross-time weight}
#' }
#' @source LAPOP AmericasBarometer (https://www.vanderbilt.edu/lapop/)  # Ensure this line has a valid source.
#' @export
#'
ym23 <- load("data/ym23.rda")
