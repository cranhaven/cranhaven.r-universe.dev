#' bra23: Single-country Single-year Dataset
#'
#' A dataset containing the AmericasBarometer Brazil 2023 survey round.
#'
#' @format A data frame
#' \describe{
#' \item{ing4}{Support for Democracy}
#' \item{b12}{Trust in Armed Forces}
#' \item{b13}{Trust in the National Legislature}
#' \item{b21}{Trust in Political Parties}
#' \item{b31}{Trust in the Supreme Court of Justice}
#' \item{fs2}{Food security}
#' \item{idio2}{Personal economic situation}
#' \item{wealth}{Wealth}
#' \item{wave}{Survey round year for regional or multi-country data}
#' \item{pais}{Country of survey}
#' \item{year}{Survey round year for single-country data}
#' \item{upm}{Primary Sampling Unit}
#' \item{strata}{Stratification}
#' \item{wt}{Country-specific post stratification weight}
#' }
#' @source LAPOP AmericasBarometer (https://www.vanderbilt.edu/lapop/)
#' @export
bra23 <- load("data/bra23.rda")
