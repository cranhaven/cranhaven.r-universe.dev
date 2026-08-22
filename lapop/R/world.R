#' bra23: Single-country Single-year Dataset
#'
#' A dataset containing the World Map geometry for lapop_map() function.
#'
#' @format A data frame
#' \describe{
#' \item{pais}{Country name}
#' \item{pais_lab}{Country ISO2 code}
#' \item{geometry}{Polygon with geometry}
#' }
#' @source LAPOP AmericasBarometer (https://www.vanderbilt.edu/lapop/)
#' @export
world <- load("data/world.rda")
