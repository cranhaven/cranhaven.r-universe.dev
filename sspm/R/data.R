#' SFA boundaries data
#'
#' SFA boundaries.
#'
#' @format A data frame and sf object:
#' \describe{
#'   \item{sfa}{SFA ID number}
#'   \item{geometry}{sf geometry}
#'   \item{area}{sf geometry area}
#' }
#' @source \url{https://www.dfo-mpo.gc.ca/fisheries-peches/ifmp-gmp/shrimp-crevette/shrimp-crevette-2018-002-eng.html}
"sfa_boundaries"

#' Simulated biomass data
#'
#' Simulated biomass data for test and practice.
#'
#' @format A data frame:
#' \describe{
#'   \item{year_f}{Year as a factor}
#'   \item{sfa}{SFA ID number}
#'   \item{weight_per_km2}{Simualated biomass in kg per km2}
#'   \item{temp_at_bottom}{Simulated water temperature}
#'   \item{lon_dec}{Longitude}
#'   \item{lat_dec}{Latitude}
#'   \item{row}{Row ID}
#'   \item{uniqueID}{Unique ID for simulated observation}
#' }
"borealis_simulated"

#' Simulated predator data
#'
#' Simulated predator data for test and practice.
#'
#' @format A data frame:
#' \describe{
#'   \item{year_f}{Year as a factor}
#'   \item{sfa}{SFA ID number}
#'   \item{weight_per_km2}{Simualated biomass in kg per km2}
#'   \item{lon_dec}{Longitude}
#'   \item{lat_dec}{Latitude}
#'   \item{row}{Row ID}
#'   \item{uniqueID}{Unique ID for simulated observation}
#' }
"predator_simulated"

#' Simulated catch data
#'
#' Simulated catch data for test and practice.
#'
#' @format A data frame:
#' \describe{
#'   \item{year_f}{Year as a factor}
#'   \item{sfa}{SFA ID number}
#'   \item{catch}{Simualated catch in kg}
#'   \item{lon_dec}{Longitude}
#'   \item{lat_dec}{Latitude}
#'   \item{row}{Row ID}
#'   \item{uniqueID}{Unique ID for simulated observation}
#' }
"catch_simulated"
