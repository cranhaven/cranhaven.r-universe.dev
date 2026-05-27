#' foreSIGHT: A package for Systems Insights from Generation of Hydroclimatic Timeseries
#'
#' A tool to create hydroclimate scenarios, stress test systems and visualize
#' system performance in scenario-neutral climate change impact assessments.
#' @docType package
#' @name foreSIGHT
#' @useDynLib foreSIGHT
# NULL
#> NULL
#' @keywords internal
"_PACKAGE"

# globalVariables(c(".", "!!"))

#' Climate attributes from projections.
#'
#' A example dataset containing the climate attribute values
#' in fraction/additive change
#'
#' @format A data frame with 15 rows and 12 variables:
#' \describe{
#'   \item{P_day_all_tot_m}{change in mean annual total P, fraction}
#'   \item{P_day_all_seasRatioMarAug}{change in seasonal (Mar-Aug) ratio of P, fraction}
#'   \item{P_day_all_P99}{change in 99th percentile of P, fraction}
#'   \item{Temp_day_all_avg}{change in averageTemp, additive}
#'   \item{Name}{name of the climate model}
#'   \item{Avg. Deficit}{performance metric values}
#' }
"egClimData"

#' Performance metrics of the tank model using simple scaled scenarios.
#'
#' @format A list with 2 elements
#' \describe{
#'   \item{volumetric reliability (fraction)}{Volumetric reliaiblity of tank}
#'   \item{reliability (fraction)}{Reliability of tank}
#' }
"egScalPerformance"

#' Summary of a simple scaled scenario.
#'
#' Summary generated using the function \code{getSimSummary}.
#'
#' @format A list containing 3 elements
#' \describe{
#'   \item{simDates}{the dates of the simulation}
#'   \item{expSpace}{the exposure space of the simulation}
#'   \item{controlFile}{"scaling"}
#' }
"egScalSummary"

#' Summary of a regGrid scenario.
#'
#' Summary generated using the function \code{getSimSummary} for
#' a scenarios generated using stochastic models for a regGrid exposure space
#'
#' @format A list containing 13 elements
"egSimSummary"


#' Summary of a OAT scenario.
#'
#' Summary generated using the function \code{getSimSummary} for
#' a scenarios generated using stochastic models for an OAT exposure space
#'
#' @format A list containing 13 elements
"egSimOATSummary"

#' Performance metrics of the tank model using OAT scenarios.
#'
#' @format A list with 2 elements
#' \describe{
#'   \item{Avg. Deficit}{average daily deficit of water, litres}
#'   \item{Reliability}{reliability of the tank, fraction}
#' }
"egSimOATPerformance"

#' Performance metrics of the tank model using regGrid scenarios.
#'
#' @format A list with 2 elements
#' \describe{
#'   \item{Avg. Deficit}{average daily deficit of water, litres}
#'   \item{Reliability}{reliability of the tank, fraction}
#' }
"egSimPerformance"

#' Performance metrics of an alternate tank model using regGrid scenarios.
#'
#' @format A list with 2 elements
#' \describe{
#'   \item{Avg. Deficit}{average daily deficit of water, litres}
#'   \item{Reliability}{reliability of the tank, fraction}
#' }
"egSimPerformanceB"


#' Output from call to generateScenarios() using multi-site model (see example 5 in generateScenarios).
#'
#' @format A list with 4 elements
#' \describe{
#'   \item{Rep1}{List containing majority of simulation output, including output for different calibration stages }
#'   \item{simDates}{the dates of the simulation}
#'   \item{expSpace}{the exposure space of the simulation}
#'   \item{controlFile}{the setting in the control file}
#' }
"egMultiSiteSim"

#' Catchment data for Scott Creek in South Australia for period 1976-1985.
#'
#' @format A list with 4 elements
#' \describe{
#'   \item{times}{Vector of times in POSIXct format}
#'   \item{P}{Vector of precipitation data (mm)}
#'   \item{PET}{Vector of PET data (mm) (seasonally variable, no changes annually)}
#'   \item{Qobs}{Vector of observed streamflow (mm)}
#' }
"data_A5030502"

