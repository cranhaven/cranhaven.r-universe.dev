#' Daily flu mortality from 1918 flu pandemic. 
#' 
#' Daily mortality data from 1918-09-01 through 1918-12-31 in Indiana, Kansas, and Philadelphia
#' 
#' @format A data frame with 122 entries for 3 locations
#' \describe{
#'   \item{Date}{date}
#'   \item{Indiana}{daily deaths for all of Indiana}
#'   \item{Kansas}{daily deaths for all of Kansas}
#'   \item{Philadelphia}{daily deaths for Philadelphia}
#' }
#' @source Rogers SL (1920). Special Tables of Mortality from Influenza and Pneumonia, in Indiana, Kansas, and Philadelphia, PA (U.S. Dept Commerce, Washington, DC).
"spanish_flu"

#' Delay distribution from 1918 flu pandemic. 
#' 
#' Daily death proportions.
#' 
#' @format A data frame with 31 entries and 3 columns.
#' \describe{
#'   \item{days}{number of days since infection}
#'   \item{proportion}{proportion of deaths that happen on that day}
#' }
#' @source Goldstein E, et al. (2009). Reconstructing influenza incidence by deconvolution of daily mortality time series (PNAS). \url{https://www.pnas.org/content/pnas/106/51/21825.full.pdf}
"spanish_flu_delay_dist"

#' Delay distribution from COVID-19 pandemic. 
#' 
#' Daily case, hospitalization, and death proportions.
#' 
#' @format A data frame with 61 entries and 4 columns.
#' \describe{
#'   \item{days}{number of days since infection}
#'   \item{case}{proportion of cases confirmed by a test that are recorded on that day}
#'   \item{hospitalization}{proportion of cases that become hospitalized that are hospitalized on that day}
#'   \item{death}{proportion of cases that result in death that die on that day}
#' }
#' @source Time from incidence to symptoms: Lauer et al., "Estimated Incubation Period of COVID-19", ACC (2020). \url{https://www.acc.org/latest-in-cardiology/journal-scans/2020/05/11/15/18/the-incubation-period-of-coronavirus-disease}.
#' @source Time from symptoms to recorded cases: Case line data from Florida through 2020-07-14 with same day waits removed. \url{https://open-fdoh.hub.arcgis.com/datasets/florida-covid19-case-line-data}.
#' @source Time from symptoms to hospitalization: Wang et al., "Clinical Characteristics of 138 Hospitalized Patients With 2019 Novel Coronavirus–Infected Pneumonia in Wuhan, China", JAMA (2020). \url{https://jamanetwork.com/journals/jama/fullarticle/2761044}.
#' @source Time from hospitalization to death: Lewnard et al. "Incidence, clinical outcomes, and transmission dynamics of severe coronavirus disease 2019 in California and Washington: prospective cohort study", BJM (2020). \url{https://www.bmj.com/content/369/bmj.m1923.long}
"covid_delay_dist"

#' New York City data from the COVID-19 pandemic. 
#' 
#' Daily case, hospitalization, and death proportions by borough through 2020-06-30.
#' 
#' @format A data frame with 615 entries and 5 columns.
#' \describe{
#'   \item{date}{record date}
#'   \item{borough}{record borough: Brooklyn, Bronx, Manhattan, Queens, and Staten Island}
#'   \item{case}{number of recorded cases}
#'   \item{hospitalization}{number of new hospital admissions}
#'   \item{death}{number of recorded deaths}
#' }
#' @source New York City Department of Health \url{https://raw.githubusercontent.com/nychealth/coronavirus-data/master/boro/boroughs-case-hosp-death.csv}.
"covid_new_york_city"
