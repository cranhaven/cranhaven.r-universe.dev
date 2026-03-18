#' General population survival data.
#'
#' A dataset containing daily population survival rates for individuals up to 100 years old,
#' from the UK population, derived from the 2009 mortality rates found at:
#' \url{https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesunitedkingdomreferencetables},
#' Adapted from public sector information licensed under the Open Government Licence v3.0.
#' Data were relabelled according to the mean year of the three-year birth window.
#' It is stored as a \code{data.table} for efficient access.
#'
#' @format A data frame with 109575 rows and 3 columns:
#' \describe{
#'  \item{age}{age in days}
#'  \item{sex}{string, either 'M' or 'F'}
#'  \item{surv}{survival probability , estimated as the cumulative product of (1 - mortality rate)}
#' }
"UKmortality"

#' Simulated patient dataset.
#'
#' A dataset in the format of a disease registry, where the outcome being modelled is death
#' due to the simulated disease. The registry began in January 2003, with 1000 incident cases being
#' recorded over a period of nearly exactly ten years. The patients are followed up for a further
#' two years until 17.03.2015, at which point any subjects alive are marked as right censored.
#'
#' Demographic and disease-specific data required for prevalence estimations are included,
#' such as sex, age, and dates of entry and event. \code{eventdate} marks the date of the
#' last known follow-up with the patient, corresponding to death (\code{status = 1}) or
#' censorship (\code{status = 0}).
#'
#' @format A data frame with 1000 rows and 6 columns:
#' \describe{
#'  \item{time}{time between date of diagnosis and death or censorship in days}
#'  \item{status}{event marker; 1 if patient is deceased and 0 if alive or censored}
#'  \item{age}{age in years at point of entry into the registry}
#'  \item{sex}{string with values 'M' and 'F'}
#'  \item{entrydate}{date of entry into the registry in YYYY-MM-DD format}
#'  \item{eventdate}{date of death or censorship in YYYY-MM-DD format}
#' }
"prevsim"