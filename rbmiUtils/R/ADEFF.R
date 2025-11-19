#' Example efficacy trial dataset
#'
#' A simplified example of a simulated trial dataset, with missing data.
#'
#' @format `ADEFF`
#' A data frame with 1,000 rows and 10 columns:
#' \describe{
#'    \item{USUBJID}{Unique subject identifier}
#'    \item{AVAL}{Primary outcome variable}
#'    \item{TRT01P}{Planned treatment}
#'    \item{STRATA}{Stratification at randomisation}
#'    \item{REGION}{Stratification by region}
#'    \item{REGIONC}{Stratification by region, numeric code}
#'    \item{BASE}{Baseline value of primary outcome variable}
#'    \item{CHG}{Change from baseline}
#'    \item{AVISIT}{Visit number}
#'    \item{PARAM}{Analysis parameter name}
#' }
#'
"ADEFF"
