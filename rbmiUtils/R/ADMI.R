#' Example multiple imputation trial dataset
#'
#' A simplified example of a simulated trial ADMI dataset
#'
#' @format `ADMI`
#' A data frame with 100,000 rows and 12 columns:
#' \describe{
#'    \item{USUBJID}{Unique patient identifier}
#'    \item{STRATA}{Stratification at randomisation}
#'    \item{REGION}{Stratification by region}
#'    \item{REGIONC}{Stratification by region, numeric code}
#'    \item{TRT}{Planned treatment}
#'    \item{BASE}{Baseline value of primary outcome variable}
#'    \item{CHG}{Change from baseline}
#'    \item{AVISIT}{Visit number}
#'    \item{IMPID}{Imputation number identifier}
#'    \item{CRIT1FLN}{Responder criteria (binary)}
#'    \item{CRIT1FL}{Responder criteria (categorical)}
#'    \item{CRIT}{Responder criteria (definition)}
#' }
#'
"ADMI"
