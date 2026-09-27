#' Behavioral Risk Factor Surveillance System (BRFSS) 2013 Subsample
#'
#' The `brfss` data set contains a randomly selected subsample of 2,000 respondents
#' from the 2013 Behavioral Risk Factor Surveillance System (BRFSS) survey.
#' The BRFSS is an ongoing, state-based telephone survey conducted by the U.S.
#' Centers for Disease Control and Prevention (CDC) that collects information on
#' health-related risk behaviors, chronic health conditions, and the use of preventive
#' health services from non-institutionalized adults.
#'
#' In the context of post-linkage data analysis (PLDA), this data set is
#' partitioned into two separate files to simulate a secondary analysis scenario
#' where disparate data sources must be linked using quasi-identifiers (e.g., state,
#' month of interview, sex, and marital status) prior to performing regression
#' analyses (e.g., predicting weight based on height, physical health, and mental health).
#'
#' Pre-processing steps have been applied to this subsample to remove missing values
#' ("Refused" or `NA`) and to filter out extreme outliers in physical measurements.
#'
#' \itemize{
#'   \item \code{X}: State of residence (categorical).
#'   \item \code{imonth}: Month of interview (categorical).
#'   \item \code{Weight}: Weight of the respondent in pounds. Records with a weight
#'   less than 50 lbs or greater than 650 lbs were excluded.
#'   \item \code{Height}: Height of the respondent, formulated as (feet * 100 + inches).
#'   Records with a height less than 54 inches or greater than 84 inches were excluded.
#'   \item \code{Physhlth}: Number of days with poor physical health. Records marked
#'   "Refused" or NA were excluded.
#'   \item \code{Menthlth}: Number of days with poor mental health. Records marked
#'   "Refused" or NA were excluded.
#'   \item \code{Exerany}: Indicator of whether the respondent engaged in physical
#'   activity. Records marked "Refused" or NA were excluded.
#'   \item \code{m.rate}: Block-wise (by interview month) mismatch rate.
#' }
#'
#' @docType data
#' @name brfss
#' @usage data(brfss)
#' @format A data frame with 2,000 rows and 8 variables.
#'
#' @references Centers for Disease Control and Prevention (CDC) (2013). "Behavioral
#' Risk Factor Surveillance System Survey Data." U.S. Department of Health and Human
#' Services, Centers for Disease Control and Prevention. Available at
#' \url{https://www.cdc.gov/brfss/annual_data/annual_2013.html}.
"brfss"
