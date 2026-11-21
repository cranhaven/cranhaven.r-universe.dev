## Sources from DESCRIPTION
#' @name OPSR-package
"_PACKAGE"

## Rdpack
#' @importFrom Rdpack reprompt
NULL

## Generics outside OPSR
#' @importFrom stats AIC BIC coef coefficients fitted model.frame model.matrix
#'   model.weights predict residuals update nobs
NULL

#' @importFrom methods setMethod
NULL

#' @importFrom texreg extract
NULL

## C++
#' @useDynLib OPSR, .registration=TRUE
#' @importFrom Rcpp evalCpp setRcppClass
NULL

#' Interface to C++ Log-Likelihood Implementation
#'
#' This is the main computation engine wrapped by [`opsr.fit`].
#'
#' @param theta named coefficient vector as parsed from formula interface [`opsr`].
#' @param W list of matrices with explanatory variables for selection process for each regime.
#' @param X list of matrices with expalanatory varialbes for outcome process for each regime.
#' @param Y list of vectors with continuous outcomes for each regime.
#' @param weights vector of weights. See also [`opsr`].
#' @param nReg integer number of regimes.
#' @param nThreads number of threads to be used by `OpenMP` (should be max. `nReg`).
#'
#' @return Numeric vector of (weighted) log-likelihood contributions.
#' @usage loglik_cpp(theta, W, X, Y, weights, nReg, nThreads)
#' @name loglik_cpp
#'
#' @seealso [`opsr.fit`], [`loglik_R`]
#' @export
NULL

#' Prepares Coefficients for Likelihood Function
#'
#' Extracts the coefficients for each regime
#'
#' @param theta named coefficient vector as parsed from formula interface [`opsr`].
#' @param nReg integer number of regimes.
#'
#' @return Named list of length `nReg`
#' @usage opsr_prepare_coefs(theta, nReg)
#' @name opsr_prepare_coefs
#'
#' @example R/examples/ex-opsr_prepare_coefs.R
#' @export
NULL

#' Check Whether `OpenMP` is Available
#'
#' @return boolean
#' @usage opsr_check_omp()
#' @name opsr_check_omp
#'
#' @export
NULL

#' Check Maximum Number of Threads Available
#'
#' @return integer
#' @usage opsr_max_threads()
#' @name opsr_max_threads
#'
#' @seealso [`opsr_check_omp`]
#' @export
NULL

## Data
## see here for documentation example: https://github.com/tidyverse/nycflights13/blob/main/R/flights.R

#' Telework Data
#'
#' Telework data as used in \insertCite{Wang+Mokhtarian:2024;textual}{OPSR}.
#'
#' @example R/examples/ex-telework_data.R
#'
#' @references
#' \insertRef{Wang+Mokhtarian:2024}{OPSR}
#'
#' @format Data frame with numeric columns
#' \describe{
#' \item{id}{Respondent ID}
#' \item{weight}{Sample weight}
#' \item{vmd}{Weekly vehicle-miles traveled}
#' \item{vmd_ln}{Log-transformed VMD, the dependent variable of the outcome model}
#' \item{twing_status}{Teleworking status: 1=Non-TWer, 2=Non-usual TWer, 3=Usual TWer}
#' \item{female}{Sex: female}
#' \item{age_mean}{Mean-centered age}
#' \item{age_mean_sq}{Sqaure of mean-centered age}
#' \item{race_white}{Race: white only}
#' \item{race_black}{Race: black only}
#' \item{race_other}{Race: other}
#' \item{edu_1}{Education: high school or lower}
#' \item{edu_2}{Education: some college}
#' \item{edu_3}{Education: BA or higher}
#' \item{hhincome_1}{Household income: less than $50,000}
#' \item{hhincome_2}{Household income: $50,000 to $99,999}
#' \item{hhincome_3}{Household income: $100,000 or more}
#' \item{flex_work}{Flexible work schedule}
#' \item{work_fulltime}{Full-time worker}
#' \item{twing_feasibility}{Teleworking feasibility (days/month)}
#' \item{vehicle}{Number of household vehicles}
#' \item{child}{Number of children}
#' \item{urban}{Residential location: urban}
#' \item{suburban}{Residential location: suburban}
#' \item{smalltown}{Residential location: small town}
#' \item{rural}{Residential location: rural}
#' \item{att_prolargehouse}{Attitude: pro-large-house}
#' \item{att_proactivemode}{Attitude: pro-active-mode}
#' \item{att_procarowning}{Attitude: pro-car-owning}
#' \item{att_wif}{Attitude: work-interferes-with-family}
#' \item{att_proteamwork}{Attitude: pro-teamwork}
#' \item{att_tw_effective_teamwork}{Attitude: TW effective teamwork}
#' \item{att_tw_enthusiasm}{Attitude: TW enthusiasm}
#' \item{att_tw_location_flex}{Attitude: TW location flexibility}
#' \item{region_waa}{Region indicator: respondents from WAA MSA}
#' }
"telework_data"


#' TimeUse+ Data
#'
#' TimeUse+ \insertCite{Winkler+Meister+Axhausen:2024}{OPSR} was a tracking study
#' conducted at the Institute for Transport Planning and Systems (IVT) at ETH Zurich.
#' The data allows researchers to investigate time-use and mobility patterns.
#' Full data access can be requested via <doi:10.3929/ethz-b-000634868>.
#'
#' @details
#' The data comprises employed individuals only and is based on valid days only.
#' A valid day has at least 20h of information where 70% of the events were
#' validated by the user. The telework variables are based on tracked work activities.
#'
#' @references
#' \insertRef{Winkler+Meister+Axhausen:2024}{OPSR}
#'
#' @format Data frame with numeric and factor columns
#' \describe{
#' \item{id}{Respondent ID}
#' \item{start_tracking}{Indicator for the months when the participant started tracking}
#' \item{weekly_km}{Weekly distance covered (in km) across all modes}
#' \item{log_weekly_km}{Log of `weekly_km`}
#' \item{wfh_days}{Based on tracked work episodes. Average full working days spent in home office during a typical week.}
#' \item{wfh}{Derived from `wfh_days`: NTW=Non-TWer, NUTW=Non-usual TWer (< 3 days/week), UTW=Usual TWer(3+ days/week)}
#' \item{commute_km}{Map matched commute distance in km}
#' \item{log_commute_km}{Log of `commute_km`}
#' \item{age}{Age}
#' \item{car_access}{Has access to a household car}
#' \item{dogs}{Household owns dogs}
#' \item{driverlicense}{Owns driver's license for cars}
#' \item{educ_higher}{Higher education (e.g., university)}
#' \item{fixed_workplace}{Main work location does not change regularly}
#' \item{grocery_shopper}{Does usually do the grocery shopping}
#' \item{hh_income}{Gross household income per month (CHF)}
#' \item{hh_size}{Total household size}
#' \item{isco_clerical}{International standard classification of Occupations (ISCO-08): Clerical support workers}
#' \item{isco_craft}{International standard classification of Occupations (ISCO-08): Craft related trade workers}
#' \item{isco_elementary}{International standard classification of Occupations (ISCO-08): Elementary occupations}
#' \item{isco_managers}{International standard classification of Occupations (ISCO-08): Managers}
#' \item{isco_plant}{International standard classification of Occupations (ISCO-08): Plant and machine operators, and assemblers}
#' \item{isco_professionals}{International standard classification of Occupations (ISCO-08): Professionals}
#' \item{isco_service}{International standard classification of Occupations (ISCO-08): Service and sales workers}
#' \item{isco_agri}{International standard classification of Occupations (ISCO-08): Skilled agricultural, forestry and fishery workers}
#' \item{isco_tech}{International standard classification of Occupations (ISCO-08): Technicians and associate professionals}
#' \item{married}{Married}
#' \item{n_children}{Number of household members below the age of 18}
#' \item{freq_onl_order}{Orders products online more than once a month}
#' \item{parking_home}{Has at least one reserved parking space at home}
#' \item{parking_work}{Has at least one reserved parking space at work location}
#' \item{permanent_employment}{Employment contract type: permenent (unlimited employed)}
#' \item{rents_home}{Residential situation: Rents home}
#' \item{res_loc}{Residential location}
#' \item{sex_male}{Gender}
#' \item{shift_work}{Whether participant works in shifts}
#' \item{swiss}{Swiss citizen}
#' \item{vacation}{Participant took time off of work during study}
#' \item{workload}{Workload (% of full-time employment which is 41.7 h/week)}
#' \item{young_kids}{Whether children aged 12 or younger live in the household}
#' }
"timeuse_data"
