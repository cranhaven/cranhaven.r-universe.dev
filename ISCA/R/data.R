#' Cross-sectional, artificial data on 1000 individuals
#'
#' Small, artificially created dataset in a cross-sectional format. 
#' Provides information on 1000 individuals to illustrate the use of the package.
#'
#' @usage data(sim_data)
#' 
#' @format A data frame with 1,000 rows and 7 columns:
#' \describe{
#'   \item{female}{Dichotomous variable (0/1) indicating a person's sex}
#'   \item{age}{value indicating a person's age, range 18-80}
#'   \item{education}{value indicating a person's level of education, range 1-9}
#'   \item{income}{value indicating a person's income}
#'   \item{religiosity}{value indicating a person's level of religiosity, range 1-10}
#'   \item{discrimination}{value indicating a person's level of experience discrimination, range 0-8 }
#'   \item{native}{Dichotomous variable (0/1) indicating whether a person is a native (1) or an immigrant (0)}
#' }
#' @references The data was artificially created for the ISCA package.
#' @examples
#' 
#' data(sim_data)
#' head(sim_data)
"sim_data"
