#' ACS_CC
#'
#' A case-control sample extracted from American Community Survey (ACS) 2018, restricted to white males residing in California with at least a bachelor's degree.
#' The original ACS dataset is not from case-control sampling, but this case-control sample is obtained by the following procedure.
#' The case sample is composed of 921 individuals whose income is top-coded.
#' The control sample of equal size is randomly drawn without replacement from the pool of individuals whose income is not top-coded.
#' Age is restricted to be between 25 and 70.
#' @format A data frame with 1842 rows and 4 variables:
#' \describe{
#'   \item{age}{age, in years}
#'   \item{ind}{industry code, in four digits}
#'   \item{baplus}{1 if a respondent has a master’s degree, a professional degree, or a doctoral degree; 0 otherwise}
#'   \item{topincome}{1 if income is top-coded; 0 otherwise}
#' }
#' @source \url{https://usa.ipums.org/usa/}
"ACS_CC"


