#' Dataset contains information of full comprehensive Australian automobile insurance policies between years 2004 and 2005
#' A dataset containing the claim and three attributes of 67,856 policies
#' @docType data
#' @format A data frame with 67856 rows and 4 columns
#' \describe{
#'    \item{y}{Binary vaiable with 0 denote a policy with no claim, and 1 denote a claim policy.}
#'    \item{gender}{gender of deriver}
#'    \item{age}{age of deriver}
#'    \item{exposure}{period from the date of insured to the investigation, with a maximum of one year}
#' }
#' @references De Jong P et al. (2008). “Generalized linear models for insurance data.” Cambridge Books.
#' @keywords dataset
#' @examples
#' data(car.insur)
"car.insur"
