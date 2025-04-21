
#' A generic function for the MNIS API
#'
#' The function requests data in JSON format, but the type of object,
#' and all URLs, paths and parameters are user-defined. `mnis_base`
#' does not include the option to tidy variable names and data types.
#'
#' See the API documentation at
#' \url{http://data.parliament.uk/membersdataplatform/memberquery.aspx} for
#' details and limits on requests made to MNIS.
#'
#' @param request The request query being made to the MNIS URL.
#' @export
#' @examples
#' \dontrun{
#'
#' x <- mnis_base("House=Commons|IsEligible=true/")
#' }
#'
mnis_base <- function(request) {
  baseurl <- "http://data.parliament.uk/membersdataplatform/services/mnis/members/query/"

  q_url <- paste0(base_url, "members/query/")

  request <- utils::URLencode(request)

  query <- paste0(baseurl, request)

  got <- mnis_query(query)

  x <- do.call(rbind, got$Members$Member)
}
