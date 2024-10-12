#' Get National Library Metadata for identifiers
#'
#' This function retrieves metadata for objects from the National Library API based on either a vector of dhlabids or a vector of National Library URNs.
#'
#' @param dhlabids A vector of dhlabids (default is NULL). When provided, the function will use dhlabids to fetch metadata.
#' @param urns A vector of National Library URNs (default is NULL). When provided, the function will use URNs to fetch metadata.
#' @return A dataframe containing the National Library metadata for the specified objects.
#' @examples
#'   urns_example <- c("URN:NBN:no-nb_digibok_2008051404065", "URN:NBN:no-nb_digibok_2010092120011")
#'   metadata_urns <- get_metadata(urns = urns_example)
#' @export
get_metadata <- function(dhlabids = NULL, urns = NULL){

  url <- "https://api.nb.no/dhlab/get_metadata"

  params <- list("dhlabids" = dhlabids, "urns" = urns)
  #query <- POST(url, body = params, encode = "json")
  query <- api_call_wrapper(url, body = params, encode = "json")

  if  (is.null(query)) {
    return(NULL)
  }

  return(do.call(cbind, content(query)))

}
