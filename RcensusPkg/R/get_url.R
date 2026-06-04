#' @title .get_url
#'
#' @description An internal function that creates the base url
#'   address to be submitted as a http GET request by \code{.get_dt}.
#'   It initializes with the dataset name and optionally the vintage.
#'
#' @param dataset A string that sets the name of the data set of interest (e.g. "acs/acs5").
#'  This is a required parameter.
#' @param vintage An optional numeric that sets the vintage of interest.
#'
#' @return Returns a url string.
#'
#' @keywords internal
#'
.get_url <- function(dataset, vintage){
  if(is.null(vintage)){
    apiurl <- paste("https://api.census.gov/data", dataset, sep = "/")
  }else {
    apiurl <- paste("https://api.census.gov/data", vintage, dataset, sep = "/")
  }
  return(apiurl)
}

