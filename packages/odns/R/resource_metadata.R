#' Get metadata for a resource.
#' 
#' @description Get a specified resources metadata as a list.
#'  
#' @param resource A character vector of length 1 specifying a resource id
#'  which identifies the resource for which metadata should be returned.
#'  
#' @return a list containing the resource metadata.
#' 
#' @examples
#' \dontrun{
#' resource_metadata(resource = "edee9731-daf7-4e0d-b525-e4c1469b8f69")
#' }
#' 
#' @export
resource_metadata <- function(resource) {
  
  query <- utils::URLencode(glue::glue(
    "https://www.opendata.nhs.scot/api/3/action/resource_show?id={resource}"
  ))
  
  cap_url(query)
  
  res <- httr::RETRY(
    verb = "GET",
    url = query,
    times = 3,
    quiet = TRUE,
    terminate_on = c(404)
  )
  
  detect_error(res)
  
  con <- httr::content(res)
  
  con <- jsonlite::fromJSON(jsonlite::toJSON(con$result))
  
  return(con)
}
