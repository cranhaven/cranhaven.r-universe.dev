#' Get metadata for a package.
#' 
#' @description Get a specified packages metadata as a list.
#'  
#' @param package A character vector of length 1 specifying a package id or name
#'  which identifies the package for which metadata should be returned.
#'  
#' @return a list containing the package metadata.
#' 
#' @examples
#' \dontrun{
#' package_metadata(package = "standard-populations")
#' package_metadata(package = "4dd86111-7326-48c4-8763-8cc4aa190c3e")
#' }
#' 
#' @export
package_metadata <- function(package) {
  
  query <- utils::URLencode(glue::glue(
    "https://www.opendata.nhs.scot/api/3/action/package_show?id={package}"
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
