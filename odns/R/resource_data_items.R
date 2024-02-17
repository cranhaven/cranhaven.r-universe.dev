#' Get a table of available fields and their types for a specified resource.
#'
#' @param resource A character string containing the resource id of the resource
#'  for which information is to be returned.
#'
#' @return A data.frame detailing the names and types of all fields available
#'  for the chosen resource.
#'
#' @examples
#' \dontrun{
#' resource_data_items(resource="edee9731-daf7-4e0d-b525-e4c1469b8f69")
#' }
#'
#' @export
resource_data_items <- function(resource) {
  
  query <- utils::URLencode(
    glue::glue(
      "https://www.opendata.nhs.scot/api/3/action/",
      "datastore_search?id={resource}&limit=0"
    ))
  
  cap_url(query)
  
  res = httr::RETRY(
    verb = "GET",
    url = query,
    times = 3,
    quiet = TRUE,
    terminate_on = c(404)
  )
  
  detect_error(res)
  
  cont = httr::content(res)
  
  cont = lapply(cont$result$fields, as.data.frame, stringsAsFactors = FALSE)
  
  cont = data.table::setDF(
    data.table::rbindlist(cont, use.names = TRUE, fill = TRUE)
  )[c("id", "type")]
  
  return(cont)
}
