#' Get the number of rows present in a resource.
#' 
#' @param resource A character string containing the resource id of the data set
#'  to be returned.
#'
#' @return An integer of length 1 indicating the number of rows present in the
#'  specified resource.
#'
#' @examples
#' \dontrun{
#' nrow_resource(resource = "edee9731-daf7-4e0d-b525-e4c1469b8f69")
#' }
#' @export
nrow_resource <- function(resource) {
  query <- utils::URLencode(
    glue::glue(
      "https://www.opendata.nhs.scot/api/3/action/datastore_search_sql?sql=",
      "SELECT COUNT(*) FROM \"{resource}\"",
    ))
  
  res <- httr::RETRY(
    verb = "GET",
    url = query,
    times = 3,
    quiet = TRUE,
    terminate_on = c(404)
  )
  
  detect_error(res)
  
  res <- as.integer(unlist(httr::content(res)$result$records))
  
  return(res)
}
