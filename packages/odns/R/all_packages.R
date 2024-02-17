#' Details packages available from <opendata.nhs.scot>.
#' 
#' @description Details all packages available from <opendata.nhs.scot> in a
#'  data.frame along with the package id, with the option to limit results based
#'  on a search term.
#'  
#' @param contains a character string containing an expression to be used as
#'  search criteria against the packages 'title' field.
#' @param limit a numeric value specifying the maximum number of rows to be
#' returned. Defaults to 1000L.
#' 
#' @return a data.frame containing the names of all available packages and their
#'  package ids, or those whose name contains the string specified in the 
#'  \code{contains} argument.
#'  
#' @examples
#' \dontrun{
#' all_packages()
#' all_packages(contains = "standard-populations")
#' }
#' 
#' @export
all_packages <- function(contains = NULL, limit = 1000L) {
  
  query = utils::URLencode(glue::glue(
    "https://www.opendata.nhs.scot/api/3/action/",
    "package_search?",
    "{if (is.null(contains)) \"\" else glue::glue(\"q=title:{contains}&\")}",
    "rows={limit}"
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
  
  out <- jsonlite::fromJSON(jsonlite::toJSON(httr::content(res)$result))$results
  
  out <- data.frame(
    package_name=unlist(out$name),
    package_id = unlist(out$id),
    stringsAsFactors = FALSE
  )
  
  return(out)
}
