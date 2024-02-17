#' Provides an overview of all resources available from <opendata.nhs.scot>.
#' 
#' @description Provides an overview of all resources available from 
#'  <opendata.nhs.scot>, with the option to limit results based on both package
#'  and resource names. The returned data.frame can be used to look-up package
#'  and resource ids and is useful for exploring the available data sets.
#'  
#' @param package_contains  a character string containing an expression to be 
#'  used as search criteria against the packages 'title' field.
#' @param resource_contains a character string containing a regular expression
#'  to be matched against available resource names. If a character vector > 
#'  length 1 is supplied, the first element is used.
#' 
#' @return a data.frame containing details of all available packages and
#'  resources, or those containing the string specified in the 
#'  \code{package_contains} and \code{resource_contains} arguments.
#'  
#' @examples
#' \dontrun{
#' all_resources()
#' all_resources(package_contains = "standard-populations")
#' all_resources(
#'   package_contains = "standard-populations", resource_contains = "European"
#' )
#' }
#' 
#' @export
all_resources <- function(package_contains = NULL, resource_contains = NULL) {
  
  stopifnot(is.null(package_contains) || length(package_contains)== 1)
  stopifnot(is.null(resource_contains) || length(resource_contains)== 1)
  
  query = utils::URLencode(glue::glue(
    "https://www.opendata.nhs.scot/api/3/action/",
    "package_search?",
    "{if (is.null(package_contains)) \"\" else glue::glue(\"q=title:{package_contains}&\")}",
    "rows={32000}"
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
  
  pkgs <- unlist(out$name)
  
  names(pkgs) <- unlist(out$id)
  
  out <- jsonlite::fromJSON(jsonlite::toJSON(out$resources), flatten = TRUE)
  
  out <- data.table::rbindlist(out, use.names = TRUE, fill = TRUE)
  
  out <- data.frame(
    resource_name = unlist(as.character(out$name)),
    resource_id = unlist(as.character(out$id)),
    package_name = unname(as.character(pkgs[unlist(out$package_id)])),
    package_id = unlist(as.character(out$package_id)),
    url = unlist(as.character(out$url)),
    last_modified = unlist(as.character(out$last_modified)),
    stringsAsFactors = FALSE
  )
  
  if (!is.null(resource_contains)) {
    out <- out[grepl(as.character(resource_contains), out$resource_name, ignore.case = TRUE),] 
    if (nrow(out) == 0)  warning(
      "No resources found for arguments provided. Returning empty data.frame."
    )
  }
  
  return(out)
}
