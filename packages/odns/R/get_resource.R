#' Get one or more resources or all resources within a package.
#'
#' @description Get data from one or more resources, or all resources within a 
#'  package, as a list, with each resource in tabular format. Where field
#'  selection and/or filtering of data is required the \code{get_data} function
#'  can be used.
#'
#' @param package A character vector specifying package ids or names. If the 
#'  \code{resource} argument is not provided all resources under each of the
#'  specified packages will be returned. The \code{package} argument itself is
#'  optional, but one of \code{package} or \code{resource} arguments must be
#'  provided.
#' @param resource A character vector specifying resource ids or names. If the 
#'  \code{package} argument is also provided then resources will only be
#'  returned if they exist under one of the specified packages, otherwise each
#'  of the specified resources will be returned. The \code{resource} argument
#'  itself is optional, but one of \code{resource} or \code{package} arguments
#'  must be provided.
#' @param limit A numeric value specifying the maximum number of rows to be
#'  returned. Default value `Inf` returns all rows. Note; when multiple 
#'  resources are returned the limit applies to each.
#' 
#' @return A list containing all the resources within a package, or those
#'  specified, as data.frames.
#'  
#' @examples
#' \dontrun{
#' get_resource(
#'   package = "4dd86111-7326-48c4-8763-8cc4aa190c3e",
#'   limit = 5L
#'   )
#'   
#' get_resource(
#'   package = "4dd86111-7326-48c4-8763-8cc4aa190c3e",
#'   resource = "edee9731-daf7-4e0d-b525-e4c1469b8f69",
#'   limit = 5L
#'   )
#' 
#' get_resource(
#'   package = "standard-populations",
#'   resource = "European Standard Population",
#'   limit = 5L
#'   )
#'   
#'  get_resource(
#'   resource = "European Standard Population",
#'   limit = 5L
#'   )
#' }
#'  
#' @export
get_resource <- function(package = NULL, resource = NULL,  limit = Inf) {
  
  stopifnot("The package or resource argument must be specified." = !all(is.null(package), is.null(resource)))
  
  res <- (function(x = package, y = resource) {
    
    rsrc <- all_resources()
    
    if (!is.null(x)){
      rsrc <- rsrc[rsrc$package_name %in% x | rsrc$package_id %in% x,]
      x = x[!x %in% c(rsrc$package_name, rsrc$package_id)]
    } 
    
    if (!is.null(y)) {
      rsrc <- rsrc[rsrc$resource_name %in% y | rsrc$resource_id %in% y,]
      y = y[!y %in% c(rsrc$resource_name, rsrc$resource_id)]
    }
    
    rsrc <- list(
      urls = rsrc$url,
      pckg_not_found = list("packages", x),
      rsrc_not_found = list("resources", y)
    )
    
    return(rsrc)
  })()
  
  len <- length(res$urls)
  
  out <- vector(mode = "list", length = len)
  
  if (len > 0) {
    
    out <- lapply(res$urls, data.table::fread, keepLeadingZeros = TRUE, 
                  data.table = FALSE, nrows = limit, showProgress = FALSE)
    
    for (ii in list(res$pckg_not_found, res$rsrc_not_found)){
      
      if (length(ii[[2]]) > 0) {
        message(glue::glue("The following {ii[[1]]} were not found;\n"))
        message(glue::glue("{ii[[2]]} \n\n"))
      }
    }
    
  } else {
    
    warning("No resources found for arguments provided. Returning empty list.")
    out <- list()
    
  }
  
  return(out)
}
