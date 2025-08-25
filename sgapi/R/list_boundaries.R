#' @title List Available Boundaries
#' 
#' @description
#' Retrieve all available ArcGIS boundary layers
#' from the 'ONS Open Geography Portal'.
#'
#' @param base_url Open geography portal base url
#' 
#' @returns A vector of available boundary layers on 'ONS Open Geography'.
#' 
#' @export

list_boundaries <- function(base_url = "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services") {
  open_geography_url <- paste0(base_url, "/?f=json")
  
  message("Querying open geography portal -> ", open_geography_url)
  
  raw_data <- httr::content(httr::GET(open_geography_url))
  
  return(unlist(lapply(raw_data$services, function(x) x[["name"]])))
}

