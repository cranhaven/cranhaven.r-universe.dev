#' @title List 'nomis' Data Sources
#' 
#' @description
#' Return a list of the data sources available on 'nomis'.
#'
#' @description
#' Returns a list including the name, id and description of each
#' data source available on 'nomis'. More information can be found here: https://www.nomisweb.co.uk/api/v01/help
#' 
#' @param base_url Base nomis url to query
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' list_data_sources()
#' 
#' @returns A tidy dataframe of all available data sources accessible through the 'nomis' API system. 
#' @export

list_data_sources <- function(base_url = "https://www.nomisweb.co.uk/api/v01") {
  y <- httr::GET(paste0(base_url, "/contenttype/sources.json")) %>%
    httr::content()
 
  sources <- data.frame()
  for (i in seq_along(y$contenttype$item)) {
    if(y$contenttype$item[[i]]$id != "census" ){
      source <- data.frame(
        source_name = y$contenttype$item[[i]]$name,
        source_id = y$contenttype$item[[i]]$id,
        source_description = y$contenttype$item[[i]]$description)
      sources <- dplyr::bind_rows(sources, source)
    } else{
      for (j in seq_along(y$contenttype$item[[i]]$item)) {
        source <- data.frame(
          source_name = y$contenttype$item[[i]]$item[[j]]$name,
          source_id = y$contenttype$item[[i]]$item[[j]]$id,
          source_description = if(length(y$contenttype$item[[i]]$item[[j]]$description) >0 ){
            y$contenttype$item[[i]]$item[[j]]$description
          }else{"No Description"})
        sources <- dplyr::bind_rows(sources, source)
      }
    }
   
  }
 
  return(sources)
}
