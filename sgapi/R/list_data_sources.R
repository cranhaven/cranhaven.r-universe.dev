#' @title List 'nomis' Data Sources
#' 
#' @description
#' Return a list of the data sources available on 'nomis'.
#'
#' @description
#' Returns a list including the name, id and description of each
#' data source available on 'nomis'. More information can be found here: https://www.nomisweb.co.uk/api/v01/help
#' 
#' @importFrom magrittr %>%
#'
#' @examples
#' list_data_sources()
#' 
#' @returns A tidy dataframe of all available data sources accessible through the 'nomis' API system. 
#' @export

list_data_sources <- function() {
  y <-
    httr::GET("https://www.nomisweb.co.uk/api/v01/contenttype/sources.json") %>%
    httr::content()
  
  sources <- data.frame()
  for (i in seq_along(y$contenttype$item)) {
      source <-
        data.frame(
          source_name = y$contenttype$item[[i]]$name,
          source_id = y$contenttype$item[[i]]$id,
          source_description = y$contenttype$item[[i]]$description
        )

    sources <- dplyr::bind_rows(sources, source)
  }
  
  return(sources)
}
