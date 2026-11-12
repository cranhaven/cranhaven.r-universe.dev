#' @title Available 'nomis' Tables
#'
#' @description
#' List all available datasets on 'nomis'. 
#' User can then use the items on this list to query 'nomis' data.
#' 
#' @param base_url Url of the API from which the available tables are listed.
#' 
#' @returns A tidy dataframe containing
#' the name and ID of each table available on 'nomis'.
#' 
#' @export

list_tables <- function(base_url = "https://www.nomisweb.co.uk/api/v01"){
  
  y <- httr::content(httr::GET(paste0(base_url, "/dataset/def.sdmx.json")))

  nomis_ids <- lapply(y$structure$keyfamilies$keyfamily, function (x) x[["id"]])
  nomis_names <- lapply(y$structure$keyfamilies$keyfamily, function (x) x[["name"]]$value)

  return(data.frame(id = unlist(nomis_ids), name = unlist(nomis_names)))
}
