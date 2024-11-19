#' @title Get 'nomis 'Table IDs
#' 
#' @description
#' Extract unique table ids for 'nomis' tables containing given name in their title, 
#' these unique table ids can be used to rapidly query census data in other functions e.g. 'get_overview("NM_102_1")'
#' 
#' @import xml2
#' 
#' @param name A string to search for within 'nomis' table titles.
#' 
#' @examples get_table_id(name="employment")
#' 
#' @returns A dataframe of 'nomis' table codes and names, as strings, for all 'nomis' tables containing the selected 'name' in their title. 
#' 
#' @export

get_table_id <- function(name){
  base_url <- "https://www.nomisweb.co.uk/api/v01/"
  raw_data <- httr::GET(
    paste0(
      base_url,
      "dataset/def.sdmx.json?search=name-*",
      name,
      "*"
    )) %>%
    httr::content()
  assert_function(length(raw_data$structure$keyfamilies)==2L,"No tables with chosen name")
  
  num_files=length(raw_data$structure$keyfamilies$keyfamily)
  message(paste0(num_files," table names contain your selected character string"))
  d_rows <- data.frame()
  for (i in seq_along(raw_data$structure$keyfamilies$keyfamily)) {
    d_row <- data.frame(dn = i,
                        n = raw_data$structure$keyfamilies$keyfamily[[i]]$id,
                        v = raw_data$structure$keyfamilies$keyfamily[[i]]$name$value)
    d_rows <- dplyr::bind_rows(d_rows, d_row)
  }
  return(d_rows)
}
