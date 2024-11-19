#' @title Keyword Search of 'nomis'
#' 
#' @description 
#' Function to return dataframe of all 'nomis' tables, 
#' and their corresponding codes and descriptions, which have the chosen keyword.
#' 
#' @import xml2
#' 
#' @param usr_keyword Keyword to search 'nomis' tables for, e.g. "religion", "employment", "housing"
#' 
#' @examples get_keyword_table_id(usr_keyword="passports")
#' @returns A tidy dataframe of all nomis tables and their ids, which contain the chosen keyword.
#' @export

get_keyword_table_id <- function(usr_keyword){
  raw_data <- httr::GET(
    paste0("https://www.nomisweb.co.uk/api/v01/dataset/def.sdmx.json?search=keyword-*",
      usr_keyword,
      "*"
    )) %>%
    httr::content()
  
  
  #validation check
  assert_function(length(raw_data$structure$keyfamilies)==2L,"No tables with chosen keyword")
  
  num_files=length(raw_data$structure$keyfamilies$keyfamily)
  message(paste0(num_files," tables have your selected keyword"))
  d_rows <- data.frame()
  for (i in seq_along(raw_data$structure$keyfamilies$keyfamily)) {
      d_row <- data.frame(dn = i,
                          n = raw_data$structure$keyfamilies$keyfamily[[i]]$id,
                          v = raw_data$structure$keyfamilies$keyfamily[[i]]$name$value)
      d_rows <- dplyr::bind_rows(d_rows, d_row)
  }
  return(d_rows)
}
  