#' @title Get 'nomis' Table 
#' 
#' @description
#' Retrieve a 'nomis' table, containing the columns of interest, using a given table ID. 
#' Option to apply filters to the query, and to circumnavigate the limit on number of rows returned using your 'nomis' uid.
#' 
#' @import readr
#' 
#' @param id a table ID recognised by 'nomis' (e.g. "NM_1_1")
#' @param options a list of paramaters to pass to the API query.
#' @param selection a vector of column names to return. NULL returns all. Defaults to NULL.
#' @param uid Unique 'nomis' identifier to enable larger 'nomis' queries - https://www.nomisweb.co.uk/. Defaults to NULL.
#' 
#' @examples 
#' get_table(id="NM_1_1", options = list("geography" = "TYPE480", "time" = "latest"))
#' get_table(id="NM_1002_1", options = list("geography" = "TYPE265", "time" = "latest"),
#' selection = "GEOGRAPHY_NAME,C_AGE_NAME,OBS_VALUE",uid=NULL)
#' 
#' @returns A tidy dataframe of selected 'nomis' table with the selected parameters and user filters applied.
#' @export

get_table <- function(id,
                     options,
                     selection = NULL,
                     uid = NULL){
  baseUrl = "https://www.nomisweb.co.uk/api/v01/"
  
  #assertFunction(is.null(layers), "selectedBoundary is not a valid boundary, 
  #see https://geoportal.statistics.gov.uk/ for available boundaries")
  
  if (!is.null(uid)){
    uid_link=paste0("&uid=",uid)
  }
  else{
    uid_link=""
  }
  queries <- c()
  for(i in names(options)){
    if(i=="time"){new_i="date"}
    else{new_i = i}
    query <- paste0(new_i,"=", options[[i]])
    queries <- c(queries, query)
  }
  total_queries <- paste0(queries, collapse = "&")
  
  if(!is.null(selection)){
    select_query <- paste0(selection, collapse = ",")
    raw_data <- httr::GET(paste0(
      baseUrl,
      "dataset/",
      id,
      ".data.csv?",
      total_queries,
      "&select=",
      select_query,
      uid_link))
  }else{
    raw_data <- httr::GET(paste0(
      baseUrl,
      "dataset/",
      id,
      ".data.csv?",
      total_queries,
      uid_link))
  }
  assert_function(raw_data$status_code>=400L, paste0("API has failed, review the filters applied. The status code is: ",raw_data$status_code))
  output_data <- httr::content(raw_data)
  assert_function(length(output_data)==2L,"API has failed, review the filters applied, and check that the table id is correct")
  if (nrow(output_data)==25000){warning("Query has been truncated at 25,000 rows. Use a nomis uid to return the full table")}
  return(output_data)
}


