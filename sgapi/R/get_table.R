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
#' @param base_url Nomis base url to query
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
                      uid = NULL,
                      base_url = "https://www.nomisweb.co.uk/api/v01"){
  
  if (!is.null(uid)){
    options[["uid"]] <- uid
  }
  
  names(options)[names(options) == "time"] <- "date"

  if (!is.null(selection)) {
    options[["select"]] <- selection
  }

  query_string <- do.call(build_url_query_string, options)
  nomis_url <- sprintf("%s/dataset/%s.data.csv%s", base_url, id, query_string)

  raw_data <- httr::GET(nomis_url)
  
  assert_function(raw_data$status_code>=400L, paste0("API has failed, review the filters applied. The status code is: ",raw_data$status_code))
  output_data <- httr::content(raw_data)
  assert_function(length(output_data)==2L,"API has failed, review the filters applied, and check that the table id is correct")
  if (nrow(output_data)==25000){warning("Query has been truncated at 25,000 rows. Use a nomis uid to return the full table")}
  return(output_data)
}


