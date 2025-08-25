#' @title Get ONS data
#' 
#' @description
#' Retrieve data from the Office of National Statistics (ONS) via the Nomis API for a 
#' given dataset. 
#' 
#' @param id a table ID recognised by 'nomis' (e.g. "NM_1_1")
#' @param base_url Nomis base url to query. Default: "https:www.nomisweb.co.uk/api/v01"
#' @param csv_parser_fnc Function for parsing the csv file from Nomis API url. Default: read.csv
#' @param ... Additional parameters used to filter and aggregate the data retrieved from ONS. 
#'        These are passed to build_url_query_string to build everything in the url after "?". 
#'        See "https://www.nomisweb.co.uk/api/v01/help for information on the options available
#' 
#' @examples 
#' # Pull all data from the "Jobseeker's Allowance" dataset
#' \dontrun{get_ons_table("nm_1_1")}
#' # Filter "Jobseeker's Allowance" dataset and select columns to output ('select')
#' get_ons_table("nm_1_1", geography = "TYPE480", time = "latest", measures = 20100, item = 1, 
#'               select = c("geography_name", "sex_name", "obs_value"))
#' 
#' # Aggregate statistics using 'rows' and 'cols'
#' get_ons_table("nm_1_1", 
#'               geography = "TYPE480", time = "latest", measures = 20100, item = 1, 
#'               select = c("geography_name", "sex_name", "obs_value"),
#'               rows = c("geography_name"), cols = c("sex_name"))
#'
#' @returns A dataframe containing the data downloaded from ONS' Nomis API
#' @export

get_ons_table <- function(id, base_url = "https://www.nomisweb.co.uk/api/v01", csv_parser_fnc = utils::read.csv, ...) {
  query_string <- build_url_query_string(...)
  nomis_url <- sprintf("%s/dataset/%s.data.csv%s", base_url, id, query_string)

  message("Querying Nomis API -> ", nomis_url)

  nomis_conn <- url(nomis_url)
  nomis_table <- csv_parser_fnc(nomis_conn)

  if ((length(nomis_table) == 1) && names(nomis_table) == "X..DOCTYPE.html.") {
    stop("Nomis id does not exist. Use sgapi::list_tables() for full list of ids")
  }

  if (nrow(nomis_table) == 25000) {
    warning("Query has been truncated to 25,000 rows - providing a nomis uid (i.e. uid = '0x...') will return the full table. See 'API Key, your Unique ID and Signatures' section at https://www.nomisweb.co.uk/api/v01/help for further details")  
  }
  
  return(nomis_table)
}

