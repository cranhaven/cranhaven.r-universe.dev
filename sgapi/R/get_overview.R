#' @title Table Overview
#' 
#' @description 
#' Retrieve dataset overview for a valid 'nomis' table id. 
#' Returned object includes description of the dataset, last update date, contact for the data. 
#' It also extracts all of the available instances of the available table dimensions, which can then be used to filter the dataset 'nomis' table.
#' 
#' @importFrom magrittr %>%
#' 
#' @param id A valid 'nomis' id.
#' 
#' @examples 
#' get_overview(id="NM_1_1")
#' 
#' @returns An object with overview information of chosen data set. Object has the structure of the extracted JSON object.
#' @export

get_overview <- function(id) {
  tryCatch(
    {
      base_url = "https://www.nomisweb.co.uk/api/v01/"
      raw_data <- httr::GET(paste0(base_url,
                       "dataset/",
                       id,
                       ".overview.json")) %>%
                       httr::content()
      assert_function(length(raw_data)==2L,"Chosen Nomis table id does not exist, see column 'id' in sgapi::nomisTables for available table ids. If table is recorded in sgapi::nomisTables, contact nomis to check for any know issues with the dataset")
      return(raw_data)
    },
    error = function(e) {
      message("Chosen Nomis table id does not exist, see column 'id' in sgapi::nomisTables for available table ids. If table is recorded in sgapi::nomisTables, contact nomis to check for any know issues with the dataset")
      return(NULL)
  }
  )
}
