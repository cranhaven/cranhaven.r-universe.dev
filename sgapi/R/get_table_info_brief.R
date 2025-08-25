#' @title Key 'nomis' Table Information
#' 
#' @description
#' Retrieve summary information about
#' a given 'nomis' dataset. This is
#' useful as it provides the description 
#' of the dataset and any caveats. 
#' It also returns information about the current status of the data, and when it was last updated.
#' 
#' @importFrom magrittr %>%
#' 
#' @param id A valid 'nomis' table id given as a string, e.g. NM_46_1.
#' @param base_url Base nomis url to query
#' 
#' @examples get_table_info_brief(id="NM_1_1")
#' 
#' @returns A json file containing the DatasetInfo, DatasetMetadata, Dimensions (variables), Dataset Contact, Units from the target 'nomis' table.
#' @export

get_table_info_brief <- function(id, base_url = "https://www.nomisweb.co.uk/api/v01"){
  tryCatch(
    {
      raw_info <- httr::GET(paste0(base_url,
                                  "/dataset/",
                                   id,
                                  ".overview.json?select=DatasetInfo,DatasetMetadata,Dimensions,Codes-workforce,Contact,Units")) %>%
      httr::content()
      assert_function(length(raw_info)==2L,"Chosen Nomis table id does not exist, see column 'id' in sgapi::nomisTables for available table ids")
    return(raw_info)
    },
  error = function(e) {
    message("Chosen Nomis table id does not exist, see column 'id' in sgapi::nomisTables for available table ids")
    return(NULL)
  }
  )

}
