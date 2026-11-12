#' @title Lookup Between Boundary Scales
#' 
#' @description
#' Extract a lookup table between two boundary scales from 'ONS Open Geography' portal, 
#' e.g. get a lookup between Regions and Parliamentary constituencies.
#' 
#' @param lookup_table A valid ONS lookup table
#' @param col_name_1 Field in ONS table containing the constituency code of the smaller scale resolution.
#' @param col_name_2 Field in ONS table containing the constituency code of the larger scale resolution.
#' @param col_name_3 Field in ONS table containing the constituency name of the smaller scale resolution.
#' @param col_name_4 Field in ONS table containing the constituency name of the larger scale resolution.
#' @param base_url Open geography portal base url
#' 
#' @examples 
#' \dontrun{
#' get_table_link_lookup(lookup_table="LAD22_CTY22_EN_LU",col_name_1="LAD22CD",
#' col_name_2="CTY22CD",col_name_3="LAD22NM",col_name_4="CTY22NM")
#' }
#' 
#' @returns A tidy dataframe, providing a lookup between two chosen boundary resolutions.
#' @export

get_table_link_lookup <- function(lookup_table, col_name_1, col_name_2, col_name_3, col_name_4, 
                                  base_url = "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services") {
  
  assert_function(grepl("\\s", lookup_table),"Boundary must be not contain any spaces, see https://geoportal.statistics.gov.uk/search?q=Boundary&sort=Date%20Created%7Ccreated%7Cdesc for available boundaries")
  
  api_link <- sprintf("%s/%s/FeatureServer/0", base_url, lookup_table)
  
  url_queries <- list(
    "prefix" = "query?",
    "where" = "1%3D1",
    "returnGeometry" = "false",
    "resultType" = "standard",
    "returnCountOnly" = "false",
    "outSR" = 4326,
    "f" = "json"
  )

  if ((col_name_1 == col_name_2) & (col_name_3 == col_name_4)) {
    url_queries[["outFields"]] <- c(col_name_1, col_name_3)
  } else {
    url_queries[["outFields"]] <- c(col_name_1, col_name_2, col_name_3, col_name_4)
  }

  url_query_string <- do.call(build_url_query_string, url_queries)
  ons_url <- sprintf("%s/%s", api_link, url_query_string)

  raw_ons_data <- httr::content(httr::GET(ons_url))
  features <- raw_ons_data$features

  # API allows a maximum of 32,000 'features' to be fetched in 
  # each call. Therefore, if we don't have all the data then we
  # loop, applying an offset, until we have it all
  result_offset <- 0
  while (length(raw_ons_data$features) > 0) {
    result_offset <- result_offset + 32000
    curr_url <- sprintf("%s&%s", ons_url, paste("resultOffset", result_offset, sep = "="))
    raw_ons_data <- httr::content(httr::GET(curr_url))
    features <- c(features, raw_ons_data$features)
  }
  
  ons_data <- data.frame(
    con_name_large = sapply(features, function(x) x[["attributes"]][[col_name_1]]),
    con_name_small = sapply(features, function(x) x[["attributes"]][[col_name_2]]),
    con_code_large = sapply(features, function(x) x[["attributes"]][[col_name_3]]),
    con_code_small = sapply(features, function(x) x[["attributes"]][[col_name_4]])
  )

  return(dplyr::distinct(ons_data))
}


