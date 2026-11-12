#' @title Get Boundaries Using Area Names
#' 
#' @description
#' Extract a geojson shapefile of the chosen areas at the user-selected resolution. List of all boundaries can be found here: 
#' https://geoportal.statistics.gov.uk/search?q=Boundary&sort=Date%20Created%7Ccreated%7Cdesc
#' 
#' @usage get_boundaries_areaname(boundary, col_name_var, chosen_constituency_list, 
#' base_url = "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services")
#'
#' @param boundary The resolution of constituencies, e.g. Census Output Areas or Westminster Constituencies. Available boundaries can be found here: https://geoportal.statistics.gov.uk/ 
#' @param col_name_var The name of the datafield where the constituency name is held, e.g. PCON22NM for 2022 Parliamentary Constituencies.
#' @param chosen_constituency_list List of chosen constituencies.
#' @param base_url Open geography portal base url
#' 
#' @examples get_boundaries_areaname(boundary="Local_Authority_Districts_December_2022_UK_BGC_V2",
#' col_name_var="LAD22NM",chosen_constituency_list=c("Westminster","Tower Hamlets","County Durham"))
#' 
#' @returns An sf object of the constituencies submitted to the function. If there are no constituencies, return is NULL.
#' @export

get_boundaries_areaname <- function(boundary,
                                    col_name_var,
                                    chosen_constituency_list,
                                    base_url = "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services") {
  
  url_queries <- list(
    "prefix" = "query?",
    "outSR" = 4326,
    "outFields" = "*",
    "f" = "geojson"
  )
  
  max_constituencies_per_query <- 30

  #API query fails if there are too many options in the filter (i.e. too many area names)
  #Therefore, query is broken down into smaller ones for large lists of area names 
  spatial_object <- sf::st_set_crs(sf::st_sf(sf::st_sfc()), value = "WGS84")
  
  num_constituencies <- length(chosen_constituency_list)
  start_ints <- seq(1, num_constituencies, max_constituencies_per_query)
  end_ints <- sapply(start_ints, function (x) min(x + (max_constituencies_per_query - 1), num_constituencies))
  
  for (i in 1: length(start_ints)) {
    url_queries[["where"]] <- build_constituency_query_string(chosen_constituency_list[start_ints[i]: end_ints[i]], col_name_var, "3D")
    url_query_string <- do.call(build_url_query_string, url_queries)
    full_api_link <- sprintf("%s/%s/FeatureServer/0/%s", base_url, boundary, url_query_string)
    
    new_sf <- sf::st_read(full_api_link)
    spatial_object <- rbind(spatial_object, new_sf)
  }
  
  if (length(spatial_object) == 1L){
    warning("OUT OF BOUNDS. The selected area name(s) do not exist for your chosen boundary scale (please ensure you haven't included any leading or trailing blank spaces in the area names)")}
  
  return(spatial_object)
}

