#' @title Get Boundaries Using Area Names
#' 
#' @description
#' Extract a geojson shapefile of the chosen areas at the user-selected resolution.
#' 
#' @usage get_boundaries_areaname(boundary,col_name_var,chosen_constituency_list)
#' 
#' @param boundary The resolution of constituencies, e.g. Census Output Areas or Westminster Constituencies. Available boundaries can be found here: https://geoportal.statistics.gov.uk/ 
#' @param col_name_var The name of the datafield where the constituency name is held, e.g. PCON22NM for 2022 Parliamentary Constituencies.
#' @param chosen_constituency_list List of chosen constituencies.
#' 
#' @examples get_boundaries_areaname(boundary="Local_Authority_Districts_December_2022_UK_BGC_V2",
#' col_name_var="LAD22NM",chosen_constituency_list=c("Westminster","Tower Hamlets","County Durham"))
#' 
#' @returns An sf object of the constituencies submitted to the function. If there are no constituencies, return is NULL.
#' @export

get_boundaries_areaname <- function(boundary,
                                    col_name_var,
                                    chosen_constituency_list) {
  
  assert_function(grepl("\\s",boundary),"Boundary must be not contain any spaces, see https://geoportal.statistics.gov.uk/search?q=Boundary&sort=Date%20Created%7Ccreated%7Cdesc for available boundaries")
  layer=0
  output_fields="*"
  
  base_url = "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/"
  api_list_constituencies = ''
  
  num_constituencies <- length(chosen_constituency_list)
  tryCatch(
    {
  #API query fails if there are too many options in the filter (i.e. too many area names)
  #Therefore, query is broken down into smaller ones for large lists of area names 
  if (num_constituencies<=30){
  for (i in 1:length(chosen_constituency_list)) {
    if (i < length(chosen_constituency_list)) {
      api_list_constituencies <- paste0(api_list_constituencies,gsub("\\s","%20",chosen_constituency_list[i]),"'%20OR%20",col_name_var,"%20%3D%20'")}
    else {
      api_list_constituencies <- paste0(api_list_constituencies,gsub("\\s","%20",chosen_constituency_list[i]),"')%20")
    }
  }
  
  full_api_link <-paste0(
    base_url,
    boundary,
    "/FeatureServer/",
    layer,
    "/query?where=%20(",
    col_name_var,
    "%20%3D%20'",
    api_list_constituencies,
    "&",
    "outFields=",
    output_fields,
    "&outSR=4326&f=geojson"
  )

  spatial_object <- sf::st_read(full_api_link)
  }
  else {
    #For large query loop through smaller constituent queries
    spatial_object <- sf::st_set_crs(sf::st_sf(sf::st_sfc()), value = "WGS84")
    num_loops = ceiling(num_constituencies/30)
    for (n in 1:num_loops){
      api_list_constituencies = ''
      start_int=((n-1)*30)+1
      if (n<num_loops){
          end_int=((n-1)*30)+30
      }
      else {end_int=num_constituencies}
      for (i in start_int:end_int) {
        if (i < end_int) {
          api_list_constituencies <- paste0(api_list_constituencies,gsub("\\s","%20",chosen_constituency_list[i]),"'%20OR%20",col_name_var,"%20%3D%20'")}
        else {
          api_list_constituencies <- paste0(api_list_constituencies,gsub("\\s","%20",chosen_constituency_list[i]),"')%20")
        }
      }
      
      full_api_link <-paste0(
        base_url,
        boundary,
        "/FeatureServer/",
        layer,
        "/query?where=%20(",
        col_name_var,
        "%20%3D%20'",
        api_list_constituencies,
        "&",
        "outFields=",
        output_fields,
        "&outSR=4326&f=geojson"
      )
      
      new_sf <- sf::st_read(full_api_link)
    
    #Bind together spatial objects of smaller queries 
    spatial_object <- rbind(spatial_object, new_sf)
    }
  }
  if(length(spatial_object)==1L){warning("OUT OF BOUNDS. The selected area name(s) do not exist for your chosen boundary scale (please ensure you haven't included any leading or trailing blank spaces in the area names)")}
  return(spatial_object)
  },
  error = function(e) {
    message("Error in column name for chosen boundary, or in the list of area names. Check your chosen boundary scale on https://geoportal.statistics.gov.uk/")
    return(NULL)
  }
  
  )

}

