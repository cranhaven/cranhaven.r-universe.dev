#' @title Lookup Between Boundary Scales
#' 
#' @description
#' Extract a lookup table between two boundary scales from 'ONS Open Geography' portal, 
#' e.g. get a lookup between Regions and Parliamentary constituencies.
#' 
#' @import dplyr
#' @importFrom magrittr %>%
#' 
#' @param lookup_table A valid ONS lookup table
#' @param col_name_1 Field in ONS table containing the constituency code of the smaller scale resolution.
#' @param col_name_2 Field in ONS table containing the constituency code of the larger scale resolution.
#' @param col_name_3 Field in ONS table containing the constituency name of the smaller scale resolution.
#' @param col_name_4 Field in ONS table containing the constituency name of the larger scale resolution.
#' 
#' @examples 
#' \dontrun{
#' get_table_link_lookup(lookup_table="LAD22_CTY22_EN_LU",col_name_1="LAD22CD",
#' col_name_2="CTY22CD",col_name_3="LAD22NM",col_name_4="CTY22NM")
#' }
#' 
#' @returns A tidy dataframe, providing a lookup between two chosen boundary resolutions.
#' @export

get_table_link_lookup <- function(lookup_table,col_name_1,col_name_2,col_name_3,col_name_4){
  
  assert_function(grepl("\\s",lookup_table),"Boundary must be not contain any spaces, see https://geoportal.statistics.gov.uk/search?q=Boundary&sort=Date%20Created%7Ccreated%7Cdesc for available boundaries")
  api_link <- paste0("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/",lookup_table,"/FeatureServer/0/")
  queries <- "query?where=1%3D1&outFields="

  #Check whether the two scales are the same- this is the case for single constituency lookups
  if ((col_name_1 == col_name_2) & (col_name_3 == col_name_4)){
    api_link_count <- paste0(api_link,queries,col_name_1,",",col_name_3,"&returnGeometry=false&resultType=standard&returnCountOnly=true&outSR=4326&f=json")
  }
  else{
  api_link_count <- paste0(api_link,queries,col_name_1,",",col_name_2,",",col_name_3,",",col_name_4,"&returnGeometry=false&resultType=standard&returnCountOnly=true&outSR=4326&f=json")
  }
  
  #Find the number of rows in the dataset of interes
  x_c <- httr::GET(paste0(
    api_link_count))
  raw_count <- httr::content(x_c)


  #The restful/esri API has a limit of 32,000 rows that it will return.
  #If the table is larger than this you must loop through the API mul;tiple times to extract all of the data
  available_datasets <- data.frame()
  # (re-)setting loop parameters to 0
  loop_bool = "True" 
  loop_num=0
  #Some tables do not produce a response to the counting process above, must use a more rudimentary process for these tables
  if (raw_count$count ==0){
    while (loop_bool == "True"){
      off_set_val = loop_num*32000
    if ((col_name_1 == col_name_2) & (col_name_3 == col_name_4)){
      api_link_full <- paste0(api_link,queries,col_name_1,",",col_name_3,"&returnGeometry=false&resultType=standard&resultOffset=",off_set_val,"&outSR=4326&f=json")
    }
    else{
      api_link_full <- paste0(api_link,queries,col_name_1,",",col_name_2,",",col_name_3,",",col_name_4,"&returnGeometry=false&resultType=standard&resultOffset=",off_set_val,"&outSR=4326&f=json")
    }
    

    x <- httr::GET(paste0(
      api_link_full))
    raw_data <- httr::content(x)
    raw_count <- length(raw_data$features)
    if (raw_count == 0){loop_bool <- "False"}
    
    if (raw_count > 0){
    for(j in 1:length(raw_data$features)){
      
      
      available_datasets <- rbind(available_datasets,
                                 data.frame(
                                   con_name_large = raw_data[["features"]][[j]][["attributes"]][[col_name_1]],
                                   con_name_small = raw_data[["features"]][[j]][["attributes"]][[col_name_2]],
                                   con_code_large = raw_data[["features"]][[j]][["attributes"]][[col_name_3]],
                                   con_code_small = raw_data[["features"]][[j]][["attributes"]][[col_name_4]]
                                 )
                                 
      )
    }}
    loop_num <- loop_num+1
    }
  }
  #If the dataset has fewer than 32,000 rows, can just do the simple extraction
  else if ((raw_count <= 32000) & (raw_count > 0)){
    if ((col_name_1 == col_name_2) & (col_name_3 == col_name_4)){
      api_link_full <- paste0(api_link,queries,col_name_1,",",col_name_3,"&returnGeometry=false&resultType=standard&outSR=4326&f=json")
    }
    else{
      api_link_full <- paste0(api_link,queries,col_name_1,",",col_name_2,",",col_name_3,",",col_name_4,"&returnGeometry=false&resultType=standard&outSR=4326&f=json")
    }
    #extract the data using the API
    x <- httr::GET(paste0(
      api_link_full))
    raw_data <- httr::content(x)
    #Generate a table showing the large constituency names and codes, and the corresponding small constituency names contained within the larger constituency
    #e.g. The large constituency may be the "North East" region, and the small constituencies would be all of the LSOAs within the North East region
  for(j in 1:length(raw_data$features)){
    

    available_datasets <- rbind(available_datasets,
                               data.frame(
                                 con_name_large = raw_data[["features"]][[j]][["attributes"]][[col_name_1]],
                                 con_name_small = raw_data[["features"]][[j]][["attributes"]][[col_name_2]], # No longer needed - removed to save space
                                 con_code_large = raw_data[["features"]][[j]][["attributes"]][[col_name_3]],
                                 con_code_small = raw_data[["features"]][[j]][["attributes"]][[col_name_4]]
                               )
    )
  }}
  #if the dataset is large and the API count is successful, loop through the dataset in 32,000 row extracts until all of the data has been extracted
  else if (raw_count > 32000){
    num_loops = ceiling(raw_count/32000)
    for (n in 1:num_loops){
    off_set_val <- (n-1)*32000
    if ((col_name_1 == col_name_2) & (col_name_3 == col_name_4)){
      api_link_full <- paste0(api_link,queries,col_name_1,",",col_name_3,"&returnGeometry=false&resultType=standard&resultOffset=",off_set_val,"&outSR=4326&f=json")
    }
    else{
      api_link_full <- paste0(api_link,queries,col_name_1,",",col_name_2,",",col_name_3,",",col_name_4,"&returnGeometry=false&resultType=standard&resultOffset=",off_set_val,"&outSR=4326&f=json")
    }
    
    rm(x)
    rm(raw_data)
    x <- httr::GET(paste0(
      api_link_full))
    raw_data <- httr::content(x)
    
    for(j in 1:length(raw_data$features)){
      

      available_datasets <- rbind(available_datasets,
                                 data.frame(
                                   con_name_large = raw_data[["features"]][[j]][["attributes"]][[col_name_1]],
                                   con_name_small = raw_data[["features"]][[j]][["attributes"]][[col_name_2]],
                                   con_code_large = raw_data[["features"]][[j]][["attributes"]][[col_name_3]],
                                   con_code_small = raw_data[["features"]][[j]][["attributes"]][[col_name_4]]
                                 )
      )
    }}
  }
  #remove duplicates
  available_datasetsfin <- available_datasets %>% distinct()
  return(available_datasetsfin)
}


