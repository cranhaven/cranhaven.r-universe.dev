#' Get Metadata on Client Ids for a List of Product Ids
#' 
#' Returns metadata (what Revulytics calls "properties") for every 
#' Client Id installed during user-provided date range for all product
#' Ids in a list.
#' 
#' It is not recommended that your username be stored directly in your
#' code. There are various methods and packages available that are more 
#' secure; this package does not require you to use any one in particular.
#' 
#' This API call can only return 200 Client Ids at a time. It will take a
#' long time to execute if you have many Client Ids, as the function will
#' submit requests to the API repeatedly; this may even result in a timeout
#' error from the server. In order to provide data for troubleshooting 
#' this function will write a message to the console after each call. 
#' It is recommended that you divert the console output to a text file. 
#' You can do this in multiple ways, including with the sink function (see
#' example for how to do this).
#' 
#' For the same reason you are encouraged to break your request into
#' smaller chunks using the install dates and/or splitting up your
#' product Ids.
#' 
#' @param rev_product_ids A vector of revulytics product id's for which
#' you want active user data.
#' @param rev_session_id Session ID established by the connection to
#' Revulytics API. This can be obtained with revulytics_auth().
#' @param rev_username Revulytics username.
#' @param product_properties_df Data frame with available properties 
#' for all product ids. Can obtain with the get_product_properties function.
#' @param desired_properties The property names of the metadata you want
#' to collect.
#' @param installed_start_date Date object for the starting date of 
#' product installations.
#' @param installed_end_date Date object for the ending date of 
#' product installations.
#' 
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom purrr "map_dfr"
#' @importFrom purrr "map_dfc"
#' @import httr
#' @import jsonlite
#' @importFrom tidyselect "all_of"
#' @importFrom tidyr "pivot_longer"
#' @importFrom tibble "tibble"
#' 
#' @return Data frame with selected properties for each Client Id.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' rev_user <- "my_username"
#' rev_pwd <- "super_secret"
#' product_ids_list <- c("123", "456", "789")
#' session_id <- revulytics_auth(rev_user, rev_pwd)  
#' product_properties <- get_product_properties(product_ids_list, session_id, rev_user)
#' sink("output_filename.txt") 
#' sink(stdout(), type = "message")
#' client_metadata <- get_client_metadata(product_ids_list, session_id, rev_user,
#' product_properties, c("Property1", "Property2"), start_date, end_date)
#' sink()
#' }


get_client_metadata <- function(rev_product_ids, rev_session_id, rev_username, product_properties_df, desired_properties, installed_start_date, installed_end_date) {
  
  product_df_base <- tibble(revulytics_product_id = character(), client_id = character(),
                            property_friendly_name = character(), property_value = character())
  
  get_one_product_metadata <- function(product_iter) {
    
    message(paste0("Starting product id ", product_iter))
  
    custom_property_names <- product_properties_df %>%
      filter(.data$revulytics_product_id == product_iter, .data$property_friendly_name %in% desired_properties) %>%
      select(.data$property_name) %>%
      pull()
    
    custom_property_friendly_names <- product_properties_df %>%
      filter(.data$revulytics_product_id == product_iter, .data$property_friendly_name %in% desired_properties) %>%
      select(.data$property_friendly_name) %>%
      pull()
    
    i <- 0
    
    keep_going <- TRUE
    
    while (keep_going == TRUE) {
      message(paste0("iteration ", i))
      
      i <- i + 1
      
      body <- paste0("{\"user\":\"",rev_username,
                     "\",\"sessionId\":\"",
                     rev_session_id,
                     "\",\"productId\":",
                     product_iter,
                     ",\"startAtClientId\":",
                     jsonlite::toJSON(ifelse(exists("content_json"), content_json$nextClientId, NA_character_), auto_unbox = TRUE),
                     paste0(",\"globalFilters\":{\"dateInstalled\":{\"type\":\"dateRange\",\"min\":\"",
                            installed_start_date,
                            "\",\"max\":\"",
                            installed_end_date,
                            "\"}},"),
                     paste0("\"properties\":", jsonlite::toJSON(array(c(custom_property_names)), auto_unbox = TRUE), "}"),
      sep = "")
      
      

      request <- httr::RETRY("POST",
                            url = "https://api.revulytics.com/reporting/clientPropertyList",
                            body = body,
                            encode = "json",
                            times = 4,
                            pause_min = 10,
                            terminate_on = NULL,
                            terminate_on_success = TRUE,
                            pause_cap = 5)
      
      check_status(request)
      
      request_content <- httr::content(request, "text", encoding = "ISO-8859-1")
      content_json <- jsonlite::fromJSON(request_content, flatten = TRUE)
      
      message(paste0("nextClientId = ", content_json$nextClientId))
      
      build_data_frame <- function(c){
        properties <- as.data.frame(content_json$results[c])
      }
      
      product_df <- purrr::map_dfc(1:length(content_json$results), build_data_frame)
      names(product_df)[2:length(content_json$results)] <- c(custom_property_friendly_names)
      product_df2 <- product_df %>%
        tidyr::pivot_longer(tidyselect::all_of(custom_property_friendly_names), names_to = "property_friendly_name", values_to = "property_value") %>%
        mutate(property_value = if_else(.data$property_value == "<NULL>" | .data$property_value == "", NA_character_, .data$property_value),
               revulytics_product_id = product_iter) %>%
        rename(client_id = .data$clientId) %>%
        select(.data$revulytics_product_id, .data$client_id, .data$property_friendly_name, .data$property_value) %>%
        filter(!is.na(.data$property_value))

      product_df_base <- bind_rows(product_df2, product_df_base)

      keep_going <- ifelse(content_json$reachedEnd == "FALSE", TRUE, FALSE)
      
    }
    
    return(product_df_base)
  
  }
  
  all_products_df <- purrr::map_dfr(rev_product_ids, get_one_product_metadata)
  return(all_products_df)
  
}
