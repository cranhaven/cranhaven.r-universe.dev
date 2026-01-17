#' Get Daily Property Values for All Clients for a List of Product Ids
#' 
#' Returns the list of daily client properties for all the client Ids
#' installed during a user provided date range for all the Product Ids.
#' 
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
#' @param rev_product_ids A vector of revulytics product id.
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
#' @param daily_start_date Date object for the starting date of desired
#' properties of the product.
#' @param daily_end_date Date object for the ending date of desired 
#' properties of the product.
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
#' daily_client_properties <- get_daily_client_properties(product_ids_list, session_id, rev_user,
#' product_properties, c("Property1", "Property2"), start_date, end_date,
#' daily_start_date = "01-01-2020", daily_end_date = "01-31-2020")
#' sink()
#' }


get_daily_client_properties <- function(rev_product_ids, rev_session_id, rev_username, product_properties_df, desired_properties, installed_start_date, installed_end_date,
                                        daily_start_date,daily_end_date) {
  
  trialpurchase_df <- data.frame()
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
                     paste0("\"properties\":", jsonlite::toJSON(array(c("geography.country")), auto_unbox = TRUE), ","),
                     paste0("\"retDailyData\":{\"startDate\":\"",
                            daily_start_date,
                            "\",\"stopDate\":\"",
                            daily_end_date,
                            "\",\"properties\":",
                            jsonlite::toJSON(array(c(custom_property_names)), auto_unbox = TRUE),
                            "}}"),
                     sep = "")
      
    
      terminal_codes <- list(c("400","401","403","404"))
      
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
      
      colnames(product_df)[1] <- "client_id"
      
      daily_propertytype <- product_df$dailyData
      
      daily_propertytype_flat <- bind_rows(daily_propertytype, .id = "column_label") %>%
        mutate(id = as.numeric(.data$column_label),
               property_date = as.Date(date)) %>%
        select(-.data$column_label, -date)
      
      suppressMessages(
        names(daily_propertytype_flat)[1:length(custom_property_friendly_names)] <- c(custom_property_friendly_names)
      )
      
      client_df <- purrr::map_dfc(1:nrow(product_df), ~ (nrow(product_df[[3]][[.x]]))) %>%
        tidyr::pivot_longer(everything(), names_to = "a", values_to = "b") %>%
        cbind(product_df)%>%
        filter(.data$b!=0)%>%
        select(3)%>%
        mutate(id = row_number())

      client_df_merged <- merge(x = daily_propertytype_flat, y=client_df, by="id", all.x=TRUE)%>%
        tidyr::pivot_longer(cols=-c("id", "property_date", "client_id"), names_to = "property_name", values_to = "property_value")%>%
        select(-id)

      
      final_df <- client_df_merged %>%
        group_by(.data$client_id, .data$property_value) %>%
        slice(which.min(as.Date(.data$property_date,"%Y-%m-%d")))%>%
        ungroup()
     
      trialpurchase_df <- bind_rows(final_df, trialpurchase_df)%>%
        mutate(revulytics_product_id = product_iter)
        
      
      keep_going <- ifelse(content_json$reachedEnd == "FALSE", TRUE, FALSE)
      
    }
    
     return(trialpurchase_df)
    
  }
  
  all_products_df <- purrr::map_dfr(rev_product_ids, get_one_product_metadata)
   return(all_products_df)
   
   
}