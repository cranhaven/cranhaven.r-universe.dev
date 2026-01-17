#' Get All Categories and Events for a List of Product Ids
#' 
#' Returns all of the unique categories and events (basic and advanced)
#' for each product id.
#' 
#' It is not recommended that your username be stored directly in your
#' code. There are various methods and packages available that are more 
#' secure; this package does not require you to use any one in particular.
#' 
#' @param rev_product_ids A vector of revulytics product id's for which
#' you want active user data.
#' @param rev_session_id Session ID established by the connection to
#' Revulytics API. This can be obtained with revulytics_auth().
#' @param rev_username Revulytics username.
#' 
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom purrr "map_dfr"
#' @import httr
#' @import jsonlite
#' 
#' @return Data frame with categories, events and event type by product id.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' rev_user <- "my_username"
#' rev_pwd <- "super_secret"
#' product_ids_list <- c("123", "456", "789")
#' session_id <- revulytics_auth(rev_user, rev_pwd)  
#' category_event <- get_categories_and_events(product_ids_list, session_id, rev_user)
#' }

get_categories_and_events <- function(rev_product_ids, rev_session_id, rev_username) {

  get_by_product <- function(x) {
  
    request_body <- list(
      user = rev_username,
      sessionId = rev_session_id,
      productId = x,
      showEvents = array("all")
    )

    
    body <- jsonlite::toJSON(request_body, auto_unbox = TRUE)
    request <- httr::RETRY("POST",
                          url = "https://api.revulytics.com/eventTracking/listEventNames",
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
    
    parse_json_into_df <- function(x){
      category_name <- content_json$results$category[x]
      event_name <- as.data.frame(content_json$results$categoryEventNames[x]) %>%
        cbind(category_name) %>%
        mutate(category_name = as.character(.data$category_name))
    }
    
    category_event <- purrr::map_dfr(1:length(content_json$results$category), parse_json_into_df) %>%
      mutate(event_type = case_when(
                            .data$advanced ~ "ADVANCED",
                            .data$basic ~ "BASIC",
                            TRUE ~ "INACTIVE"),
             date_first_seen = as.Date(.data$dateFirstSeen),
             revulytics_product_id = x) %>%
      select(.data$revulytics_product_id, .data$category_name, .data$eventName, .data$event_type, .data$date_first_seen) %>%
      rename(event_name = .data$eventName)
    
  }
  
  category_event_by_prod <- purrr::map_dfr(rev_product_ids, get_by_product)
  return(category_event_by_prod)
}

