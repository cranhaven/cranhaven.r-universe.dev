#' Get Active Users by Product ID and Various Date Spans
#' 
#' For a given period of time (a day, week, or month) Revulytics' API
#' summarizes and returns the number of active users. With this function
#' you can return daily, weekly, or monthly active users for multiple 
#' product ids. 
#' 
#' You can specify a start and end date but Revulytics does not store
#' an indefinite period of historical data. In my experience this is 
#' three years but I do not know if this varies on a product or client
#' level.
#' 
#' It is not recommended that your username be stored directly in your
#' code. There are various methods and packages available that are more 
#' secure; this package does not require you to use any one in particular.
#' 
#' @param rev_product_ids A vector of revulytics product id's for which
#' you want active user data.
#' @param rev_date_type Level of aggregation, Revulytics will accept
#' "day", "week", or "month".
#' @param rev_start_date Date formatted YYYY-MM-DD. Revulytics may give
#' an error if you try to go back too far.
#' @param rev_end_date Date formatted YYYY-MM-DD.
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
#' @return Data frame with active users for each product id and
#' unique date within the range
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' rev_user <- "my_username"
#' rev_pwd <- "super_secret"
#' product_ids_list <- c("123", "456", "789")
#' start_date <- lubridate::floor_date(Sys.Date(), unit = "months") - months(6)
#' end_date <- Sys.Date() - 1
#' session_id <- revulytics_auth(rev_user, rev_pwd)  
#' monthly_active_users <- get_active_users(product_ids_list,
#' "month",
#' start_date,
#' end_date,
#' session_id,
#' rev_user)
#' }


get_active_users <- function(rev_product_ids, rev_date_type, rev_start_date, rev_end_date, rev_session_id, rev_username) {
  
  . <- NA # prevent variable binding note for the dot in the get_by_product function
  
  get_by_product <- function(x, rev_date_type) {
    request_body <- list(
      user = rev_username,
      sessionId = rev_session_id,
      productId = x,
      startDate = rev_start_date,
      stopDate = rev_end_date,
      dateSplit = rev_date_type,
      clientStatus = array("active")
    )
    

    request <- httr::RETRY("POST",
                           url = "https://api.revulytics.com/reporting/generic/dateRange?responseFormat=raw",
                     body = request_body,
                     encode = "json",
                     times = 4,
                     pause_min = 10,
                     terminate_on = NULL,
                     terminate_on_success = TRUE,
                     pause_cap = 5)
    
    check_status(request)
    
    request_content <- httr::content(request, "text", encoding = "ISO-8859-1")
    content_json <- jsonlite::fromJSON(request_content, flatten = TRUE)
    
    iteration_df <- as.data.frame(unlist(content_json$results)) %>% cbind(rownames(.)) %>%
      dplyr::rename(active_user_date = 2, active_users = 1) %>%
      dplyr::mutate(active_user_date = as.Date(substr(.data$active_user_date, 1, 10)),
             revulytics_product_id = x)
    rownames(iteration_df) <- NULL
    return(iteration_df)
  }
  
  df <- purrr::map_dfr(rev_product_ids, get_by_product, rev_date_type)
  
  return(df)
}

