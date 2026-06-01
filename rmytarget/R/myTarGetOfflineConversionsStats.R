#' Returns Statistics on Events Attributed With 'myTarget' Ad Impressions From Offline Conversion Lists for Campaigns
#'
#' @param date_from Start date
#' @param date_to End date
#' @param object_id Campaigns IDs
#' @param auth R auth object
#' @param token_path Path to directory where you save credential data
#' @param login Your login, or client name in MyTarget account
#'
#' @return tibble with offline conversions statistics
myTarGetOfflineConversionsStats <- 
  function(date_from       = Sys.Date() - 7,
           date_to         = Sys.Date(), 
           object_id       = NULL, 
           auth            = NULL,
           login       = getOption('rmytarget.login'), 
           token_path  = myTarTokenPath()
  ) {
    
    start_time <- Sys.time()
    
    message("Authorize.")
    
    if (is.null(auth)) {
      auth <- myTarAuth(login = login, token_path = token_path)
    }
    
    # if null obj id
    if ( is.null(object_id)) {
      message("Loading object list.")

      # load obj
      objects <- do.call("myTarGetCampaignList",
                         list(login = login,
                              token_path = token_path))
      
      object_id <- objects$id
      
      Sys.sleep(1)
    }
    
    ans <- GET(url = str_interp("https://target.my.com/api/v2/statistics/offline_conversions/campaigns/day.json"),
               query = list(id              = paste0(object_id, collapse = ","),
                            date_from       = date_from,
                            date_to         = date_to),
               add_headers(Authorization = paste0("Bearer ",auth$access_token)))
    
    # get answer content
    temp_all_data <- content(ans, as = "parsed")
    
    # check for error
    if ( !is.null(temp_all_data$error) ) {
      stop( temp_all_data$error$code, ": ", temp_all_data$error$message)
    }
    
    res <- tibble(data = temp_all_data$items) %>%
           unnest_wider('data') %>%
           unnest_longer('rows') %>%
           unnest_wider('rows') %>% 
           select(-'total')
    
    return(res)
}
