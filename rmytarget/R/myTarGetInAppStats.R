# cran check
conversion_type <- NULL

#' Returns statistics on mobile app events attributed with advertising impressions myTarget by campaigns and banners
#' @param date_from Start date
#' @param date_to End date
#' @param object_type API object type, character value, apply one of campaigns, banners, users
#' @param object_id ID of API object (id campaign or any object)
#' @param attribution Attributing by event time or impression time. Available options: conversion, impression
#' @param conversion_type Conversion type: postclick - postclick, postview - postview, total - total.
#' @param auth R auth object
#' @param token_path Path to directory where you save credential data
#' @param login Your login, or client name in MyTarget account
#'
#' @return tibble with fast statistics
#' @export
#'
#' @examples
#' \dontrun{
#' rt_stat <- myTarGetFastStats()
#' }
myTarGetInAppStats <- 
  function(date_from       = Sys.Date() - 7,
           date_to         = Sys.Date(), 
           object_type     = "campaigns",
           object_id       = NULL, 
           attribution     = c("conversion", "impression"),
           conversion_type = c("postview", "postclick", "total"),
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
      # define function
      f <- switch(object_type,
                  "campaigns" = "myTarGetCampaignList",
                  "banners"   = "myTarGetAdList",
                  "users"     = "myTarGetClientList")
      # load obj
      objects <- do.call(f,
                         list(login = login,
                              token_path = token_path))
      
      object_id <- objects$id
      
      Sys.sleep(1)
    }
    
    
    message("start-loading------------->")
    
    ans <- GET(url = str_interp("https://target.my.com/api/v2/statistics/inapp/${object_type}/day.json"),
               query = list(id              = paste0(object_id, collapse = ","),
                            date_from       = date_from,
                            date_to         = date_to,
                            attribution     = attribution,
                            conversion_type = conversion_type),
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
      unnest_wider('rows') 
    
    
    if ( 'inapps' %in% names(res) ) {
      
      res <- res %>%
        unnest_longer('inapps') %>%
        unnest_wider('inapps') %>%
        select(-'total') %>% 
        filter( !is.na(conversion_type) )
      
    } else {
      
      warning('You dont have any data by your query')
      
    }
    
    return(res)
    
  }