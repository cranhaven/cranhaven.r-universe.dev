# cran checker 
clicks <- NULL
shows <- NULL
timestamp <- NULL
id <- NULL

#' Returns Basic Statistics in Real Time
#' @description returns basic statistics on advertising objects in real time, without taking into account the filtering of incorrect traffic. The summary statistics can vary significantly.
#' @param object_type API object type, character value, apply one of campaigns, banners, users
#' @param object_id ID of API object (id campaign or any object)
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
myTarGetFastStats <- 
  function(object_type = "campaigns",
           object_id   = NULL, 
           auth        = NULL,
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
    ans <- GET(url = str_interp("https://target.my.com/api/v2/statistics/faststat/${object_type}.json"),
               query = list(id = paste0(object_id, collapse = ",")),
               add_headers(Authorization = paste0("Bearer ",auth$access_token)))
    
    # get answer content
    temp_all_data <- content(ans, as = "parsed")
    
    # check for error
    if ( !is.null(temp_all_data$error) ) {
      stop( temp_all_data$error$code, ": ", temp_all_data$error$message)
    }
    
    # result object name
    obj <- switch (object_type,
             "campaigns" = "campaigns",
             "banners" = "banners",
             "users" = "advertisers"
    )
    
    message("Data actual time: ", temp_all_data$last_seen_msg_time$string)
    
    res_data <- tibble(data = temp_all_data[[obj]]) %>% 
                unnest_wider('data') %>% 
                mutate(id = names(temp_all_data[[obj]])) %>% 
                unnest_wider('minutely')
    
    res_clicks <- res_data %>% 
                  unnest_longer('clicks') %>% 
                  select('id', 'timestamp', 'clicks') %>% 
                  mutate(
                    timestamp = as.POSIXct(timestamp, origin = '1970-01-01')
                  ) %>% 
                  group_by(id, timestamp) %>% 
                  summarise(clicks = sum(clicks, na.rm = TRUE))
                  
    res_shows <- res_data %>% 
                  unnest_longer('shows') %>% 
                  group_by(id) %>% 
                  summarise(shows = sum(shows, na.rm = TRUE))
    
    res <- left_join(res_shows, res_clicks) %>% 
           select(id, timestamp, shows, clicks) %>% 
           mutate(id = as.integer(id))
    
    return(res)
    
  }