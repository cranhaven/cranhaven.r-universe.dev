#' Get Goal Statistics from 'MyTarget'
#'
#' @inheritParams myTarGetStats
#' @param conversion_type Conversion type: postclick - postclick, postview - postview, total - total.
#'
#' @return data frame with goal statics
#' @export
#' @seealso \href{https://target.my.com/adv/api-marketing/doc/stat-v2#goals}{API Documentation}
#'
myTarGetGoalsStats <- 
  function(date_from       = Sys.Date() - 7,
           date_to         = Sys.Date(), 
           object_type     = "campaigns",
           object_id       = NULL, 
           attribution     = c("impression", "conversion"),
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
      
      # match args
      attribution     <- match.arg(attribution)
      conversion_type <- match.arg(conversion_type)

      # result obj
      message("start-loading------------->")
      ans <- GET(url = str_interp("https://target.my.com/api/v2/statistics/goals/${object_type}/day.json"),
                 query = list(date_from       = date_from,
                              date_to         = date_to,
                              id              = paste0(object_id, collapse = ","),
                              attribution     = attribution,
                              conversion_type = conversion_type),
                 add_headers(Authorization = paste0("Bearer ",auth$access_token)))
      
      # get answer content
      temp_all_data <- content(ans, as = "parsed")

      # Check limits
      if ( ! myTarCheckLimits(temp_all_data) ) stop("Limit error")
      
      # check for error
      if ( !is.null(temp_all_data$error) ) {
        stop( temp_all_data$error$code, ": ", temp_all_data$error$message)
      }

      # main data
      result <- tibble( data = temp_all_data$items ) %>%
                unnest_wider('data') %>%
                unnest_longer('rows') %>% 
                unnest_wider('rows') %>% 
                select(-'total')
      
      if ('goals' %in% names(result)) {
        
        # unnest
        result <- unnest_longer(result, 'goals') %>% 
                  unnest_wider('goals')
        
      }
      
    stop_time <- Sys.time()
    message("Success.")
    message("Total time: ", as.numeric(difftime(stop_time, start_time), units = "secs"), " sec.")
    # return total result
    return(result)
  }

