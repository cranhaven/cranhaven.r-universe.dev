#' Get goal list from 'MyTarget'
#' 
#' @inheritParams myTarGetStats
#'
#' @return data frame with goals parameters
#' @export
#' 
#' @seealso \href{https://target.my.com/doc/api/ru/resource/Goals}{Goals API Docymentation}
myTarGetGoalList <-
  function(auth       = NULL, 
           login      = getOption('rmytarget.login'), 
           token_path = myTarTokenPath()) {
    
    
    if (is.null(auth)) {
      auth <- myTarAuth(login = login, token_path = token_path)
    }

    # send request
    ans <- GET(str_interp("${getOption('rmytarget.url')}api/v2/goals.json"), 
               add_headers(Authorization = paste0("Bearer ",auth$access_token)))
    
    # check answer status
    stop_for_status(ans)
    
    # parse answer
    convRaw <- content(ans, "parsed", "application/json")
    
    if ( ! myTarCheckLimits(convRaw) ) stop("Limit error")
    
    # check for error
    if ( !is.null(convRaw$error) ) {
      stop( convRaw$error$code, ": ", convRaw$error$message)
    }
    
    # json to dataframe
    result <- tibble(data = list(goal_group = convRaw)) %>% 
              unnest_longer('data') %>% 
              unnest_longer('data') %>% 
              unnest_wider('data')
    
    # return result
    return(result)
    
  }

