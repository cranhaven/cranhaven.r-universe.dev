#' Set up Onboard API keys and URL in system environment
#' @description
#' 
#' Set the Onboard API URL and API keys in the system environment.
#'  
#' @param api_type Provide the API client name.
#' 
#' @return No return value, sets API url and API key in the system environment.
#'   
#' @export
#' 
api.setup <- function(api_type = 'prod') {
  
  if(!(api_type %in% c('prod','dev','rtem'))){
    stop("Please use 'prod', 'dev', or 'rtem' for api_type")
  }
  
  api_url <- dplyr::case_when(
    api_type == 'prod' ~ 'https://api.onboarddata.io',
    api_type == 'dev' ~ 'https://devapi.onboarddata.io',
    api_type == 'rtem' ~ 'https://api.ny-rtem.com'
  )
  
  api_name <- paste0('api_key_', api_type)
  
  if (Sys.getenv("RSTUDIO") == "1"){
    api_key <- rstudioapi::askForSecret(
      name = api_name,
      message = 'Enter your API key here',
      title = "Onboard API Key")
  } else {
    api_key <- readline(prompt = "Enter your Onboard API key:")
  }
  
  Sys.setenv('api_url' = api_url)
  Sys.setenv('api_key' = api_key)
}

#' Access API keys and URL from System Environment
#' @description 
#' 
#' Returns the API url and API key.
#' 
#' @return A named list of API information, containing elements 'url' and 'key'.
#' 
api.access <- function(){
  api_url <- Sys.getenv('api_url')
  api_key <- Sys.getenv('api_key')
  
  if(api_url == '' | api_key == ''){
    stop('API credentials not set correctly.')
  } else {
    return(list(
      'url' = api_url,
      'key' = api_key
    ))
  }
}

#' Check the status of your connection with the Onboard API
#' 
#' @description 
#' Provides a status code and message for the API connection.
#' 
#' @return A character string of the API server status and message.
#'
#' @export
api.status <- function() {
  api_data <- api.access()
  
  request <- GET(url = api_data$url,
                 add_headers(`X-OB-Api` = api_data$key))

  return(httr::http_status(request$status_code)$message)
}