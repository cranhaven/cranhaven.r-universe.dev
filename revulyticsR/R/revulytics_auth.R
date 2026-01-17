#' Login and Obtain Revultyics API Session Id
#' 
#' A session must first be established before querying the API.
#' This is done using your Revulytics username and password.
#' 
#' It is not recommended that these values be stored directly 
#' in your code. There are various methods and packages 
#' available that are more secure; this package does not require
#' you to use any one in particular.
#' 
#' @param rev_username Revulytics username.
#' @param rev_password Revultyics password.
#' 
#' @import httr
#' @importFrom magrittr "%>%"
#' 
#' @return A list with details on connection to the Revulytics API.
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
#' }
 
revulytics_auth <- function(rev_username, rev_password) {

  revulytics_login <- httr::RETRY("POST",
                                 url = "https://api.revulytics.com/auth/login",
                                 body = list(user = rev_username,
                                             password = rev_password,
                                             useCookies = FALSE),
                                 encode = "json",
                                 times = 4,
                                 pause_min = 10,
                                 terminate_on = NULL,
                                 terminate_on_success = TRUE,
                                 pause_cap = 5)
  
  check_status(revulytics_login)
  
  rev_session_id <- httr::content(revulytics_login)$sessionId
  
  return(rev_session_id)
}

