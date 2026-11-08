#' Connect to API and Obtain Access Token 
#' 
#' Prior to attempting this you must have a Client Id, Client 
#' Secret and Refresh Token. The first two are assigned on 
#' an organization level; the latter must be created by
#' someone with an admin role in Acoustic and assigned to you.
#' 
#' Access tokens expire after four hours. Thus, this function should
#' be run each time you utilize the package and may need to be 
#' re-called periodically if you have a session open for a long
#' duration.
#' 
#' It is not recommended that these authentication parameters be 
#' stored directly in your code. There are various methods and 
#' packages available that are more secure; this package does not 
#' require you to use any one in particular.
#' 
#' More information on this available at https://developer.ibm.com/
#' customer-engagement/tutorials/
#' getting-started-oauth-watson-campaign-automation/ .
#' 
#' @param org_client_id Organization's Client Id.
#' @param org_client_secret Organization's Client Secret.
#' @param my_refresh_token Your personal Refresh Token.
#' @param pod_number Pod number is the number in the URL, e.g. 
#' engage1.silverpop.com.
#' 
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "RETRY"
#' @importFrom httr "POST"
#' @importFrom httr "content"
#' @importFrom httr "add_headers"
#' 
#' @return A vector with the session's access token.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' access_token <- acoustic_auth(org_client_id = "abc",
#' org_client_secret = "xyz",
#' my_refresh_token = "123")
#' }


acoustic_auth <- function(org_client_id, org_client_secret, my_refresh_token, pod_number) {
  
  # Store the credentials in a list
  body_parameters <- list(grant_type = "refresh_token",
             client_id = org_client_id,
             client_secret = org_client_secret,
             refresh_token = my_refresh_token)

  # Submit the request
  request <- httr::RETRY("POST",
                        url = paste0("https://api-campaign-us-", pod_number, ".goacoustic.com/oauth/token"),
                        httr::add_headers("Content-Type" = "application/x-www-form-urlencoded"),
                        body = body_parameters,
                        encode = "form",
                        times = 4,
                        pause_min =10,
                        terminate_on = NULL,
                        terminate_on_success = TRUE,
                        pause_cap = 5)
  
  check_request_status(request)
  check_for_faulty_xml(request)
  
  # Get and return the access_token
  request_content <- httr::content(request, "text", encoding = "ISO-8859-1")
  content_json <- jsonlite::fromJSON(request_content, flatten = TRUE)
  access_token <- content_json$access_token
  return(access_token)
}