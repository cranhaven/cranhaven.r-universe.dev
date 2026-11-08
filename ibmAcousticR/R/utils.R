#' Get Job Id of Submitted Job
#' 
#' This function is called by other functions that submit jobs to
#' the Acoustic/Silverpop API. It extracts the Job Id from the XML
#' returned by the API call.
#'
#' Job results are available as exports in the Silverpop portal by
#' going to Resources -> Data Jobs.
#' 
#' It is not recommended that these authentication parameters be 
#' stored directly in your code. There are various methods and 
#' packages available that are more secure; this package does not 
#' require you to use any one in particular.
#' 
#' @param request_obj Name of the object returned from API call,
#' should always be "request".
#' @param path XML path to the job id.
#' 
#' @importFrom httr "content"
#' @importFrom XML "xmlParse"
#' @importFrom XML "xmlValue"
#' @importFrom XML "xpathSApply"
#' 
#' @return A vector with the Job Id.
#' 
#' @keywords internal


get_job_id <- function(request_obj, path) {
  # Extract the XML from the request results
  request_content <- httr::content(request_obj, "text", encoding = "ISO-8859-1")
  request_xml <- XML::xmlParse(request_content)
  
  # Return the job id
  job_id <- XML::xpathSApply(request_xml, path, XML::xmlValue)
  message(paste0("Submit was successful, Job Id: ", job_id))
  return(job_id)
}


#' Check Request Status
#' 
#' This function is called by other functions that submit jobs to
#' the Acoustic/Silverpop API. It checks the status code
#' returned and tells the user if there was an error code and
#' exits the function.
#' 
#' @return If status code is not 200, a message to console 
#' 
#' @keywords internal


check_request_status <- function(request_obj) {
  if (request_obj$status_code == 401) {
    message(paste0("There was a 401 error. Do you need to refresh your access token?"))
    stop_quietly()
  } else if (request_obj$status_code != 200) {
    message(paste0("There was an authentication error: ", request_obj$status_code))
    stop_quietly()
  }
} 


#' Stop Function Quietly
#' 
#' Quit a function execution without printing error messages. The
#' idea came from a Stack Overflow answer 
#' https://stackoverflow.com/questions/14469522/stop-an-r-program-without-error. 
#' 
#' @return Exits a function.
#' 
#' @keywords internal


stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}


#' Detect Faulty XML Request
#' 
#' Searches the results content for the tag "<FaultString>". If it
#' is found it gives the user a message and exits the function.
#' 
#' @importFrom httr "content"
#' 
#' @return Message to the console.
#' 
#' @keywords internal

check_for_faulty_xml <- function(request_obj) {
  if(length(grep("<FaultString>", httr::content(request_obj, "text", encoding = "ISO-8859-1"))) >= 1) {
    message("Faulty XML request. Check your parameters. Full result text:")
    message(httr::content(request_obj, "text", encoding = "ISO-8859-1"))
    stop_quietly()
  }
}
