#' Check Request Status
#' 
#' This function is called by other functions that submit a 
#' requests to revulytics API. It checks the status code
#' returned and tells the user if there was an error code and
#' exits the function.

#' 
#' @return If status code is not 200, returns a message to
#' console with appropriate error message and stops execution. 
#' 
#' @keywords internal

check_status <- function(request_obj) {
  if(request_obj$status_code != 200){
    if (request_obj$status_code >= 500) {
      message(paste0("There was an error on the server and the request could not be completed. The API returned with an error: ", request_obj$status_code))
      stop_quietly()
    } else if ( request_obj$status_code <= 499 && request_obj$status_code >= 400){
      message(paste0("There is an error in the request submitted and the API returned with an error: ", request_obj$status_code))
      stop_quietly()
    } else { 
      message(paste0("The API returned with an error: %s", request_obj$status_code))
      stop_quietly()
    }
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

