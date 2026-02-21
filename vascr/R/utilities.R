#' Create a notification
#' 
#' Creates a notification, either passed through to shiny or printed with cli
#'
#' @param level Level of error, either "success", "info", "warning" or "error"
#' @param message The message to be passed on
#'
#' @returns No return, stops execution if error
#' 
#' @importFrom cli cli_alert_success cli_alert_info cli_alert_warning cli_alert_info
#' 
#' @noRd
#'
#' @examples
#' vascr_notify("success", "yay")
#' vascr_notify("info", "Test1")
#' vascr_notify("warning", "TEST")
#' vascr_notify("error", "ERROR")
#' 
vascr_notify = function(level, message){
  
  if(rlang::is_installed("shiny")){
      if(shiny::isRunning()){
        
        if(level == "success")  shiny::showNotification(id = "success_box", message, type = "default")
        if(level == "info")  shiny::showNotification(message, type = "message")
        if(level == "warning")  shiny::showNotification(message, type = "warning")
        if(level == "error")  shiny::showNotification(message, type = "error")
        
      }
  }
  
  if(level == "success")
  {
    cli_alert_success(message)
  } else if(level == "warning")
  {
    cli_alert_warning(message)
  } else if(level == "error")
  {
    #cli_alert_danger(message)
    stop(message)
  } else {
    cli_alert_info(message)
  }
  
}

