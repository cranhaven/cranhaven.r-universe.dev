myTarCheckLimits <- function( answer_obj ) {
  
  if ( ! is.null(answer_obj$limits) ) {
    
    remaining_time <- max(
      answer_obj$remaining[['1']],
      answer_obj$remaining[['3600']],
      answer_obj$remaining[['86400']], na.rm = T)
    
    err_text <- str_interp(
      "MyTarget API Limit!!!
Current calls limits:
Per second: ${answer_obj$limits[['1']]}
Per hour:   ${answer_obj$limits[['3600']]}
Per day:    ${ifelse( is.null(answer_obj$limits[['86400']]), '-', answer_obj$limits[['86400']])}

Remaining calls: 
Per second: ${answer_obj$remaining[['1']]}
Per hour:   ${answer_obj$remaining[['3600']]}
Per day:    ${ifelse( is.null(answer_obj$remaining[['86400']]), '-', answer_obj$remaining[['86400']])}

For more details go link (https://target.my.com/help/advertisers/api_arrangement/ru#rec117524632) .
")
    
    stop(err_text)
    
  } 
  
  return(TRUE)
  
}