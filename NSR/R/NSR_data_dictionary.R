#'Get NSR data dictionary 
#'
#'Returns information from the NSR data dictionary
#' @param native_status Logical. If FALSE(Default) returns information on fields. If TRUE, returns information on Native Status categories. 
#' @param ... Additional arguments passed to internal functions.
#' @return Data.frame containing requested data dictionary contents.
#' @export
#' @examples {
#' NSR_fields <- NSR_data_dictionary()
#' 
#' status_codes <- NSR_data_dictionary(native_status = TRUE)
#' }
#' 
NSR_data_dictionary <- function(native_status=FALSE, ...){

  if(native_status){
    
    return(nsr_core(mode = "dd_ns", ...)$dd)
    
  }else{
    
    return(nsr_core(mode="dd", ...)$dd)
    
  } 

}
