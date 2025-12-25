## Copyright (C) 2020 Berent Lunde
## License: GPL-3

# The Gumbel probability density function.
# Its documentation is combined with gumbel-doc
#
#' @rdname pgumbel
#' @export
dgumbel <- function(x, location = 0, scale = 1, log = FALSE, grad=FALSE){
    
    # check input
    error_messages <- c()
    error_messages_type <- c(
        "x" = "\n Error: x must be a scalar or vector of type numeric",
        "location" = "\n Error: location must be a real number",
        "scale" = "\n Error: scale must be a positive real number",
        "log" = "\n Error: log must be of type logical with length 1",
        "grad" = "\n Error: grad must be of type logical with length 1"        
    )
    
    # check x
    if(!is.vector(x, mode="numeric")){
        error_messages <- c(error_messages, error_messages_type["x"])
    }
    
    # check location
    if(is.numeric(location) && length(location)==1){
        #ok
    }else{
        error_messages <- c(error_messages, error_messages_type["location"])
    }
    
    # check scale
    if(is.numeric(scale) && length(scale)==1 && scale > 0){
        #ok
    }else{
        error_messages <- c(error_messages, error_messages_type["scale"])
    }
    
    # check log
    if(is.logical(log) && length(log)==1){
        #ok
    }else{
        # error
        error_messages <- c(error_messages, error_messages_type["log"])
    }
    
    # check grad
    if(is.logical(grad) && length(grad)==1){
        #ok
    }else{
        # error
        error_messages <- c(error_messages, error_messages_type["grad"])
    }
    
    # Any error messages?
    if(length(error_messages)>0)
        stop(error_messages)
    
    
    # if ok
    if(!grad){
        res <- .dgumbel(x, location=location, scale=scale, log_dens=log)
    }else{
        res <- .ddgumbel(x, location=location, scale=scale, log_dens=log)
    }
    
    return(res)
    
}