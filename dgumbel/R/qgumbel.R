## Copyright (C) 2020 Berent Lunde
## License: GPL-3

# The Gumbel quantile function.
# Its documentation is combined with dgumbel.
#
#' @rdname dgumbel
#' @export
qgumbel <- function(p, location = 0, scale = 1, lower.tail = TRUE, grad=FALSE){
    
    # check input
    error_messages <- c()
    error_messages_type <- c(
        "p" = "\n Error: p must be a scalar or vector of type numeric between zero and one",
        "location" = "\n Error: location must be a real number",
        "scale" = "\n Error: scale must be a positive real number",
        "lower.tail" = "\n Error: lower.tail must be of type logical with length 1",
        "grad" = "\n Error: grad must be of type logical with length 1"        
    )
    
    # check x
    if(is.vector(p, mode="numeric") && all(p > 0) && all(p<1)){
        #ok
    }else{
        error_messages <- c(error_messages, error_messages_type["p"])
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
    
    # check lower.tail
    if(is.logical(lower.tail) && length(lower.tail)==1){
        #ok
    }else{
        # error
        error_messages <- c(error_messages, error_messages_type["lower.tail"])
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
        res <- .qgumbel(p, location, scale, lower.tail)
    }else{
        res <- .dqgumbel(p, location, scale, lower.tail)
    }
    
    return(res)
    
}