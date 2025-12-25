## Copyright (C) 2020 Berent Lunde
## License: GPL-3

# The Gumbel probability distribution function.
# Its documentation is combined with dgumbel.
#
#' @rdname dgumbel
#' @export
pgumbel <- function(q, location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE, grad=FALSE){
    
    # check input
    error_messages <- c()
    error_messages_type <- c(
        "q" = "\n Error: q must be a scalar or vector of type numeric",
        "location" = "\n Error: location must be a real number",
        "scale" = "\n Error: scale must be a positive real number",
        "lower.tail" = "\n Error: lower.tail must be of type logical with length 1",
        "log.p" = "\n Error: log.p must be of type logical with length 1",
        "grad" = "\n Error: grad must be of type logical with length 1"        
    )
    
    # check x
    if(!is.vector(q, mode="numeric")){
        error_messages <- c(error_messages, error_messages_type["q"])
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
    
    # check log.p
    if(is.logical(log.p) && length(log.p)==1){
        #ok
    }else{
        # error
        error_messages <- c(error_messages, error_messages_type["log.p"])
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
        res <- .pgumbel(q, location, scale, lower.tail, log.p)
    }else{
        res <- .dpgumbel(q, location, scale, lower.tail, log.p)
    }
    
    return(res)
    
}