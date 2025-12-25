## Copyright (C) 2020 Berent Lunde
## License: GPL-3

# The Gumbel random generating function.
# Its documentation is combined with dgumbel.
#
#' @rdname dgumbel
#' @export
rgumbel <- function(n, location = 0, scale = 1){
    
    # check input
    error_messages <- c()
    error_messages_type <- c(
        "n" = "\n Error: n must be a positive integer",
        "location" = "\n Error: location must be a real number",
        "scale" = "\n Error: scale must be a positive real number"
    )
    
    # check n
    if(is.numeric(n) && length(n)==1 && n%%1==0 && n>0){
        #ok
    }else{
        error_messages <- c(error_messages, error_messages_type["n"])
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
    
    # Any error messages?
    if(length(error_messages)>0)
        stop(error_messages)

    x <- .rgumbel(n, location, scale)
        
    return(x)

}
