validate_response <- function(response) {
  status <- status_code(response)

  switch(as.character(status),
         "200" = invisible(response),
         "403" = stop("Invalid API key!"),
         "404" = {
           ctd <- content(response)
           if(is.list(ctd)) {

             # Customer specific API not found
             if("message" %in% names(ctd) && grepl("no API found", ctd$message)) {
               stop("Username not found! This service is only available to customers who ordered individual exports. If you ordered such a service, please make sure the username is spelled correctly.")
             } else if("error" %in% names(ctd)) {
               has_api_key <- grepl("apikey=", response$url)
               stop(sprintf("The API responded with\n%s.\nAre you sure the requested collection exists and is %s?",
                            ctd$error,
                            ifelse(has_api_key, "public", "non-public")))
             }
           } else {
             stop("An unknown API error ocurred.")
           }
         },
         "412" = {
           ctd <- content(response)
           if(is.list(ctd)) {
            if("message" %in% names(ctd)) {
               has_api_key <- grepl("apikey=", response$url)
               stop(sprintf("The API responded with\n%s.\nAre you sure the requested collection exists and is %s?",
                            ctd$message,
                            ifelse(has_api_key, "public", "non-public")))
             }
           } else {
             stop("An unknown API error ocurred.")
           }
         })
}
