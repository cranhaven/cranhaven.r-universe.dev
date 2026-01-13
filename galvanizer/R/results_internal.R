post_results <- function(auth, url, payload){
  # Posts the result and parses the response
  
  response <- httr::POST(url,
                   body = toJSON(payload, auto_unbox = TRUE),
                   hb_headers(auth),
                   encoding = "raw")
}

put_results <- function(auth, url, payload){
  # Updates the result and parses the response
  
  response <- httr::PUT(url,
                  body = toJSON(payload, auto_unbox = TRUE),
                  hb_headers(auth),
                  encoding = "raw")
  
}

delete_results <- function(auth, url, payload = NULL){
  # Deletes the results - expect no response
  
  response <- httr::DELETE(url,
                           body = toJSON(payload, auto_unbox = TRUE),
                           hb_headers(auth),
                           encoding = "raw")

  return(response)
}

results_payload <- function(auth, primary, secondary, type, action, payload = NULL, verbose = FALSE, ...){
  
  # Check
  if (!is.list(payload) & !is.null(payload)){stop("Data in list format is required")}
  
  # Payload must exist
  if (is.null(payload) & action != 'delete'){
    stop("Delete action detected. Payload required.")
  } 
  
  # Where is the data going
  url <- paste0(hb_url(auth), hb_url_component(type, primary, secondary))
  
  # # Create the required elements for the body
  # payload <- hb_initpayload(type)
  # payload$data$attributes$name <- name
  
  # Add any non-required body elements
  if(!is.null(payload) & action != 'delete'){
    payload <- body_augmenter(payload, ...) 
  }
  
  # print("Before calling payload")
  # print(url)
  # print(payload)
  
  # Call on the messenger 
  response <- results_payload_coordinate(auth, action, url, payload)  
  return(response)
}


results_payload_coordinate <- function(auth, action, url, payload = NULL){
  # Sets up the payload for delivery
  
  if (!is.list(payload) & !is.null(payload)){stop("Data in list format is required")}
  
  if (action == 'create'){
    response <- post_results(auth, url, payload)
  } else if (action == 'update'){
    response <- put_results(auth, url, payload)
  } else if (action == 'delete'){
    response <- delete_results(auth, url)
  }
  
  hb_validateDownload(response)
  
  if (action == 'delete'){
    return(response) # Early return for a delete
  }
  
  # If its successful, the data will come back, which can then be parsed
  
  json <- httr::content(response, 'text')
  content <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  parsed <- hb_parse_content(content, FALSE)
  
  return(parsed)
}

#' @importFrom utils modifyList
body_augmenter <- function(body, attributes = NULL, relationships = NULL){
  # Adds attributes or bodies, if specified, if required
  # If new elements needed to be added to body$data, do it here
  
  # Future improvement - pass these as ...
  # args <- as.character(as.list(substitute(list(...)))[-1L])
  
  if (!is.null(attributes)){
    if(!is.list(body$data$attributes)){
      body$data$attributes <- list() # Force this element to exist if it doesn't in the body
    }
    new_attributes <- utils::modifyList(body$data$attributes, attributes)
    body$data$attributes <- new_attributes
  }
  if (!is.null(relationships)){
    if(!is.list(body$data$relationships)){
      body$data$relationships <- list() # Force this element to exist if it doesn't in the body
    }
    new_relationships <- modifyList(body$data$relationships, relationships)
    body$data$relationships <- new_relationships
  }
  
  return(body)
}

hb_callingfunc <- function(){
  # Who is calling this function
  calling_func <- as.list(sys.call(-1))[[1]]
  
  # Find everything before first underscore
  gsub("(.+?)(\\_.*)", "\\1", calling_func)
}

hb_callingtarget <- function(){
  calling_target <- as.list(sys.call(-1))[[1]]

  sub(".*_", "", calling_target)
}

hb_initpayload <- function(type){
  # Creates a list object, with the top level of data and type
  
  payload <- list()
  payload$data$type <- type
  
  return(payload)
}
