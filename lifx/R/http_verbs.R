

# a lot of this code is following https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html

# global variable to let 'LIFX' know through which application the call is coming:
user_agent <- httr::user_agent("http://github.com/mabafaba/lifx")

# GET / PUT / POST -----------------------------------------------------------------


#' GET request
#' @template param_selector
#' @template param_token
#' @template http_verbs
lx_GET <- function(selector = "all", endpoint, token = lx_get_token()) {
    url <- lifx_api_url(selector, endpoint)
    header <- lx_auth(token)
    response <- httr::GET(url, header, user_agent)
    response <- as_lifx_api_response(response)
    response
}


#' PUT request
#' @template param_selector
#' @template param_token
#' @template http_verbs
#' @param ... named values to add to the request body
lx_PUT <- function(selector = "all", endpoint, token, ...) {
    url <- lifx_api_url(selector, endpoint)
    header <- lx_auth(token)
    response <- httr::PUT(url, header, body = list(...), encode = "json")
    response <- as_lifx_api_response(response)
    invisible(response)
}

#' POST request
#' @template param_selector
#' @template param_token
#' @template http_verbs
#' @param ... named values to add to the request body
lx_POST <- function(selector = "all", endpoint, token, ...) {
    url <- lifx_api_url(selector, endpoint)
    header <- lx_auth(token)
    response <- httr::POST(url, header, body = list(...), encode = "json")
    response <- as_lifx_api_response(response)
    invisible(response)
    
}



# helpers -----------------------------------------------------------------

lifx_api_url <- function(selector, endpoint) {
    httr::modify_url("https://api.lifx.com", path = c("v1", "lights", selector, endpoint))
}

lx_auth <- function(token = lx_get_token()) {
    httr::add_headers(Authorization = sprintf("Bearer %s", token))
    
}

