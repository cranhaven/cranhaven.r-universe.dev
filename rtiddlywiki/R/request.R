
.ip_cache_success <- new.env(parent = emptyenv())


.check_ip <- function(req) {

    # Extract host/domain from request object
    url <- httr2::url_parse(req$url)
    port <- url$port
    if (is.null(port)) {
        port <- ifelse(url$scheme == "https", 443, 80)
    }
    domain <- url$hostname
    if (exists(domain, envir = .ip_cache_success)) {
        resolved <- get(domain, envir = .ip_cache_success)
        try_req <- req |> httr2::req_options(resolve = resolved)
        return(try_req)
    }

    stopifnot(length(domain) == 1)
    
    tryCatch({
        # Resolve all IPs
        ips <- pingr::nsl(domain)
        
        if (length(ips) == 0) return(req)
        if (nrow(ips$answer) <= 1) return(req)
        
        for (i in seq_len(nrow(ips$answer))) {
            ip <- ips$answer$data[[i]]
            url2 <- url
            url2$hostname <- ip
            resolved <- paste0(domain, ":", port, ":", ip)
            try_req <- req |>
                httr2::req_options(resolve  = resolved) # self signed ssl
                
            # Attempt request (head only to test connectivity)
            success <- try({
                resp <- httr2::req_perform(try_req)
                TRUE
            }, silent = TRUE)
            
            if (!inherits(success, "try-error") && success) {
                message("Connected successfully to IP: ",  ips$answer$data[[i]])
                assign(domain, resolved, envir = .ip_cache_success)
                return(try_req)
            }
        }
    }, error = function(e) {
        TRUE
    })
    return(req)
}

request <- function(method = "GET",
                    path = '/',
                    query = NULL,
                    body = NULL,
                    auto_unbox = TRUE) {
    # Check major arguments
    stopifnot(length(method) == 1)
    stopifnot(is.character(method))
    stopifnot(method %in% c("GET", "PUT", "POST", "DELETE"))

    host <- TW_OPTIONS("host")
    stopifnot(length(host) == 1)

    # Create request
    req <- httr2::request(host) |> 
        httr2::req_options(ssl_verifypeer = 0) # self signed ssl

    # add x-auth-key header for specific permission. not general usage
    http_x_auth_key <- TW_OPTIONS("http_x_auth_key")

    if (nchar(http_x_auth_key) > 0) {
        req <- req |>
            httr2::req_headers(`X-Auth-Key` = http_x_auth_key)
    }
    req <- .check_ip(req)

    req <- req |> 
        httr2::req_url_path(path) |>
        httr2::req_method(method)
    
        
    if (!is.null(query) && is.list(query)) {
        query$.req <- req
        req <- do.call(httr2::req_url_query, query)
    }


    

    # add header for x-request-with for put request
    if (method %in% c("PUT", "DELETE")) {
        req <- req |>
            httr2::req_headers(`x-requested-with` = "TiddlyWiki")
    }

    # Add body
    if (!is.null(body)) {
        req <- req |>
            httr2::req_body_json(data = body, auto_unbox = auto_unbox)
    }

    # Perform the actual request
    resp <- req |>
        httr2::req_error(is_error = \(resp) FALSE) |>
        httr2::req_perform()

    # Return null for empty body/response
    if (length(resp$body) == 0) {
        return(NULL)
    }

    status_code <- httr2::resp_status(resp)
    if (status_code == 403) {
        stop("403 Forbidden")
    }
    c_type <- resp$headers$`Content-Type`



    if (!is.null(c_type) && grepl("text/plain", c_type)) {
        content <- resp |>
            httr2::resp_body_string()
    } else if (!is.null(c_type) && grepl("text/html", c_type)) {
        content <- resp |>
            httr2::resp_body_string()
    } else {
        content <- resp |>
            httr2::resp_body_json()
    }

    content



}


