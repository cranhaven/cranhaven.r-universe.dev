#' Get cache directory
#' @noRd
get_cache_dir <- function() {
    # If interactive or normal session, use persistent user dir
    dir <- tools::R_user_dir("inkaR", which = "cache")
    if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
    dir
}

#' Get cached API response if valid
#' @param key Cache key string
#' @param ttl_hours Time-to-live in hours (default 24)
#' @return Cached data or NULL if expired/missing
#' @noRd
get_cache <- function(key, ttl_hours = 24) {
    file_path <- file.path(get_cache_dir(), paste0(key, ".qs"))
    if (file.exists(file_path)) {
        info <- file.info(file_path)
        age_hours <- as.numeric(difftime(
            Sys.time(),
            info$mtime,
            units = "hours"
        ))
        if (age_hours < ttl_hours) {
            tryCatch(
                {
                    return(readRDS(file_path))
                },
                error = function(e) NULL
            )
        }
    }
    return(NULL)
}

#' Save API response to cache
#' @param key Cache key string
#' @param value Data to cache
#' @noRd
set_cache <- function(key, value) {
    file_path <- file.path(get_cache_dir(), paste0(key, ".qs"))
    tryCatch(
        {
            saveRDS(value, file_path)
        },
        error = function(e) NULL
    )
}

#' Clear INKAR Cache
#'
#' Clears the persistent disk cache used for API responses (like time reference metadata).
#' @return No return value, called for side effects.
#' @export
clear_inkar_cache <- function() {
    dir <- get_cache_dir()
    files <- list.files(dir, pattern = "\\.qs$", full.names = TRUE)
    if (length(files) > 0) {
        unlink(files)
        message("Cleared ", length(files), " cached files from ", dir)
    } else {
        message("Cache is already empty.")
    }
    invisible(TRUE)
}

#' Internal request helper
#'
#' @param path API endpoint path
#' @param query List of query parameters
#' @param base_url Base URL for INKAR API
#' @return httr2 request object
#' @noRd
inkar_request <- function(
    path,
    query = list(),
    base_url = "https://www.inkar.de"
) {
    # Base URL confirmed as inkar.de
    # Endpoint usually /Table/GetDataTable for POST

    if (!requireNamespace("httr2", quietly = TRUE)) {
        stop("Package 'httr2' is required for this function.")
    }

    req <- httr2::request(base_url) |>
        httr2::req_url_path_append(path) |>
        httr2::req_url_query(!!!query) |>
        httr2::req_user_agent(
            "inkaR R Package"
        ) |>
        httr2::req_retry(max_tries = 3) |>
        # Do not throw R error on HTTP error immediately, so we can parse body
        httr2::req_error(is_error = function(resp) FALSE)

    return(req)
}

#' Perform request and parse JSON
#'
#' @param req httr2 request
#' @return Parsed list or data frame
#' @noRd
perform_request <- function(req) {
    resp <- tryCatch(
        {
            httr2::req_perform(req)
        },
        error = function(e) {
            if (grepl("SSL certificate problem", e$message)) {
                message("SSL certificate verification failed. This may happen on some systems (e.g. Fedora) with incomplete CA bundles.")
                message("Error: ", e$message)
                return(NULL)
            }
            stop(e)
        }
    )

    if (is.null(resp)) return(NULL)

    if (httr2::resp_is_error(resp)) {
        # Try to print body for debugging
        try(
            {
                msg <- httr2::resp_body_string(resp)
                message(
                    "API Error (",
                    httr2::resp_status(resp),
                    "): ",
                    substr(msg, 1, 500)
                )
            },
            silent = TRUE
        )
        stop("API request failed with status ", httr2::resp_status(resp))
    }

    if (httr2::resp_content_type(resp) != "application/json") {
        warning("Response is not JSON.")
    }

    # Parse initial JSON
    content <- httr2::resp_body_json(resp, simplifyVector = TRUE)

    # Handle Double-JSON encoding (INKAR quirk)
    # Sometimes the API returns a JSON string wrapped in quotes "{\"Foo\":...}"
    if (is.character(content) && length(content) == 1) {
        # Try parsing the inner string
        tryCatch(
            {
                inner_content <- jsonlite::fromJSON(
                    content,
                    simplifyVector = TRUE
                )
                return(inner_content)
            },
            error = function(e) {
                # If not valid JSON, return original content
                return(content)
            }
        )
    }

    return(content)
}
