#' Make an Authenticated API Call to a DHIS2 Server
#'
#' `api_get()` function executes a request to the DHIS2 API server, handling
#' authentication, query parameters, retries, and logging.
#'
#' @param endpoint The DHIS2 API endpoint path to call (e.g., "analytics", "dataElements").
#' @param ... Additional query parameters for the API call.
#' @param retry Number of times to retry the API call in case of failure
#'   (defaults to 2).
#' @param verbosity Level of http information to print during the call:
#'  - 0: No output
#'  - 1: Show headers
#'  - 2: Show headers and bodies
#'  - 3: Show headers, bodies, and curl status message
#' @param timeout Maximum number of seconds to wait for the response (default to 60).
#' @param paging Set if responses should be paginated. (default is FALSE).
#' @param call The execution environment of a currently running function, e.g.,
#'   [caller_env()]. The function will be mentioned in error messages for debugging.
#'
#' @return A parsed JSON object containing the DHIS2 API response data.
#'
#' @details Uses HTTP Basic Authentication with credentials provided using
#'   [khis_cred()]
#'
#' @examplesIf khis_has_cred()
#'
#' analytics_data <- api_get("analytics", startDate = "2023-01-01", endDate = "2023-02-28")
#'
#' @noRd

api_get <- function(endpoint,
                    ...,
                    retry = 2,
                    verbosity = 0,
                    timeout = 60,
                    paging = FALSE,
                    auth = NULL,
                    call = caller_env()) {

    check_required(endpoint, call = call)
    check_scalar_character(endpoint, call = call)
    check_has_credentials(auth = auth, call = call)

    params <- list2(
        ...,
        paging = paging,
        ignoreLimit = 'true'
    )

    resp <- request(khis_base_url(auth)) %>%
        req_url_path_append('api', endpoint) %>%
        req_url_query(!!!params) %>%
        req_headers('Accept' = 'application/json') %>%
        req_user_agent('khisr/1.0.6 (https://khisr.damurka.com)') %>%
        req_retry(max_tries = retry) %>%
        req_timeout(timeout) %>%
        req_auth_khis_basic(auth = auth, call = call) %>%
        req_error(body = handle_error) %>%
        req_perform(verbosity = verbosity, error_call = call) %>%
        resp_body_json()

    return(resp)
}

handle_error <- function(resp) {
    content_type <- resp_content_type(resp)
    url <- resp_url(resp)

    # Handle JSON response
    if (grepl("application/json", content_type, ignore.case = TRUE)) {
        tryCatch({
            parsed_body <- resp_body_json(resp)
            error_message <- parsed_body$message %||% "Unknown error in JSON response."
            return(error_message)
        }, error = function(e) {
            return("Failed to parse JSON response: Invalid or malformed JSON.")
        })
    } else {
        # If not JSON, return a meaningful error message
        return(paste0(
            "Unsupported content type '", content_type, "' received from: ", url,
            ". Only 'application/json' is supported."
        ))
    }
}
