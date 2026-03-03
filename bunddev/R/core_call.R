#' Call an API operation
#'
#' @param api Registry id.
#' @param operation_id OpenAPI operationId (use this OR path+method).
#' @param params List of query parameters.
#' @param path API path (use with method instead of operation_id).
#' @param method HTTP method (use with path instead of operation_id).
#' @param parse Response parsing mode.
#' @param base_url Optional base URL override.
#' @param body Optional request body (for POST/PUT requests).
#' @param body_type Body encoding type ("json" or "form").
#' @param headers Optional named list of custom HTTP headers.
#' @param safe Logical; apply throttling and caching.
#' @param refresh Logical; refresh cached GET responses.
#'
#' @details
#' This is the low-level OpenAPI caller. It supports two modes:
#' - Use `operation_id` to lookup endpoints by their OpenAPI operationId
#' - Use `path` + `method` for APIs without operationIds
#'
#' The function fills path parameters from `params`, applies auth (if
#' configured), and optionally caches GET responses when `safe = TRUE`.
#'
#' Use [bunddev_parameters()] and [bunddev_parameter_values()] to discover valid
#' parameters before calling.
#'
#' @seealso
#' [bunddev_call_tidy()] for adapter-specific tidy outputs, and
#' [bunddev_auth_set()] to configure API keys.
#'
#' @examples
#' \dontrun{
#' # Retrieve Autobahn road ids (raw response)
#' bunddev_call("autobahn", "list-autobahnen")
#' }
#'
#' @return Parsed response.
#' @export
bunddev_call <- function(api, operation_id = NULL, params = list(),
                         path = NULL, method = NULL,
                         parse = c("json", "text", "raw", "xml"),
                         base_url = NULL,
                         body = NULL, body_type = c("json", "form"),
                         headers = NULL,
                         safe = TRUE, refresh = FALSE) {
  parse <- rlang::arg_match(parse)
  if (!is.null(body)) {
    body_type <- rlang::arg_match(body_type)
  }

  # Validate that exactly one approach is used
  has_operation_id <- !is.null(operation_id)
  has_path_method <- !is.null(path) && !is.null(method)

  if (!has_operation_id && !has_path_method) {
    cli::cli_abort("Must provide either 'operation_id' OR both 'path' and 'method'.")
  }
  if (has_operation_id && has_path_method) {
    cli::cli_abort("Cannot provide both 'operation_id' AND 'path'/'method'. Choose one approach.")
  }

  spec <- bunddev_spec(api)

  # Build endpoint list - include ALL endpoints regardless of operationId
  endpoints <- purrr::imap(spec$paths, function(path_item, endpoint_path) {
    if (!is.list(path_item)) {
      return(NULL)
    }
    methods <- names(path_item)
    purrr::map(methods, function(endpoint_method) {
      operation <- path_item[[endpoint_method]]
      if (!is.list(operation)) {
        return(NULL)
      }
      list(
        method = endpoint_method,
        path = endpoint_path,
        operation_id = operation$operationId,  # Can be NULL
        operation = operation
      )
    })
  })

  endpoints <- purrr::flatten(endpoints)
  endpoints <- purrr::compact(endpoints)

  # Find endpoint by operation_id OR by path+method
  if (has_operation_id) {
    match <- purrr::keep(endpoints, ~ !is.null(.x$operation_id) && .x$operation_id == operation_id)

    # Fallback to endpoint registry if no operationId found in spec
    if (length(match) == 0) {
      reg_entry <- tryCatch(bunddev_endpoint_spec(operation_id), error = function(e) NULL)
      if (!is.null(reg_entry) && reg_entry$api == api) {
        # Find endpoint by path and method from registry
        match <- purrr::keep(endpoints, ~ .x$path == reg_entry$path &&
                                           .x$method == reg_entry$method)
      }

      if (length(match) == 0) {
        cli::cli_abort("Operation '{operation_id}' not found for API '{api}'.")
      }
    }

    endpoint <- match[[1]]
  } else {
    # Find by path and method
    match <- purrr::keep(endpoints, ~ .x$path == path && .x$method == tolower(method))

    if (length(match) == 0) {
      # If endpoint not found in spec, create a synthetic endpoint for dynamic paths
      # This allows adapters to use bunddev_call() with paths not defined in the spec
      endpoint <- list(
        method = tolower(method),
        path = path,
        operation_id = NULL,
        operation = list()
      )
    } else {
      endpoint <- match[[1]]
    }
  }

  # Determine base URL - use override if provided, otherwise resolve from spec
  if (is.null(base_url)) {
    # Handle OpenAPI 3.0 (servers) and Swagger 2.0 (host + basePath) formats
    # Priority: operation-level servers > path-level servers > global servers > Swagger 2.0 host

    # 1. Check for operation-level servers (path-specific overrides in OpenAPI 3.0)
    if (!is.null(endpoint$operation$servers) && length(endpoint$operation$servers) > 0) {
      base_url <- endpoint$operation$servers[[1]]$url
    # 2. Check for path-level servers
    } else if (!is.null(spec$paths[[endpoint$path]]$servers) &&
               length(spec$paths[[endpoint$path]]$servers) > 0) {
      base_url <- spec$paths[[endpoint$path]]$servers[[1]]$url
    # 3. Fall back to global servers (OpenAPI 3.0)
    } else if (!is.null(spec$servers) && length(spec$servers) > 0) {
      base_url <- spec$servers[[1]]$url
    # 4. Fall back to Swagger 2.0 format
    } else if (!is.null(spec$host)) {
      scheme <- if (!is.null(spec$schemes)) spec$schemes[1] else "https"
      base_path <- spec$basePath %||% ""
      base_url <- paste0(scheme, "://", spec$host, base_path)
    } else {
      cli::cli_abort("OpenAPI spec has no servers or host defined.")
    }

    if (is.null(base_url) || base_url == "") {
      cli::cli_abort("OpenAPI server URL is missing.")
    }
  }

  if (stringr::str_detect(base_url, "\\{")) {
    variables <- spec$servers[[1]]$variables
    if (is.null(variables) || !is.list(variables)) {
      cli::cli_abort("Server URL requires variables, but none are provided.")
    }
    var_matches <- stringr::str_match_all(base_url, "\\{([^}]+)\\}")[[1]]
    if (nrow(var_matches) > 0) {
      for (var_name in var_matches[, 2]) {
        default_value <- variables[[var_name]]$default
        if (is.null(default_value) || default_value == "") {
          cli::cli_abort("Server URL variable '{var_name}' has no default.")
        }
        base_url <- stringr::str_replace_all(
          base_url,
          paste0("\\{", var_name, "\\}"),
          default_value
        )
      }
    }
  }

  # Save original params before path substitution for cache key calculation
  original_params <- params

  path <- endpoint$path
  path_params <- stringr::str_match_all(path, "\\{([^}]+)\\}")[[1]]
  if (nrow(path_params) > 0) {
    for (param in path_params[, 2]) {
      if (!param %in% names(params)) {
        cli::cli_abort("Missing path parameter '{param}'.")
      }
      value <- as.character(params[[param]])
      path <- stringr::str_replace_all(path, paste0("\\{", param, "\\}"), value)
    }
    params[path_params[, 2]] <- NULL
  }

  base_url <- stringr::str_remove(base_url, "/$")
  path <- stringr::str_remove(path, "^/")
  url <- paste0(base_url, "/", path)

  method <- toupper(endpoint$method)
  method_lower <- tolower(endpoint$method)

  if (isTRUE(safe)) {
    bunddev_rate_limit_wait(api)
  }

  cache_path <- NULL
  if (isTRUE(safe) && method_lower == "get") {
    cache_path <- bunddev_response_cache_path(api, operation_id, original_params,
                                                path = endpoint$path)
    if (!isTRUE(refresh) && file.exists(cache_path)) {
      raw_body <- readBin(cache_path, "raw", n = file.info(cache_path)$size)
      return(bunddev_parse_response(raw_body, parse))
    }
  }

  req <- httr2::request(url)
  req <- httr2::req_method(req, method)

  if (length(params) > 0) {
    req <- httr2::req_url_query(req, !!!params)
  }

  # Add request body if provided
  if (!is.null(body)) {
    if (body_type == "json") {
      req <- httr2::req_body_json(req, body)
    } else if (body_type == "form") {
      req <- httr2::req_body_form(req, !!!body)
    }
  }

  # Handle authentication
  auth <- bunddev_auth_get(api)

  # Check if spec requires API key but none is configured - try to extract from spec
  if (auth$type == "none" && !is.null(spec$components$securitySchemes)) {
    for (scheme_name in names(spec$components$securitySchemes)) {
      scheme <- spec$components$securitySchemes[[scheme_name]]
      if (scheme$type == "apiKey" && scheme$`in` == "header") {
        # Check if description contains the API key value (for public keys)
        if (!is.null(scheme$description)) {
          # Pattern: "X-API-Key ist die Client-ID <value>"
          match <- stringr::str_match(scheme$description, "(?:Client-ID|client[_\\s-]?id)[^a-zA-Z0-9_-]+([-a-zA-Z0-9_]+)")
          if (!is.na(match[1, 2])) {
            header_list <- list()
            header_list[[scheme$name]] <- match[1, 2]
            req <- httr2::req_headers(req, !!!header_list)
            auth$type <- "public_key_from_spec"  # Mark as handled
          }
        }
      }
    }
  }

  if (auth$type == "api_key") {
    token <- bunddev_auth_token(api)

    # Determine header name from OpenAPI spec security schemes
    header_name <- "Authorization"
    if (!is.null(spec$components$securitySchemes)) {
      # Find the first apiKey scheme
      for (scheme_name in names(spec$components$securitySchemes)) {
        scheme <- spec$components$securitySchemes[[scheme_name]]
        if (scheme$type == "apiKey" && scheme$`in` == "header") {
          header_name <- scheme$name
          break
        }
      }
    } else if (!is.null(spec$securityDefinitions)) {
      # Swagger 2.0 format
      for (scheme_name in names(spec$securityDefinitions)) {
        scheme <- spec$securityDefinitions[[scheme_name]]
        if (scheme$type == "apiKey" && scheme$`in` == "header") {
          header_name <- scheme$name
          break
        }
      }
    }

    # Set the header with just the token value (no scheme prefix for custom headers)
    if (header_name == "Authorization") {
      auth_value <- bunddev_auth_header(api, token)
    } else {
      auth_value <- token
    }

    header_list <- list()
    header_list[[header_name]] <- auth_value
    req <- httr2::req_headers(req, !!!header_list)
  } else if (auth$type == "oauth2") {
    # Try to fetch OAuth token; fall back to API key if no secret available
    token <- bunddev_oauth_token(api)
    if (!is.null(token)) {
      # Use OAuth token in configured header
      oauth_header <- auth$oauth_token_header %||% "OAuthAccessToken"
      header_list <- list()
      header_list[[oauth_header]] <- token
      req <- httr2::req_headers(req, !!!header_list)
    } else {
      # Fall back to sending client ID as API key
      client_id <- bunddev_oauth_client_id(api)
      fallback_header <- auth$oauth_fallback_header %||% "X-API-Key"
      header_list <- list()
      header_list[[fallback_header]] <- client_id
      req <- httr2::req_headers(req, !!!header_list)
    }
  }

  # Add custom headers if provided
  if (!is.null(headers) && length(headers) > 0) {
    req <- httr2::req_headers(req, !!!headers)
  }

  resp <- httr2::req_perform(req)
  raw_body <- tryCatch(
    httr2::resp_body_raw(resp),
    error = function(e) raw(0)
  )

  # Handle empty responses
  if (length(raw_body) == 0) {
    if (parse == "json") {
      return(list())
    } else if (parse == "text") {
      return("")
    }
    return(raw(0))
  }

  if (!is.null(cache_path)) {
    writeBin(raw_body, cache_path)
  }

  bunddev_parse_response(raw_body, parse)
}

bunddev_parse_response <- function(raw_body, parse) {
  if (parse == "json") {
    return(jsonlite::fromJSON(rawToChar(raw_body), simplifyVector = FALSE))
  }
  if (parse == "text") {
    return(rawToChar(raw_body))
  }
  if (parse == "xml") {
    return(xml2::read_xml(rawToChar(raw_body)))
  }

  raw_body
}
