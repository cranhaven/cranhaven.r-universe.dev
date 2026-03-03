#' Set authentication configuration for an API
#'
#' @param api Registry id.
#' @param type Authentication type ("none", "api_key", or "oauth2").
#' @param env_var Environment variable containing credentials (for api_key) or client ID (for oauth2).
#' @param scheme Authentication scheme used in the Authorization header (e.g., "Bearer", "ApiKey", "OAuth").
#' @param oauth_url OAuth2 token endpoint URL (for oauth2 type).
#' @param oauth_secret_env OAuth2 client secret environment variable (for oauth2 type).
#' @param oauth_default_id Default OAuth2 client ID if env_var is not set (for oauth2 type).
#' @param oauth_token_header Header name for OAuth token (default "OAuthAccessToken").
#' @param oauth_fallback_header Header name when no secret available (default "X-API-Key").
#'
#' @details
#' Store auth configuration used by [bunddev_call()] and adapter helpers.
#'
#' For API key auth: set `env_var` to the name of an environment variable containing
#' the key. The key is sent as `Authorization: {scheme} <key>`.
#'
#' For OAuth2 client credentials: set `oauth_url` to the token endpoint,
#' `env_var` to the client ID env var, and `oauth_secret_env` to the client secret
#' env var. If the secret is available, fetches an OAuth token; otherwise falls
#' back to sending the client ID as an API key.
#'
#' @seealso
#' [bunddev_auth_get()] to inspect the stored configuration, and
#' [bunddev_call()] to make authenticated requests.
#'
#' @examples
#' # API key authentication
#' Sys.setenv(JOBBOERSE_API_KEY = "jobboerse-jobsuche")
#' bunddev_auth_set("jobsuche", type = "api_key", env_var = "JOBBOERSE_API_KEY", scheme = "X-API-Key")
#'
#' # OAuth2 client credentials
#' bunddev_auth_set("berufssprachkurssuche",
#'   type = "oauth2",
#'   oauth_url = "https://rest.arbeitsagentur.de/oauth/gettoken_cc",
#'   env_var = "BERUFSSPRACHKURSSUCHE_API_KEY",
#'   oauth_secret_env = "BERUFSSPRACHKURSSUCHE_CLIENT_SECRET",
#'   oauth_default_id = "bd24f42e-ad0b-4005-b834-23bb6800dc6c")
#'
#' @return The updated auth configuration.
#' @export
bunddev_auth_set <- function(api, type = "api_key", env_var, scheme = NULL,
                              oauth_url = NULL, oauth_secret_env = NULL,
                              oauth_default_id = NULL,
                              oauth_token_header = "OAuthAccessToken",
                              oauth_fallback_header = "X-API-Key") {
  type <- rlang::arg_match(type, c("none", "api_key", "oauth2"))

  if (type == "api_key" && (missing(env_var) || is.null(env_var) || env_var == "")) {
    cli::cli_abort("env_var is required for auth type '{type}'.")
  }

  if (type == "oauth2" && is.null(oauth_url)) {
    cli::cli_abort("oauth_url is required for auth type 'oauth2'.")
  }

  auth <- getOption("bunddev.auth", list())
  env_value <- if (missing(env_var) || is.null(env_var)) NA_character_ else env_var
  auth[[api]] <- list(
    type = type,
    env_var = env_value,
    scheme = scheme,
    oauth_url = oauth_url,
    oauth_secret_env = oauth_secret_env,
    oauth_default_id = oauth_default_id,
    oauth_token_header = oauth_token_header,
    oauth_fallback_header = oauth_fallback_header
  )
  options(bunddev.auth = auth)
  auth[[api]]
}

#' Get authentication configuration for an API
#'
#' @param api Registry id.
#'
#' @details
#' Returns the stored auth configuration for the API or a default `none` entry
#' if no auth has been configured.
#'
#' @seealso
#' [bunddev_auth_set()] to configure credentials.
#'
#' @examples
#' bunddev_auth_get("jobsuche")
#'
#' @return A list with auth settings.
#' @export
bunddev_auth_get <- function(api) {
  auth <- getOption("bunddev.auth", list())
  if (!is.list(auth) || is.null(auth[[api]])) {
    return(list(type = "none", env_var = NA_character_, scheme = NA_character_))
  }

  auth[[api]]
}

#' Get raw auth token for an API
#'
#' @param api Registry id.
#'
#' @return The raw token value, or NULL if no auth configured.
#' @keywords internal
bunddev_auth_token <- function(api) {
  auth <- bunddev_auth_get(api)
  if (auth$type == "none") {
    return(NULL)
  }

  if (is.na(auth$env_var) || auth$env_var == "") {
    cli::cli_abort("API key env_var is not set for '{api}'.")
  }
  token <- Sys.getenv(auth$env_var)
  if (token == "") {
    cli::cli_abort("Environment variable '{auth$env_var}' is not set.")
  }

  token
}

#' Build Authorization header value
#'
#' @param api Registry id.
#' @param token Optional token to use instead of env_var lookup.
#'
#' @details
#' Constructs the Authorization header value based on the configured scheme.
#' Supports template-style schemes containing `%s` for the key/token.
#'
#' @return A character string for the Authorization header, or NULL if no auth.
#' @keywords internal
bunddev_auth_header <- function(api, token = NULL) {

  auth <- bunddev_auth_get(api)
  if (auth$type == "none") {
    return(NULL)
  }

  if (is.null(token)) {
    token <- bunddev_auth_token(api)
  }

  scheme <- if (!is.null(auth$scheme) && !is.na(auth$scheme) && auth$scheme != "") {
    auth$scheme
  } else {
    "X-API-Key"
  }

  # Support template-style schemes (e.g., 'OAuth oauth_consumer_key="%s"')
  if (grepl("%s", scheme, fixed = TRUE)) {
    sprintf(scheme, token)
  } else {
    paste0(scheme, " ", token)
  }
}

#' Fetch OAuth2 token for an API
#'
#' @param api Registry id.
#'
#' @details
#' Fetches an OAuth2 access token using the client credentials flow.
#' Returns NULL if no client secret is available (will fall back to API key auth).
#'
#' @return OAuth token string, or NULL if unavailable.
#' @keywords internal
bunddev_oauth_token <- function(api) {
  auth <- bunddev_auth_get(api)

  if (auth$type != "oauth2") {
    return(NULL)
  }

  # Get client ID
  client_id <- NULL
  if (!is.na(auth$env_var) && auth$env_var != "") {
    client_id <- Sys.getenv(auth$env_var)
  }
  if (is.null(client_id) || client_id == "") {
    client_id <- auth$oauth_default_id
  }
  if (is.null(client_id) || client_id == "") {
    cli::cli_abort("OAuth2 client ID not configured for '{api}'.")
  }

  # Get client secret
  if (is.null(auth$oauth_secret_env) || is.na(auth$oauth_secret_env)) {
    return(NULL)  # No secret, will fall back to API key
  }
  client_secret <- Sys.getenv(auth$oauth_secret_env)
  if (client_secret == "") {
    return(NULL)  # No secret, will fall back to API key
  }

  # Fetch OAuth token
  resp <- tryCatch({
    httr2::request(auth$oauth_url) |>
      httr2::req_method("POST") |>
      httr2::req_body_form(
        client_id = client_id,
        client_secret = client_secret,
        grant_type = "client_credentials"
      ) |>
      httr2::req_perform()
  }, error = function(e) NULL)

  if (is.null(resp)) {
    return(NULL)
  }

  raw_body <- httr2::resp_body_raw(resp)
  text <- rawToChar(raw_body)

  # Try to parse JSON response
  if (jsonlite::validate(text)) {
    parsed <- jsonlite::fromJSON(text)
    token <- parsed$access_token %||% parsed$token
    if (!is.null(token) && token != "") {
      return(token)
    }
  }

  # Fallback: extract token from text response
  token <- stringr::str_extract(text, "[A-Za-z0-9-_]{200,}")
  if (!is.na(token) && token != "") {
    return(token)
  }

  NULL
}

#' Get OAuth2 client ID for an API
#'
#' @param api Registry id.
#'
#' @details
#' Returns the OAuth2 client ID from env var or default config.
#'
#' @return Client ID string.
#' @keywords internal
bunddev_oauth_client_id <- function(api) {
  auth <- bunddev_auth_get(api)

  if (auth$type != "oauth2") {
    cli::cli_abort("API '{api}' is not configured for OAuth2.")
  }

  # Get client ID from env var
  if (!is.na(auth$env_var) && auth$env_var != "") {
    client_id <- Sys.getenv(auth$env_var)
    if (client_id != "") {
      return(client_id)
    }
  }

  # Fall back to default
  if (!is.null(auth$oauth_default_id) && auth$oauth_default_id != "") {
    return(auth$oauth_default_id)
  }

  cli::cli_abort("OAuth2 client ID not configured for '{api}'.")
}
