#' Get ID Token
#'
#' Get an ID token to log in on an Armadillo server
#'
#' @param server the URL of the Armadillo server
#'
#' @return The ID token string
#'
#' @importFrom MolgenisAuth discover device_flow_auth
#'
#' @export
armadillo.get_token <- function(server) { # nolint
  lifecycle::deprecate_warn("3.0.0", "armadillo.get_token()", "armadillo.get_credentials()")
  credentials <- armadillo.get_credentials(server)
  return(credentials@id_token)
}

#' @title ArmadilloCredentials Class
#' @description An S4 class to represent authentication credentials used for accessing Armadillo servers.
#' @slot access_token Character. The access token used for authentication.
#' @slot expires_in Numeric. The number of seconds until the token expires.
#' @slot expires_at POSIXct. The timestamp when the token expires.
#' @slot id_token Character. The ID token containing identity information.
#' @slot refresh_token Character. The token used to obtain a new access token when expired.
#' @slot token_type Character. The type of token (typically "Bearer").
#' @slot auth_type Character. the authentication provider type (e.g., "keycloak", "fusionauth").
#' @keywords internal
#' @export
setClass(
  "ArmadilloCredentials",
  slots = list(
    access_token = "character",
    expires_in = "numeric",
    expires_at = "POSIXct",
    id_token = "character",
    refresh_token = "character",
    token_type = "character",
    auth_type = "character")
)


#' Get credentials information
#'
#' @details This function fetched various details from the auth server, including refresh, id and
#' access tokens. NOTE: in order to get refresh token, refresh tokens should be turned on for this
#' armadillo instance on the auth server. When access tokens are fetched using this method, it
#'enables the automatic refreshing of tokens when they time out. This works by checking whether the
#'current token(s) have expired, and if so attempting to refresh them and write the refreshed
#' token to the connection and credentials objects in the global environment. For this to work
#' correctly, ensure that there only exists one DataSHIELD connections object in the global
#' environment, and that for each datasource there is only object containing their credentials.
#'
#' @param server the URL of the Armadillo server
#' @return The credentials
#' @importFrom MolgenisAuth discover device_flow_auth
#' @importFrom methods new
#' @examples
#' \dontrun{
#' example_url <- "https://armadillo.example.org"
#' credentials <- armadillo.get_credentials(example_url)
#'
#' builder <- newDSLoginBuilder()
#' builder$append(
#'   url = example_url,
#'   server = "example_server",
#'   token = credentials@access_token,
#'   driver = "ArmadilloDriver")
#' logindata <- builder$build()
#'
#' conns <- datashield.login(logins = logindata, assign = FALSE)
#' }
#' @export
armadillo.get_credentials <- function(server) { # nolint
  auth_info <- .get_oauth_info(server)$auth
  endpoint <- discover(auth_info$issuerUri)
  credentials <- device_flow_auth(
    endpoint,
    auth_info$clientId
  )

  credentials_obj <- new("ArmadilloCredentials",  access_token = credentials$access_token,
                         expires_in =  credentials$expires_in,
                         expires_at = Sys.time() + credentials$expires_in,
                         id_token =  credentials$id_token,
                         refresh_token =  credentials$refresh_token,
                         token_type =  credentials$token_type,
                         auth_type = ifelse(grepl("realms", endpoint$authorize), "keycloak", "fusionauth")
  )

  return(credentials_obj)
}

#' Refresh OAuth token using FusionAuth
#'
#' Attempts to refresh the OAuth access token using the FusionAuth refresh endpoint.
#' It uses the current `access_token` and `refresh_token` from the provided credentials,
#' and performs a POST request to the FusionAuth server.
#'
#' @param server Character. The URL of the Armadillo server.
#' @param credentials An `ArmadilloCredentials` S4 object containing the current tokens.
#' @return A list containing the new credentials returned by the refresh endpoint.
#' @importFrom httr POST set_cookies content handle
#' @noRd
.refresh_token <- function(server, credentials) {
  message("\nAttempting refresh...")
  # get auth url
  auth_info <- .get_oauth_info(server)

if(credentials@auth_type == "fusionauth") {

  # post to fusionauth refresh endpoint with current access/refresh tokens
  fusionAuthRefreshUri <- paste0(auth_info$auth$issuerUri, "/api/jwt/refresh")
  response <- httr::POST(
    url = fusionAuthRefreshUri,
    handle=handle(''),
    config=httr::set_cookies(refresh_token=credentials@refresh_token, access_token=credentials@access_token)
    )

  content <- content(response)

  if (!is.null(content$fieldErrors)){
    stop(paste0(" ", unlist(content$fieldErrors)))
  } else if (is.null(content$token)) {
    stop("Refresh failed")
  }

  new_credentials <- new("ArmadilloCredentials",
                         access_token = content$token,
                         expires_in = as.numeric(.get_updated_expiry_date(auth_info, content$token) - Sys.time()),
                         expires_at = .get_updated_expiry_date(auth_info, content$token),
                         id_token = credentials@id_token,
                         refresh_token = content$refreshToken,
                         token_type = credentials@token_type,
                         auth_type = credentials@auth_type)

} else {
  keyCloakRefreshUri <- paste0(auth_info$auth$issuerUri, "/protocol/openid-connect/token")
  response <- httr::POST(
    url = keyCloakRefreshUri,
    body = list(
      grant_type = "refresh_token",
      refresh_token = credentials@refresh_token,
      client_id = auth_info$auth$clientId
    ),
    encode = "form"
  )

  content <- content(response)

  if (!is.null(content$fieldErrors)){
    stop(paste0(" ", unlist(content$fieldErrors)))
  } else if (is.null(content$access_token)) {
    stop("Refresh failed")
  }

  new_credentials <- new("ArmadilloCredentials",
                          access_token = content$access_token,
                          expires_in = content$expires_in,
                          expires_at = Sys.time() + content$expires_in,
                          id_token = credentials@id_token,
                          refresh_token = credentials@refresh_token,
                          token_type = credentials@token_type,
                          auth_type = credentials@auth_type)

}

  message("Refresh successful")
  return(new_credentials)
}

#' Get updated token expiry date from FusionAuth
#'
#' Sends a request to FusionAuth's `/api/jwt/validate` endpoint to retrieve
#' and return the updated expiry time of the JWT access token.
#'
#' @param auth_info A list containing authentication metadata, including `auth$issuerUri`.
#' @param credentials An object with an `@access_token` slot containing the JWT.
#'
#' @return A POSIXct object representing the token's expiry time.
#' @keywords internal
#' @noRd
.get_updated_expiry_date <- function(auth_info, token) {
  validate_url <- paste0(auth_info$auth$issuerUri, "/api/jwt/validate")
  response <- httr::GET(
    url = validate_url,
    httr::add_headers(Authorization = paste("Bearer", token))
    )
  return(as.POSIXct(content(response)$jwt$exp))
}

#' Get oauth server discovery information
#'
#' Specifically this method returns the
#' - issuer URL
#' - clientId
#'
#' @param armadillo_server url of the Armadillo server
#'
#' @importFrom httr GET stop_for_status content
#' @importFrom urltools path
#'
#' @return a dataframe with issuerUrl and clientId
#'
#' @noRd
.get_oauth_info <- function(armadillo_server) {
  info_url <- armadillo_server
  urltools::path(info_url) <- "actuator/info"
  response <- httr::GET(info_url)
  httr::stop_for_status(response, task = "fetch server info")
  return(httr::content(response))
}

#' @title Reset Armadillo Token if Expired with tryCatch
#' @description Tries to refresh token and not break things if this fails
#' @param conn A `DSConnection` object.
#' @param env The DataSHIELD connection environment
#' @return The updated `DSConnection` object with a refreshed token if needed.
#' @keywords internal
#' @noRd
.refresh_token_safely <- function(conn, env = getOption("datashield.env", globalenv())) {
  tryCatch({
    new_conn <- .reset_token_if_expired(conn, env)
    if (inherits(new_conn, "ArmadilloConnection")) {
      return(new_conn)
      } else {
        return(conn)
      }
  }, error = function(e) {
    warning("Failed to reset token: ", e$message, call. = FALSE)
    conn
  })
}

#' @title Reset Armadillo Token if Expired
#' @description Checks if the current token has expired and refreshes it if necessary.
#' @param conn A `DSConnection` object.
#' @return The updated `DSConnection` object with a refreshed token if needed.
#' @keywords internal
#' @noRd
.reset_token_if_expired <- function(conn, env = getOption("datashield.env", globalenv())) {
  credentials <- .get_armadillo_credentials(conn)
  if(is.null(credentials)) {
    return(NULL)
  } else if(credentials$object@expires_at < Sys.time()) {
    .check_multiple_conns(env)
    new_credentials <- .refresh_token(conn@handle$url, credentials$object)
    conn@token <- new_credentials@access_token
    .reset_token_global_env(credentials, new_credentials, conn)
    return(conn)
    } else {
    return(NULL)
  }
}

#' Check for multiple DataSHIELD connections
#'
#' Checks whether multiple DataSHIELD connection objects exist in the environment,
#' and if so, issues a warning if token refresh is not possible.
#'
#' @param env Environment where DataSHIELD connections are stored.
#'
#' @return A warning if multiple connections are found; otherwise, nothing is returned.
#' @keywords internal
#' @noRd
.check_multiple_conns <- function(env){
  multiple_conns <- .getDSConnectionsMod(env)$flag == 2
if(multiple_conns) {
  stop("Token has expired however it was not possible to refresh token because multiple DataSHIELD connection objects found in environment. Run ?armadillo.get_credentials for more details")
}
}

#' @title Get Armadillo Credentials
#' @description Retrieves the Armadillo credentials that match the provided connection.
#' @param conn A `DSConnection` object.
#' @param env The environment where credentials are stored. Defaults to `globalenv()` or `getOption("datashield.env")`.
#' @return A list with the name and object of the matched credentials, or `NULL` if no match is found.
#' @importFrom DSI datashield.connections_find
#' @keywords internal
#' @noRd
.get_armadillo_credentials <- function(conn, env = getOption("datashield.env", globalenv())) {
  all_credentials <- .get_all_armadillo_credentials(env)
  if(is.null(all_credentials)) {
    return(NULL)
  } else {
    .get_matching_credential(all_credentials, conn)
  }
}

#' @title Get All Armadillo Credentials
#' @description Scans the specified environment for objects of class `"ArmadilloCredentials"`.
#' @param env The environment to search. Defaults to `globalenv()` or `getOption("datashield.env")`.
#' @return A named list of `"ArmadilloCredentials"` objects.
#' @keywords internal
#' @noRd
.get_all_armadillo_credentials <- function(env = getOption("datashield.env", globalenv())) {
  objs <- ls(envir = env)
  conns <- sapply(objs, function(x) {
    obj <- get(x, envir = env)
    inherits(obj, "ArmadilloCredentials")
  }, USE.NAMES = TRUE)
  if(length(objs) == 0 | length(conns) == 0) {
    return(NULL)
  }
  matched <- objs[conns]
  if(is.character(matched) && length(matched) == 0) {
    return(NULL)
  } else {
    return(mget(matched, envir = env))
  }
}

#' @title Get Matching Credential
#' @description Matches a token from a connection with stored credentials.
#' @param credentials A named list of `"ArmadilloCredentials"` objects.
#' @param conn A `DSConnection` object containing the token to match.
#' @return A list with the name and object of the matched credential, or `NULL` if not found.
#' @keywords internal
#' @noRd
.get_matching_credential <- function(credentials, conn) {
  target_token <- conn@token

  for (name in names(credentials)) {
    cred <- credentials[[name]]
    if (inherits(cred, "ArmadilloCredentials") && cred@access_token == target_token) {
      return(list(name = name, object = cred))
    }
  }

  return(NULL)
}


#' @title Reset Armadillo Credentials
#' @description Updates the stored credentials in the environment with refreshed tokens.
#' @param old_credentials A list with name and object of the old credentials.
#' @param new_credentials A list with new token and refreshToken values.
#' @param env The environment where credentials are stored. Defaults to `globalenv()` or `getOption("datashield.env")`.
#' @return None. Used for side-effects.
#' @keywords internal
#' @noRd
.reset_armadillo_credentials <- function(old_credentials, new_credentials, env = getOption("datashield.env", globalenv())) {
  credentials_to_return <- old_credentials$object
  credentials_to_return@access_token <- new_credentials@access_token
  credentials_to_return@refresh_token <- new_credentials@refresh_token
  credentials_to_return@expires_at <- new_credentials@expires_at
  assign(old_credentials$name, credentials_to_return, envir = env)
  message(paste0("Token reset in connections object ", "'", old_credentials$name, "'", "in ", format(env)))
}

#' @title Reset Connections Object
#' @description Updates the token in the connections object to reflect refreshed credentials.
#' @param old_credentials A list with the old credentials.
#' @param new_credentials A list with new token values.
#' @param conn A `DSConnection` object.
#' @param env The environment containing the connections object. Defaults to `globalenv()` or `getOption("datashield.env")`.
#' @return None. Used for side-effects.
#' @keywords internal
#' @noRd
.reset_connections_object <- function(old_credentials, new_credentials, conn, env=getOption("datashield.env", globalenv())) {
  old_conns <- .getDSConnectionsMod(env)
  old_conns$flag <- NULL
  conns_to_return <- old_conns[[1]]

  for (i in seq_along(conns_to_return)) {
    if (methods::slot(conns_to_return[[i]], "token") == old_credentials$object@access_token) {
      methods::slot(conns_to_return[[i]], "token") <- new_credentials@access_token
      break  # stop after first match
    }
  }
  assign(names(old_conns), conns_to_return, envir = env)
  message(paste0("Token reset in credentials object ", "'", names(old_conns), "'", "in ", format(env)))
}

#' @title Reset Token in Global Environment
#' @description Updates both the Armadillo credentials and the connection object in the global environment.
#' @param old_credentials A list with the old credentials.
#' @param new_credentials A list with new token and refreshToken values.
#' @param conn A `DSConnection` object.
#' @param env The environment where updates should be made. Defaults to `globalenv()` or `getOption("datashield.env")`.
#' @return None. Used for side-effects.
#' @keywords internal
#' @noRd
.reset_token_global_env <- function(old_credentials, new_credentials, conn, env=getOption("datashield.env", globalenv())) {
  .reset_armadillo_credentials(old_credentials, new_credentials, env)
  .reset_connections_object(old_credentials, new_credentials, conn, env)
  .reset_connections_object(old_credentials, new_credentials, conn, sys.frame(1))
}

#' Detect DataSHIELD Connections in an Environment
#'
#' Scans a given environment for DataSHIELD connection objects. Slightly modified from original version in DSI
#'
#' @param env An environment object to search for DataSHIELD connections.
#'
#' @return A list with:
#' \describe{
#'   \item{flag}{Indicator of result type:
#'     \itemize{
#'       \item 0: No connections found.
#'       \item 1: One connection list found.
#'       \item 2: Multiple connection lists found.
#'     }}
#'   \item{conns}{Either the connection list (if one found) or a character vector with the names of multiple connection lists.}
#' }
#'
#' @keywords internal
#' @noRd
.getDSConnectionsMod <- function(env) {
  symbols <- base::ls(name=env)
  if (length(symbols) > 0) {
    connlist <- c()
    flag <- 0
    for (i in 1:length(symbols)) {
      obj <- base::get(symbols[i], envir = env)
      if ("list" %in% class(obj)) {
        if (length(obj) > 0) {
          if (.isDSConnection(obj[[1]])) {
            connlist <- append(connlist, symbols[i])
            flag <- 1
          }
        }
      }
    }
    if (flag == 1) {
      if (length(connlist) > 1) {
        flag <- 2
        return(list(flag=flag, conns=connlist))
      } else {
        pp <- connlist[[1]]
        conns <- base::get(pp, envir = env)
        conns_to_return <- list(flag=flag, conns=conns)
        names(conns_to_return) <- c("flag", pp)
        return(conns_to_return)
      }
    }
  }
  return(list(flag=0, conns=NULL))
}

#' Check if provided object is a S4 class instance and if this class inherits from \code{\link{DSConnection-class}}.
#' @keywords internal
#' @importFrom methods getClass
#' @noRd
.isDSConnection <- function(obj) {
  if (isS4(obj)) {
    cls <- getClass(class(obj)[[1]])
    "DSConnection" %in% names(cls@contains)
  } else {
    FALSE
  }
}
