
#' Class ArmadilloDriver with constructor armadillo
#'
#' An Armadillo DataSHIELD Service Driver implementing the DataSHIELD Interface
#' (DSI) \code{\link[DSI]{DSDriver-class}}. This class should always be initialized
#' with the \code{\link{armadillo}} function.
#' It returns a singleton that allows you to connect to Armadillo
#'
#' @importClassesFrom DSI DSDriver
#' @export
#' @keywords internal
methods::setClass("ArmadilloDriver", contains = "DSDriver")

#' Create an Armadillo DataSHIELD Service driver
#'
#' Convenience function for creating a [ArmadilloDriver] object.
#'
#' @export
armadillo <- function() {
  methods::new("ArmadilloDriver")
}

#' Connect to an Armadillo DataSHIELD service
#'
#' Connect to an Armadillo DataSHIELD service, with provided credentials.
#'
#' @param drv \code{\link{ArmadilloDriver-class}} class object.
#' @param name Name of the connection, which must be unique among all the
#' DataSHIELD connections.
#' @param restore Workspace name to be restored in the newly created DataSHIELD
#' R session.
#' @param username The username to authenticate with.
#' @param password The password to authenticate with.
#' @param token The ID token to authenticate with.
#' @param url URL of the server.
#' @param profile the profile to select, default "default"
#' @param opts Curl options as described by httr (call httr::httr_options()
#' for details). Can be provided by "Armadillo.opts" option.
#' @param ... Unused, needed for compatibility with generic.
#'
#' @return A \code{\link{ArmadilloConnection-class}} object.
#'
#' @importMethodsFrom DSI dsConnect
#' @importFrom base64enc base64encode
#' @importFrom stringr str_length str_remove_all
#' @export
methods::setMethod(
  "dsConnect", "ArmadilloDriver",
  function(drv, name, restore = NULL, username = "", password = "",
           token = "", url, profile = "default", opts = list(), ...) {
    handle <- httr::handle(url)

    if (stringr::str_length(username) > 0) {
      if (stringr::str_length(password) == 0) {
        stop("Please provide a password with the username")
      }
      encoded <- base64enc::base64encode(
        charToRaw(paste0(username, ":", password))
      )
      auth_header <-
        httr::add_headers("Authorization" = paste0("Basic ", encoded))
    } else {
      if (stringr::str_length(token) == 0) {
        stop("Please provide a token or a username / password combination")
      }
      auth_header <-
        httr::add_headers("Authorization" = paste0("Bearer ", token))
    }

    profile_to_select <- if (profile == "") "default" else profile

    response <- httr::POST(
      handle = handle,
      path = "/select-profile",
      body = profile_to_select,
      config = auth_header
    )
    .handle_request_error(response)
    if (response$status == 404 && profile_to_select != "default") {
      stop(paste0("Profile not found: '", profile, "'"))
    }
    cookies <- httr::cookies(response)

    # Restore users workspace
    if (!is.null(restore)) {
      restore_response <- httr::POST(
        handle = handle,
        query = list(id = restore),
        path = "/load-workspace",
        config = auth_header
      )
      .handle_request_error(restore_response)
    }

    methods::new("ArmadilloConnection",
      name = name,
      handle = handle,
      user = username,
      cookies = cookies,
      token = token
    )
  }
)

#' Get driver info
#'
#' Get information about a driver.
#'
#' @param dsObj \code{\link{ArmadilloDriver-class}} class object
#' @param ... Unused, needed for compatibility with generic.
#'
#' @return The connection information. This returns the version of the
#' package (`driver.version`). There is no underlying client library.
#'
#' @importMethodsFrom DSI dsGetInfo
#' @importFrom utils packageVersion
#' @export
methods::setMethod(
  "dsGetInfo", "ArmadilloDriver",
  function(dsObj, ...) { # nolint
    return(list(
      driver.version = packageVersion("DSMolgenisArmadillo")
    ))
  }
)
