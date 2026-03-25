# This file is the interface between DHIS2 credential storage.

# Initialization happens in .onLoad
.auth <- NULL

#' Sets DHIS2 Credentials
#'
#' `khis_cred()` sets the credentials for accessing a DHIS2 instance.
#'
#' @param username The DHIS2 username. Only required if configuration file not
#'   provided.
#' @param password The DHIS2 password. Only required if configuration file not
#'   provided.
#' @param server The server URL of the DHIS2 instance. Only required if configuration
#'   file not provided.
#' @param api_version The API version of the DHIS2 instance (optional).
#' @param config_path An optional path to a configuration file containing username
#'   and password. This is considered more secure than providing credentials directly
#'   in code.
#' @param base_url Deprecated. The base URL of the DHIS2 instance. Use `server` instead.
#'
#' @family credential functions
#'
#' @return Auth object
#'
#' @export
#'
#' @details
#' This function allows you to set the credentials for interacting with a DHIS2
#' server. You can either provide the username and password directly (less secure)
#' or specify a path to a configuration file containing these credentials. Using
#' a configuration file is recommended for improved security as it prevents
#' credentials from being stored directly in your code.
#'
#' @examples
#'
#' \dontrun{
#'     # Option 1: Using a configuration file (recommended)
#'     # Assuming a configuration file named "credentials.json":
#'     khis_cred(config_path = "path/to/credentials.json")
#'
#'     # Option 2: Providing credentials directly (less secure)
#'     khis_cred(username = "your_username",
#'               password = "your_password",
#'               base_url='https://dhis2-instance/api')
#' }

khis_cred <- function(username = NULL,
                      password = NULL,
                      server = NULL,
                      api_version = NULL,
                      config_path = NULL,
                      base_url = deprecated()) {

    # Ensure either config_path or credentials are provided
    if (is.null(config_path) && (is.null(username) || is.null(password))) {
        khis_abort(
            message = c(
                'x' = 'Missing credentials',
                '!' = 'Please provide either a valid {.arg config_path} or both {.arg username} and {.arg password}.'
            ),
            class = 'khis_missing_credentials'
        )
    }

    # Prevent simultaneous use of config_path and direct credentials
    if (!is.null(config_path) && (!is.null(username) || !is.null(password))) {
        khis_abort(
            message = c(
                "x" = "Conflicting credentials input.",
                "!" = "You cannot provide both {.arg config_path} and {.arg username} or {.arg password}. Use only one method."
            ),
            class = 'khis_multiple_credentials'
        )
    }

    # Deprecation warning for base_url and extract server if necessary
    if (is_present(base_url)) {
        lifecycle::deprecate_warn('1.0.6', 'khis_cred(base_url)', 'khis_cred(server)')
        server <- str_remove(base_url, 'api')
    }

    # Load credentials from config file if provided
    if (!is.null(config_path)) {
        credentials <- .load_config_file(config_path)
        password <- credentials[["password"]]
        username <- credentials[["username"]]
        server <- credentials[["server"]]
    }

    # validate username and password
    if (!is_scalar_character(password) || nchar(password) == 0 ||
        !is_scalar_character(username) || nchar(username) == 0) {
        khis_abort(
            message = c(
                "x" = "Invalid credentials",
                "!" = "Both {.arg username} and {.arg password} must be valid non-empty strings."
            ),
            class = 'khis_invalid_credentials'
        )
    }

    if (is_null(server)) {
        lifecycle::deprecate_warn(
            when = "1.0.6",
            what = "khis_cred(base_url)",
            details = "The use of a default URL (`https://hiskenya.org/api`) when neither `server` nor `base_url` is provided is deprecated. Please provide an explicit `server` URL."
        )
        server <- 'https://hiskenya.org'
    }

    # Validate the server URL
    check_is_valid_url(server)

    # Set the credentials in the .auth object
    .auth$set_username(username)
    .auth$set_password(password)
    .auth$set_base_url(server)

    # Attempt to fetch user profile
    user_profile <- tryCatch(
        get_user_profile(),
        error = function(e) {
            khis_cred_clear()
            khis_abort(
                message = c(
                    'x' = "Failed to retrieve user profile.",
                    '!' = 'Error Message: {.msg {e$message}}',
                    'i' = "Please check your credentials or server connection and try again."
                ),
                call = caller_env(n = 4),
                class = 'khis_credentials_error'
            )
        }
    )

    # Initialize and set user profile if successful
    profile <- init_Profile(
        user_profile[['id']],
        user_profile[['username']],
        user_profile[['email']],
        user_profile[['phoneNumber']],
        user_profile[['displayName']],
        user_profile[['firstName']],
        user_profile[['lastName']]
    )

    .auth$set_profile(profile)

    khis_info(c('i' = 'The credentials have been set.'))

    invisible(.auth)
}

#' Load Configuration File
#'
#' Loads a JSON configuration file containing credentials for accessing DHIS2 instance.
#'
#' @param config_path Path to the DHIS2 credentials file.
#'
#' @return A parsed list of the credentials in the configuration file.
#'
#'@noRd

.load_config_file <- function(config_path = NA, call = caller_env()) {
    # Load from a file
    tryCatch({
        # Read JSON data from the file
        data <- jsonlite::fromJSON(config_path)

        # Ensure the 'credentials' object exists in the JSON file
        if (!is.null(data) && 'credentials' %in% names(data)) {
            return(data[['credentials']])
        }

        # Throw error if credentials are missing from the file
        khis_abort(
            message = c(
                "x" = "Credentials missing from in configuration file.",
                "!" = "Ensure the file contains a valid 'credentials' object."
            ),
            class = 'khis_invalid_config_file',
            call = call
        )
    }, error = function(e) {
        # Handle file or JSON parsing errors
        khis_abort(
            message = c(
                "x" = "Invalid configuration file path or format.",
                "!" = "Check the {.arg config_path} and ensure the file is valid JSON."
            ),
            class = 'khis_invalid_config_path',
            call = call
        )
    })
}

#' Authenticate HTTP Request with Basic Authentication
#'
#' This function sets the Authorization header for HTTP basic authentication using
#' the provided credentials (username and password). If the credentials are not explicitly
#' provided, it defaults to the global auth credentials.
#'
#' @param req An HTTP request object created by [httr2::request].
#' @param auth (Optional) An auth object containing the username and password.
#' If not provided, the function uses global auth credentials.
#' @param arg The argument name used in error messages (defaults to the calling argument).
#' @param call The calling environment for error reporting (defaults to the calling environment).
#'
#' @return A modified HTTP request object with the Authorization header set.
#'
#' @family credential functions
#'
#' @noRd
#'
#' @examples
#'
#' # Example using global credentials
#' req <- request("http://dhis2.com/api") %>%
#'   req_auth_khis_basic()
#'
#' @seealso [httr2::req_auth_basic], [httr2::request]
req_auth_khis_basic <- function(req, auth = NULL, arg = caller_arg(req), call = caller_env()) {

    # Ensure the request object is provided
    check_required(req, arg, call = call)

    # Ensure valid credentials are available
    check_has_credentials(auth = auth, call = call)

    # Extract credentials from the provided AuthCred object or fall back to global .auth credentials
    if (!is.null(auth) && inherits(auth, 'AuthCred')) {
        username <- auth$get_username()
        password <- auth$get_password()
    } else {
        # Fallback to global .auth credentials
        username <- .auth$get_username()
        password <- .auth$get_password()
    }

    # Add basic authentication header to the request
    req_auth_basic(req, username, password)
}

#' Check if DHIS2 Credentials are Available
#'
#' This function checks whether valid credentials are available either in the provided
#' auth object or in the global auth credentials object.
#'
#' @family credential functions
#'
#' @param auth (Optional) An auth object containing DHIS2 credentials. If not provided,
#' the function will check the global auth object for credentials.
#'
#' @return A boolean value indicating whether valid credentials are available.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'     # Set the credentials using global .auth object
#'     khis_cred(username = 'DHIS2 username',
#'               password = 'DHIS2 password',
#'               server = 'https://dhis2-instance/api')
#'
#'     # Check if credentials are available. Should return TRUE
#'     khis_has_cred()
#'
#'     # Clear global credentials
#'     khis_cred_clear()
#'
#'     # Check if credentials are available. Should return FALSE
#'     khis_has_cred()
#' }
khis_has_cred <- function(auth = NULL) {
    # If auth is provided and is an AuthCred object, check its credentials
    if (!is.null(auth) && inherits(auth, 'AuthCred')) {
        return(auth$has_valid_cred() && .khis_has_cred(auth))
    }

    # Fallback to the global .auth object
    .auth$has_valid_cred() && .khis_has_cred()
}

#' Internal Helper Function to Check if Credentials Exist
#'
#' This internal function checks whether credentials exist either in the provided
#' auth object or in the global auth object.
#'
#' @param auth (Optional) An auth object. If not provided, the function will
#' check the global auth object for credentials.
#'
#' @family credential functions
#'
#' @return A boolean value indicating whether credentials are present.
#'
#' @noRd
.khis_has_cred <- function(auth = NULL) {
    # If auth is provided and is an AuthCred object, check its credentials
    if (!is.null(auth) && inherits(auth, 'AuthCred')) {
        return(auth$has_cred())
    }

    # Fallback to the global .auth object
    .auth$has_cred()
}


#' Clear the Credentials from Memory
#'
#' This function clears the DHIS2 credentials from memory. If an auth object is
#' provided, it clears the credentials from that object. If no `auth` object is
#' provided, it clears the global auth credentials.
#'
#' @family credential functions
#'
#' @param auth (Optional) An authentication object from which to clear credentials.
#' If not provided, the credentials in the global auth object will be cleared.
#'
#' @return No return value, called for side effects.
#'
#' @export
#'
#' @examples
#'
#' # Clear credentials from the global .auth object
#' khis_cred_clear()

khis_cred_clear <- function(auth = NULL) {

    # Clear credentials from the provided AuthCred object, if available
    if (!is.null(auth) && inherits(auth, 'AuthCred')) {
        auth$set_username(NULL)
        auth$clear_password()
        auth$set_base_url(NULL)
        auth$set_profile(NULL)
        return(invisible(NULL))
    }

    # Fallback to clearing the global .auth credentials
    .auth$set_username(NULL)
    .auth$clear_password()
    .auth$set_base_url(NULL)
    .auth$set_profile(NULL)

    invisible(NULL)
}

#' Retrieve the Configured Username
#'
#' This function returns the username from the configured credentials. If an
#' auth object is provided, it retrieves the username from that object.
#' Otherwise, it retrieves the username from the global auth object.
#'
#' @family credential functions
#'
#' @param auth (Optional) An auth object. If not provided, the function
#'   will retrieve the username from the global auth credentials.
#'
#' @return The username as a string, or `NULL` if no credentials are available.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'     # Set the credentials using global .auth object
#'     khis_cred(username = 'DHIS2 username',
#'               password = 'DHIS2 password',
#'               base_url = 'https://dhis2-instance/api')
#'
#'     # View the username (expect 'DHIS2 username')
#'     khis_username()
#'
#'     # Clear credentials
#'     khis_cred_clear()
#'
#'     # View the username (expect 'NULL')
#'     khis_username()
#' }

khis_username <- function(auth = NULL) {
    # If an AuthCred object is provided, return the username from it
    if (!is.null(auth) && inherits(auth, 'AuthCred')) {
        return(auth$get_username())
    }

    # Fallback to the global .auth object
    .auth$get_username()
}

#' Retrieve the Configured DHIS2 API Base URL
#'
#' This function returns the base URL for the DHIS2 API from the provided auth
#' object, or falls back to the global auth credentials if `auth` is not provided.
#'
#' @param auth (Optional) An auth object containing the DHIS2 credentials.
#' If not provided, the function retrieves the base URL from the global auth object.
#'
#' @return The DHIS2 base URL as a string, or `NULL` if no credentials are available.
#'
#' @family credential functions
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'     # Set the credentials using the global .auth object
#'     khis_cred(username = 'DHIS2 username',
#'               password = 'DHIS2 password',
#'               base_url = 'https://dhis2-instance/api')
#'
#'     # Retrieve the DHIS2 instance API base URL (expect 'https://dhis2-instance/api')
#'     khis_base_url()
#'
#'     # Clear credentials
#'     khis_cred_clear()
#'
#'     # Retrieve the base URL again (expect 'NULL')
#'     khis_base_url()
#' }

khis_base_url <- function(auth = NULL) {
    # If an AuthCred object is provided, return the base URL from it
    if (!is.null(auth) && inherits(auth, 'AuthCred')) {
        return(auth$get_base_url())
    }

    # Fallback to the global .auth object
    return(.auth$get_base_url())
}

#' Retrieve the Configured Display Name
#'
#' This function returns the display name from the configured profile in the provided
#' auth object. If `auth` is not provided, it falls back to the global auth credentials.
#'
#' @param auth (Optional) An auth object containing DHIS2 credentials.
#' If not provided, the function retrieves the display name from the global auth object.
#'
#' @return The display name as a string, or `NULL` if no profile or display name is available.
#'
#' @family credential functions
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'     # Set the credentials using global .auth object
#'     khis_cred(username = 'DHIS2 username',
#'               password = 'DHIS2 password',
#'               base_url = 'https://dhis2-instance/api')
#'
#'     # Retrieve the display name from the global .auth profile
#'     khis_display_name()
#'
#'     # Clear credentials
#'     khis_cred_clear()
#'
#'     # Retrieve the display name again (expect 'NULL')
#'     khis_display_name()
#' }
khis_display_name <- function(auth = NULL) {
    # If an AuthCred object is provided, return the display name from its profile
    if (!is.null(auth) && inherits(auth, 'AuthCred')) {
        return(auth$get_display_name())
    }

    # Fallback to the global .auth object
    if (!is.null(.auth$get_profile())) {
        return(.auth$get_profile()$get_display_name())
    }

    # Return NULL if no profile or display name is available
    return(NULL)
}

#' Internal Credentials for Documentation or Testing
#'
#' This internal function is used to provide credentials for the documentation
#'   or testing environments. It decrypts the necessary credentials and loads them
#'   into the system if available.
#'
#' @param account The environment for which credentials are needed. Must be one of `"docs"` or `"testing"`.
#'
#' @return No return value, called for side effects.
#' @noRd
khis_cred_internal <- function(account = c('docs', 'testing')) {
    # Ensure valid account type
    account <- rlang::arg_match(account, c('docs', 'testing'))

    # Check if the system can decrypt and is online
    can_decrypt <- secret_has_key('KHIS_KEY')
    online <- !is.null(curl::nslookup('google.com', error = FALSE))

    if (!can_decrypt || !online) {
        # Construct error message based on failure type(s)
        error_message <- c("Set credential unsuccessful.")
        if (!can_decrypt) {
            error_message <- c(error_message, "x" = "Unable to decrypt the {.field {account}} credentials. Make sure the encryption key 'KHIS_KEY' is available.")
        }
        if (!online) {
            error_message <- c(error_message, "x" = "No internet connection detected. Please check your connection or ensure DHIS2 is online.")
        }

        khis_abort(
            message = error_message,
            class = 'khis_cred_internal_error',
            can_decrypt = can_decrypt,
            online = online
        )
    }

    # Set credentials quietly if not in an interactive session
    if (!is_interactive()) local_khis_quiet()

    # Decrypt and load the credentials from the appropriate file
    filename <- str_glue("khisr-{account}.json")
    khis_cred(
        config_path = secret_decrypt_json(
            system.file('secret', filename, package = 'khisr'),
            'KHIS_KEY'
        )
    )

    invisible(TRUE)
}

#' Set Credentials for Documentation Environment
#'
#' This function loads credentials for the documentation environment.
#'
#' @noRd
khis_cred_docs <- function() {
    khis_cred_internal('docs')
}

#' Set Credentials for Testing Environment
#'
#' This function loads credentials for the testing environment.
#'
#' @noRd
khis_cred_testing <- function() {
    khis_cred_internal('testing')
}
