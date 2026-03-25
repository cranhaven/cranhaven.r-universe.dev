
is_testing <- function() {
    identical(Sys.getenv("TESTTHAT"), "true")
}

secret_decrypt_json <- function(path, key) {
    raw <- readBin(path, "raw", file.size(path))
    enc <- rawToChar(raw)
    invisible(secret_decrypt(enc, key = key))
}

# secret_encrypt_json <- function(json, path = NULL, key) {
#     if (!jsonlite::validate(json)) {
#         json <- readChar(json, file.info(json)$size - 1)
#     }
#     enc <- secret_encrypt(json, key = key)
#
#     if(!is.null(path)) {
#         is_string(path)
#         writeBin(enc, path)
#     }
#
#     invisible(enc)
# }

check_date <- function(date, can_be_null = FALSE, arg = caller_arg(date), error_call = caller_env()) {

    check_required(date, arg = arg, call = error_call)

    parsed_date <- ymd(date, quiet = TRUE)
    if (any(length(parsed_date) > 1,
            is_na(parsed_date),
            (length(parsed_date) == 0 && !can_be_null))) {
        khis_abort(
            message = c(
                "x" = "{.arg {arg}} has incorrect format",
                "!" = "Provide the date in the format {.code yyyy-mm-dd}"
            ),
            class = 'khis_invalid_date',
            call = error_call
        )
    }
}

check_scalar_character <- function(x, arg = caller_arg(x), call = caller_env()) {

    check_required(x, arg, call)

    if (!is_scalar_character(x) || is_empty(x)) {
        khis_abort(c('x' = '{.arg {arg}} should be scalar string'),
                   call = call)
    }
}

check_string_vector <- function(vec, arg = caller_arg(vec), call = caller_env()) {

    check_required(vec, arg = arg, call = call)

    is_string <- is_character(vec)
    no_null_na <- !any(is_null(vec) | is.na(vec))
    not_empty <- vec != ""

    if (!all(is_string, no_null_na, not_empty)) {
        khis_abort(
            message = c(
                "x" = "{.arg {arg}} contains invalid values",
                "!" = "Provide values without {.code NA}, {.code NULL} or empty string"
            ),
            call = call
        )
    }
}

check_integerish <- function(vec, arg = caller_arg(vec), call = caller_env()) {
    check_required(vec, arg = arg, call = call)

    is_integer <- is_scalar_integerish(vec)
    no_null_na <- !any(is_null(vec) | is.na(vec))

    if (!all(is_integer, no_null_na)) {
        khis_abort(
            message = c(
                "x" = "{.arg {arg}} is not an integer",
                "!" = "Provide scalar integer value without {.code NA}, {.code NULL}"
            ),
            call = call
        )
    }
}

check_level_supported <- function(level, auth = NULL, arg = caller_arg(level), call = caller_env()) {

    org_levels <- get_organisation_unit_levels(fields = "name,level", auth = auth, call = call)

    if (!level %in% org_levels$level) {
        khis_abort(
            c(
                'x' = "Invalid level specified",
                "The organisation level is invalid"
            ),
            call = call
        )
    }
    return(org_levels)
}

check_supported_operator <- function(x, arg = caller_arg(x), call = caller_env()) {
    symbols <- c(
        'eq', '!eq', 'ieq', 'ne',
        'like', '!like', '$like', '!$like', 'like$', '!like$',
        'ilike', '!ilike', '$ilike', '!$ilike', 'ilike$', '!ilike$',
        'gt', 'ge', 'lt', 'le',
        'null', '!null', 'empty',
        'token', '!token',
        'in', '!in'
    )

    if (!(x %in% symbols)) {
        khis_abort(
            message = c(
                "x" = "{.arg {arg}} is not a supported operator"
            ),
            call = call
        )
    }
}

check_has_credentials <- function(auth = NULL, call = caller_env()) {
    if (!.khis_has_cred(auth)) {
        khis_abort(
            message = c(
                "x" = "Missing credentials",
                "!" = "Please set the credentials by calling {.fun khis_cred}"
            ),
            class = 'khis_missing_credentials',
            call = call
        )
    }
}

#' Check if URL is valid
#'
#' This function checks whether the given URL is a valid scalar character and contains a valid scheme (http or https).
#'
#' @param url The URL to validate.
#' @param arg The argument name to use in error messages. Defaults to the calling argument.
#' @param call The calling environment for error reporting. Defaults to the calling environment.
#'
#' @return Returns `TRUE` if the URL is valid; otherwise, it raises an error.
#'
#' @noRd
check_is_valid_url <- function(url, arg = caller_arg(url), call = caller_env()) {

    # Check if the URL is missing or not a valid scalar character
    if (is_missing(url) || !is_scalar_character(url) || nchar(url) == 0) {
        khis_abort(
            message = c(
                'x' = 'Missing or invalid URL',
                '!' = '{.arg {arg}} must be a non-empty string in the format of "https://dhis2-instance"'
            ),
            call = call
        )
    }

    # Parse the URL
    parsed_url <- url_parse(url)

    # Check for valid scheme (http or https) and presence of host
    if (is.null(parsed_url$scheme) || !parsed_url$scheme %in% c('http', 'https') || is.null(parsed_url$host)) {
        khis_abort(
            message = c(
                'x' = 'Invalid URL scheme',
                '!' = '{.arg {arg}} must start with "http" or "https" and contain a valid host, e.g., "https://dhis2-instance".'
            ),
            call = call
        )
    }

    # If everything is valid, return TRUE
    return(TRUE)
}
