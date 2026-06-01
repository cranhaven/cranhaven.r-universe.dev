check_api_key <- function(x, sandbox){

  cls_check(sandbox, "logical")

  if (sandbox) {
    key <- if (is.null(x)) Sys.getenv("TREMENDOUS_TEST_KEY", "") else x
  } else if (!sandbox) {
    key <- if (is.null(x)) Sys.getenv("TREMENDOUS_PROD_KEY", "") else x
  }

  if (key == "") {
    cli::cli_abort("It looks like you're tring to use the Tremendous {if (sandbox) crayon::blue('Sandbox') else crayon::blue('Production')} environment but don't have a {if (sandbox) paste0('Test') else paste0('Production')} API key.")
  } else if (sandbox & !grepl("^TEST", key)) {
    cli::cli_abort("It looks like your Tremendous Test API key is invalid. It should start with {.emph 'TEST'}.
                   Visit {.url https://developers.tremendous.com/} for more details.")
  } else if (!sandbox & !grepl("^PROD", key)) {
    cli::cli_abort("It looks like your Tremendous Production API key is invalid. It should start with {.emph 'PROD'}.
                   Visit {.url https://developers.tremendous.com/} for more details.")
  } else {
    key
  }

}

#' Set Tremendous API Key in .Renviron
#'
#' @param api_key API key from
#'   [tremendous.com](https://developers.tremendous.com/).
#'
#' @return NA; Used to set the Tremendous API key for persistent use in '.Renviron'.
#'
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'   # For Sandbox Environment, your API key should look like:
#'   trem_set_api_key("TEST_YOUR-API-KEY")
#'
#'   # For Production Environment, your API key should look like:
#'   trem_set_api_key("PROD_YOUR-API-KEY")
#' }
#'
trem_set_api_key <- function(api_key) {
  if (grepl("^TEST_", api_key)) {
    if (Sys.getenv("TREMENDOUS_TEST_KEY") == "") {
      auth_message(pw_info = paste0("TREMENDOUS_TEST_KEY=", api_key),
                   key_type = "Tremendous Test API key")
    } else {
      cli::cli_alert_danger("It looks like your Tremendous Test API key, {.envvar TREMENDOUS_TEST_KEY}, is already set.
                     You may manually override it by opening your {.file .Renviron} file.")
    }
  } else if (grepl("^PROD_", api_key)) {
    if (Sys.getenv("TREMENDOUS_PROD_KEY") == "") {
    auth_message(pw_info = paste0("TREMENDOUS_PROD_KEY=", api_key),
                 key_type = "Tremendous Production API key")
    } else {
      cli::cli_alert_danger("It looks like your Tremendous Production API key, {.envvar TREMENDOUS_PROD_KEY}, is already set.
                     You may manually override it by opening your {.file .Renviron} file.")
    }
  } else {
    cli::cli_abort("Please check you have a valid Tremendous API Key.
                   Visit {.url https://developers.tremendous.com/} for more details.")
  }
}

# Modified from the {boxr} package
# https://github.com/r-box/boxr/blob/28ccd2610922b53e0275d4d128f29781b92970e0/R/boxr_auth.R#L580
auth_message <- function(pw_info, key_type) {

  cli::cli_div(theme = list(ul = list(`list-style-type` = crayon::red(cli::symbol$bullet),
                                      `margin-left` = -2)))
  cli::cli_ul("You may wish to add your {key_type} to your {.file .Renviron} file:")
  cli::cli_text(crayon::silver(pw_info))
  if (requireNamespace("clipr", quietly = TRUE)) {
    clipr::write_clip(pw_info)
    cli::cli_text("[Copied to clipboard]")
  }
  cli::cli_ul("To edit your {.file .Renviron} file:")
  cli::cli_text("  - Check that {.pkg usethis} is installed.")
  cli::cli_text("  - Call {.code usethis::edit_r_environ()}.")
  cli::cli_text("  - Check that {.file .Renviron} ends with a new line.")
  cli::cli_end()

}
