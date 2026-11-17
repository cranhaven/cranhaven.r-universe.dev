#' Check if API rate limit warning is needed
#'
#' @param n_patients Number of unique patients
#' @param use_api Whether API is being used
#' @param rate_limit The API rate limit (default: 50)
#' @return Invisible NULL, called for side effects (warnings)
#' @keywords internal
check_unique_pat <- function(n_patients, use_api, rate_limit = 50) {
  if (use_api && n_patients >= rate_limit) {
    cli::cli_alert_warning(
      "You are using the API with {n_patients} unique patient IDs",
      wrap = TRUE
    )
    cli::cli_alert_danger(
      "The NIH HEAL MME Online Calculator API has a rate limit of {rate_limit} patient-level requests per 15 minutes, so expect this to take some time!",
      wrap = TRUE
    )
    cli::cli_alert_info(
      "Consider setting {.code use_api = FALSE} to perform calculations locally instead",
      wrap = TRUE
    )
    cli::cli_alert_info(
      "Local calculations have no rate limit and don't require internet access",
      wrap = TRUE
    )
  }
  invisible(NULL)
}

#' Provide advisory message about potential API rate limit issues
#'
#' @param n_patients Number of unique patients
#' @param use_api Whether API is being used
#' @param rate_limit The API rate limit (default: 50)
#' @return Invisible NULL, called for side effects (informational messages)
#' @keywords internal
advise_api_usage <- function(n_patients, use_api, rate_limit = 50) {
  if (use_api && n_patients > 0 && n_patients < rate_limit) {
    cli::cli_alert_info(
      "You are using the API with {n_patients} unique patient IDs",
      wrap = TRUE
    )
    cli::cli_alert_info(
      "The NIH HEAL MME Online Calculator API has a rate limit of {rate_limit} patient-level requests per 15 minutes",
      wrap = TRUE
    )
    cli::cli_alert_info(
      "If you've recently made other API requests, you may still hit the rate limit",
      wrap = TRUE
    )
    cli::cli_alert_info(
      "Consider setting {.code use_api = FALSE} if you experience rate limit issues",
      wrap = TRUE
    )
  }
  invisible(NULL)
}
