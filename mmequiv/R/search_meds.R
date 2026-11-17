#' Search opioid medication list
#'
#' @param med_name A single string specifying the medication name to search for.
#'
#' @returns
#' A `data.frame` containing medications matching the `med_name` argument and
#'     their associated conversion factor(s) (`cf`).
#'
#' @export
#'
#' @seealso [get_med_list()]
#'
#' @examples
#' search_meds("oxy")
#'
search_meds <- function(med_name = NULL) {
  # Check if med_name argument is specified
  if (is.null(med_name)) {
    cli::cli_abort("{.arg med_name} must be specified to use {.fn search_meds}")
  }
  # Check fo valid inputs
  if (!is.character(med_name)) {
    cli::cli_abort(
      c(
        "{.arg med_name} must be a {.cls character} string",
        "x" = "You've supplied a {.cls {class(med_name)}} input"
      )
    )
  }
  if (length(med_name) != 1) {
    cli::cli_abort("{.arg med_name} only accepts a single string at a time")
  }

  # Base URL for the API
  base_url <- "https://research-mme.wakehealth.edu/api"

  req <- httr2::request(glue::glue("{base_url}/by_name")) |>
    httr2::req_url_query(med_name = med_name) |>
    httr2::req_headers(accept = "application/json") |>
    mmequiv_req_retry()

  resp <- req |>
    httr2::req_perform()

  resp |>
    httr2::resp_body_json(simplifyVector = TRUE)
}
