#' Retrieve full opioid medication list
#'
#' @returns
#' A `data.frame` with full list of opioid medication names (`med_name`) that are compatible with the MME calculator along with their conversion factors (`cf`).
#'
#' @export
#'
#' @seealso [search_meds()]
#'
#' @examples
#' get_med_list()
#'
get_med_list <- function() {
  # Base URL for the API
  base_url <- "https://research-mme.wakehealth.edu/api"

  req <- httr2::request(glue::glue("{base_url}/all")) |>
    httr2::req_headers(accept = "application/json") |>
    mmequiv_req_retry()

  resp <- req |>
    httr2::req_perform()

  resp |>
    httr2::resp_body_json(simplifyVector = TRUE)
}
