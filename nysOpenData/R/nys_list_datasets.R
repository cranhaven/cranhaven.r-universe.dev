#' List datasets available in nysOpenData
#'
#' Retrieves the current Open NY catalog and returns datasets available
#' for use with `nys_pull_dataset()`.
#'
#' Keys are generated from dataset titles using `janitor::make_clean_names()`.
#'
#' @return A tibble of available datasets, including generated `key`, dataset
#'   `uid`, and dataset `title`.
#' @examples
#' if (interactive() && curl::has_internet()) {
#'   nys_list_datasets()
#' }
#' @importFrom rlang .data
#' @export
nys_list_datasets <- function() {
  .nys_catalog_tbl()
}

.nys_catalog_tbl <- function() {
  raw <- jsonlite::fromJSON("https://data.ny.gov/data.json", flatten = TRUE)$dataset |>
    tibble::as_tibble()

  raw |>
    dplyr::mutate(
      uid = stringr::str_sub(.data$landingPage,-9),
      key = janitor::make_clean_names(.data$title)) |>
    dplyr::filter(!is.na(.data$uid), nzchar(.data$uid)) |>
    dplyr::distinct(.data$uid, .keep_all = TRUE) |>
    dplyr::relocate( "key", "uid", "title")
}
