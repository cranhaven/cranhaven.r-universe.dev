#' Fetch statistics from one or more Zenodo deposits
#'
#' This function will fetch statistics from multiple deposits, respecting web crawling policies from Zenodo's 'robots.txt' file.
#'
#' @param deposit_ids vector. A vector of Zenodo deposit ids.
#' @param all_versions_only logical. If `TRUE`, return only the results for all versions.
#' @param progress logical. Show a progress bar.
#'
#' @return a tibble.
#' @export
#'
#' @examplesIf identical(tolower(Sys.getenv("NOT_CRAN")), "true")
#' deposit_stats(c(10013255, 10889682), all_versions_only = TRUE)
deposit_stats <- function(deposit_ids, all_versions_only = FALSE, progress = TRUE){
  # Assertions
  checkmate::assert_vector(x = deposit_ids)
  checkmate::assert_logical(x = all_versions_only)
  checkmate::assert_logical(x = progress)

  # Map over ids, sequentially
  res <- deposit_ids |>
    purrr::map(
      scrap_stats,
      all_versions_only = all_versions_only,
      .progress = progress
    ) |>
    purrr::list_rbind()

  return(res)
}
