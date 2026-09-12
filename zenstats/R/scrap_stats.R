#' Scrap statistics from a Zenodo deposit
#'
#' @param deposit_id numeric. Zenodo deposit id.
#' @param all_versions_only logical. If `TRUE`, return only the results for all versions.
#'
#' @return a tibble
#' @export
#'
#' @examplesIf identical(tolower(Sys.getenv("NOT_CRAN")), "true")
#' scrap_stats(10013255)
#'
scrap_stats <- function(deposit_id, all_versions_only = FALSE){
  # Assertions
  checkmate::assert_number(x = deposit_id)
  checkmate::assert_logical(x = all_versions_only)

  # Deposit URL
  url <- paste0("https://zenodo.org/records/", deposit_id)

  # Check internet
  if(!curl::has_internet()){
    cli::cli_alert_warning("There is no Internet connectivity. Please check your connection.")
    return(NULL)
  }

  # Check URL
  if(!RCurl::url.exists(url, timeout.ms = 5000)){
    cli::cli_alert_warning("The deposit is not available or the Zenodo website is down. Please manually check the following URL and try again.")
    cli::cli_alert("{url}")
    return(NULL)
  }

  # Read html
  zen_web <- polite::bow(url) |>
    polite::scrape()

  # Scrap statistics table
  zen_stats <- zen_web |>
    rvest::html_element(xpath = '//*[@id="record-statistics"]') |>
    rvest::html_table()

  # Prepare table
  res <- tibble::tibble(
    date = Sys.time(),
    deposit = rep(deposit_id, 2),
    version = c("All versions", "This version"),
    views = readr::parse_number(c(zen_stats[[1,2]], zen_stats[[1,3]])),
    downloads = readr::parse_number(c(zen_stats[[2,2]], zen_stats[[2,3]])),
    volume = rlang::parse_bytes(c(zen_stats[[3,2]], zen_stats[[3,3]]))
  )

  # Filter all versions only
  if(all_versions_only){
    res <- res[1,c(1,2,4:6)]
  }

  # Return
  return(res)
}
