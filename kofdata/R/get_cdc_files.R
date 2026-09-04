#' @import httr
#' @import jsonlite
.get_cdc_files <- function(username, api_key) {
  url_template <- "https://datenservice.kof.ethz.ch/api/v1/user/%s/datasets"

  query <- list(apikey = api_key)

  listing_url <- sprintf(url_template, username)
  listing <- validate_response(GET(listing_url, query = query))
  files <- fromJSON(content(listing, as="text"))

  files
}
