#' Check Remaining Quota
#'
#' Query the API for the number of time series downloads remaining in the current month.
#' @inheritParams param_defs
#' @examples
#' try(get_remaining_quota("313984fcd9f343d3961891319b0ed321"))
#' @import httr
#' @export
get_remaining_quota <- function(api_key) {

  query = list(apikey = api_key)

  response <- validate_response(GET("https://datenservice.kof.ethz.ch/api/v1/main/remainingquota", query = query))

  fromJSON(content(response, as="text"))$quota
}
