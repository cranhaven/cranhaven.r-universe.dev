#' Tidy search API response
#'
#' Transforms API response to a tidy `data.table` for easier manipulation
#'
#' @param resp an S3 object of type `europeana_search_api`
#'
#' @returns `data.table` with stacked results collected from the search api.
#' Each row corresponds to a Cultural Heritage Object.
#'
#' @import data.table magrittr
#'
#' @examplesIf Sys.getenv("EUROPEANA_KEY") != ""
#' \donttest{
#' #set your API key with set_key(api_key = "XXXX")
#' resp <- query_search_api("arioch")
#' tidy_search_items(resp)
#' }
#' @export
tidy_search_items <- function(resp) {

  stopifnot(inherits(resp, "europeana_search_api"))
  stopifnot("Status code is not OK" = resp$response$status_code == 200)
  stopifnot("No items found" = resp$content$itemsCount > 0)

  tidy_data <- lapply(resp$content$items, function(x) {
    res <- sapply(x, function(meta) {
      meta %>%
        unlist() %>%
        paste(collapse = " \n ")
    }) %>%
      data.table() %>%
      transpose() %>%
      setnames(names(x))
  }) %>%
    rbindlist(fill = TRUE)

}
