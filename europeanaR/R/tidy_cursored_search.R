#' Tidy Cursored Search
#'
#' @description This function is a "runner" of a particular query that consequently makes
#' API requests until maximum items are reached or all related items have been
#' collected.
#'
#' @param query string with the search term(s)
#' @param max_items numeric that indicates max items collected
#' @param ... params passed to get request, see also `query_search_api()`
#'
#' @returns S3 object of type `cursored_search`. Contains a `data.table` with
#' all the responses transformed to tabular format, the path to the first
#' request that starts the cursored search, and the corresponding response object
#' compatible with `httr` methods.
#'
#' @examplesIf Sys.getenv("EUROPEANA_KEY") != ""
#' \donttest{
#' #set your API key with set_key(api_key = "XXXX")
#' #query search API up to 3 items
#' res <- tidy_cursored_search(query = "animal",
#'                             max_items = 3,
#'                             theme = "art",
#'                             media = TRUE)
#' head(res$data[, 1:3])
#' }
#'
#' @importFrom utils head
#' @export
tidy_cursored_search <- function(query, max_items = 1e4, ...) {

  responses <- list()
  cursor <- "*"
  while (!is.null(cursor)) {
    res <- query_search_api(query, cursor = cursor, ...)
    if (isTRUE(length(res$content$items) == 0)) {
      cursor <- NULL
      next ()
    }
    responses[[cursor]] <- res
    item_length <- sapply(responses, function(x) {
      length(x$content$items)
    })
    if (sum(item_length, na.rm = TRUE) >= max_items) {
      cursor <- NULL
      next ()
    }
    cursor <- res$content$nextCursor
  }

  tidy_item_list <- lapply(responses, function(europeana_res) {
    tidy_search_items(europeana_res)
  })

  res <- rbindlist(tidy_item_list, fill = TRUE)

  structure(
    list(
      data = head(res, max_items),
      url = responses[[1]]$response$url,
      response = responses[[1]]
    ),
    class = "cursored_search"
  )

}
