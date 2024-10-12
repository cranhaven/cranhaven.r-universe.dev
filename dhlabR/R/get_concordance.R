#' Retrieve Concordance for Words in Documents
#'
#' This function obtains the concordance for specified words within given documents.
#'
#' @param pids A vector or data frame containing document IDs.
#' @param words A string of words (tokens) for which the concordance will be retrieved. For multiple tokens use keyword OR
#' @param window An optional numeric value specifying the number of characters before and after the matching word (default is 20).
#' @param limit An optional numeric value specifying the maximum number of results to return (default is 5000).
#'
#' @return A data frame containing the concordance results for each word in the specified documents. Returns NULL if the API request fails or no results are found.
#'
#' @import httr
#' @import jsonlite
#' @export
#'
#' @examples
#' document_ids <- c("URN:NBN:no-nb_digibok_2008051404065", "URN:NBN:no-nb_digibok_2010092120011")
#' tokens <- "Norge"
#' window <- 20
#' limit <- 1000
#' result <- get_concordance(document_ids, tokens, window, limit)
get_concordance <- function(pids, words, window=20, limit=10) {
  if (is.data.frame(pids)) {
    pids <- unname(pids$urn)
  } else {
    pids <- unname(pids)
  }

  url <- "https://api.nb.no/dhlab/conc"

  params <- list("urns" = pids, "query" = words, "window" = window, "limit" = limit)

  json_params <- jsonlite::toJSON(params, auto_unbox = TRUE)

  # query <- POST(url, body = json_params, encode = "raw", content_type("application/json"))
  query <- api_call_wrapper(url, body = json_params, encode = "raw", content_type("application/json"))

  if  (is.null(query)) {
    return(NULL)
  }

  if (http_status(query)$category != "Success") {
    warning("API request failed: ", http_status(query)$message)
    return(NULL)
  }

  query_content <- content(query)

  if (length(query_content) == 0) {
    warning("No concordance results found.")
    return(NULL)
  }

  return(as.data.frame(do.call(cbind, query_content)))
}
