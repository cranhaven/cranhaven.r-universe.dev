#' Retrieve Token Frequencies in Documents
#'
#' This function obtains token frequencies within specified documents.
#'
#' @param pids A vector or data frame containing document IDs.
#' @param cutoff A numeric value specifying the frequency cutoff for tokens.
#' @param words A vector of words (tokens) to retrieve frequencies for.
#'
#' @return A list containing the following elements for each document:
#' - Document ID
#' - Token
#' - Token frequency in the document
#' - Total tokens in the document
#'
#' @import httr
#' @export
#'
#' @examples
#' document_ids <- c("URN:NBN:no-nb_digibok_2008051404065", "URN:NBN:no-nb_digibok_2010092120011")
#' frequency_cutoff <- 10
#' tokens <- c(".", ",", "men")
#' result <- get_document_frequencies(document_ids, frequency_cutoff, tokens)
get_document_frequencies <- function(pids, cutoff=0, words=NULL) {
  if (is.data.frame(pids)) {
    pids <- unname(pids$urn)
  } else {
    pids <- unname(pids)
  }

  url <- "https://api.nb.no/dhlab/frequencies"

  # Initialize the params list with mandatory parameters
  params <- list("urns" = pids, "cutoff" = cutoff)

  # Add the 'words' parameter, use an empty list if it's NULL
  params$words <- if (is.null(words)) list() else words

  # Use jsonlite's toJSON function to properly encode the parameters
  json_params <- jsonlite::toJSON(params, auto_unbox = TRUE)

  # Send the request with the JSON-encoded parameters
  #query <- httr::POST(url, body = json_params, encode = "raw", httr::content_type("application/json"))
  query <- api_call_wrapper(url, body = json_params, encode = "json", content_type("application/json"))

  if  (is.null(query)) {
    return(NULL)
  }

  # Get the content of the query as a list
  query_content <- httr::content(query, as = "parsed", simplifyDataFrame = FALSE)

  # Normalize depending on returned shape
  if (is.null(words) || length(words) == 0) {
    # Flatten the nested list by one level
    flattened_list <- do.call(c, query_content)
    # Use rbind to convert the flattened list to a data frame
    result_df <- do.call(rbind, flattened_list)
  } else {
    # Use rbind to convert the list to a data frame directly
    result_df <- do.call(rbind, query_content)
  }

  return(as.data.frame(result_df))
}

