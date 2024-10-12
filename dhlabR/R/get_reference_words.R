#' Retrieve Reference Words Count and Relative Frequency
#'
#' This function obtains the count and relative frequency of a vector of words within a year range for specified document types.
#'
#' @param doctype A character string indicating the document type. One of "digibok", "digavis", or "digitidsskrift".
#' @param from_year A numeric value indicating the starting year of the range.
#' @param to_year A numeric value indicating the ending year of the range.
#' @param words A vector of words for which the count and relative frequency will be retrieved.
#'
#' @return A list containing the count and relative frequency of the specified words within the given year range and document type.
#'
#' @import httr
#' @export
#'
#' @examples
#' doctype <- "digibok"
#' from_year <- 1900
#' to_year <- 2000
#' words <- c("og", "eller", "men")
#' result <- get_reference_words(doctype, from_year, to_year, words)
get_reference_words <- function(doctype = "digibok", from_year = 1990, to_year = 2000, words = NULL){

  url <- "https://api.nb.no/dhlab/reference_words"

  params <- list("doctype" = doctype, "from_year" = from_year, "to_year" = to_year, "words" = words)
  #query <- POST(url, body = params, encode = "json")
  query <- api_call_wrapper(url, body = params, encode = "json")

  if  (is.null(query)) {
    return(NULL)
  }

  return(content(query))

}
