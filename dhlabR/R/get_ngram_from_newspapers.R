#' Get Ngram Count per Year for National Library Newspaper Collection
#'
#' This function queries the National Library's book collection API to retrieve
#' the ngram count per year for the specified parameters. It can be used to plot
#' an ngram based on the words' presence in books in the library's collection.
#'
#' @param city (character, optional) The city of publication. Default is NULL.
#' @param ddk (character, optional) The Dewey Decimal Classification (DDC) code. Default is NULL.
#' @param lang (character, optional) The language code of the books. Default is NULL.
#' @param period (list, optional) A list containing the start and end years of the period to search. Default is an empty list.
#' @param publisher (character, optional) The publisher's name. Default is NULL.
#' @param title (character, optional) The title or a part of the title of the books. Default is NULL.
#' @param topic (character, optional) A topic or subject associated with the books. Default is NULL.
#' @param word (list, optional) A list of words (ngrams) to search for in the books. Default is list("hus", "blokk").
#'
#' @return A data frame with the ngram count per year for the specified parameters.
#' @export
#'
#' @importFrom utils stack
get_ngram_from_newspapers <- function(city = NULL, ddk = NULL, lang = NULL, period = list(), publisher = NULL, title = NULL, topic = NULL, word = list("hus", "blokk")){

  url <- "https://api.nb.no/dhlab/ngram_newspapers"

  params <- list("city" = city, "ddk" = ddk, "lang" = lang, "period" = period, "publisher" = publisher, "title" = title, "topic" = topic, "word" = word)
  #query <- POST(ngram_newspapers, body = params, encode = "json")
  query <- api_call_wrapper(url, body = params, encode = "json")

  if  (is.null(query)) {
    return(NULL)
  }

  return(stack(content(query)))
}
