#' Get collocations for word in corpus
#'
#' This function retrieves collocation data from a corpus using a given word and a list of unique identifiers (pids) of corpus data frame.
#'
#' @param pids A vector or data frame containing the unique identifiers of the texts in the corpus.
#' @param word The target word for which you want to find concordances.
#' @param before The number of words before the target word to include in the context (default is 10).
#' @param after The number of words after the target word to include in the context (default is 10).
#' @param sample_size The number of samples to retrieve from the API (default is 5000).
#'
#' @return A data frame of concordances.
#'
#' @import httr jsonlite
#' @export
#'
#' @examples
#'pids <- c("URN:NBN:no-nb_digibok_2008051404065", "URN:NBN:no-nb_digibok_2010092120011")
#'word <- "."
#'collocations <- get_collocations(pids, word)
#'
get_collocations <- function(pids, word, before=10, after=10, sample_size=5000){

  if (is.data.frame(pids)) {
    pids <- unname(pids$urn)
  } else {
    pids <- unname(pids)
  }

  url <- "https://api.nb.no/dhlab/urncolldist_urn"

  params <- list("urn" = pids, "word" = word, "before" = before, "after" = after, "samplesize" = sample_size)

  # query <- POST(url, body = params, encode = "json")
  query <- api_call_wrapper(url, body = params, encode = "json")

  if  (is.null(query)) {
    return(NULL)
  }

  return(as.data.frame(do.call(cbind, fromJSON(content(query)))))



}
