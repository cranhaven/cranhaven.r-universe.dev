#' Get Document Corpus
#'
#' Retrieve a corpus of documents based on the given parameters.
#'
#' @param doctype Character, document type (default: 'digibok')
#' @param author Character, author name (default: NULL)
#' @param ddk Character, Dewey Decimal Classification (default: NULL)
#' @param freetext Character, free text search (default: NULL)
#' @param subject Character, subject of the document (default: NULL)
#' @param from_timestamp Character, timestamp range start (default: NULL)
#' @param to_timestamp Character, timestamp range end (default: NULL)
#' @param publisher Character, publisher name (default: NULL)
#' @param limit Integer, maximum number of results (default: 10)
#' @param order_and_limit_by_rank Logical, order and limit results by rank (default: NULL)
#' @param title Character, title of the document (default: NULL)
#' @param from_year Integer, year range start (default: NULL)
#' @param to_year Integer, year range end (default: NULL)
#' @param fulltext Character, full text search (default: NULL)
#' @param lang Character, language code (default: 'nob')
#'
#' @return A data frame of metadata
#'
#' @examples
#'   get_document_corpus(doctype = 'digibok', author = 'Henrik Ibsen', limit = 2)
#'
#' @import httr
#' @export
get_document_corpus <- function(doctype='digibok', author=NULL, ddk=NULL, freetext=NULL, subject=NULL, from_timestamp=NULL, to_timestamp=NULL, publisher=NULL, limit=10, order_and_limit_by_rank=NULL, title=NULL, from_year=NULL, to_year=NULL, fulltext=NULL, lang='nob'){

    url <- "https://api.nb.no/dhlab/build_corpus"

    params <- list("author" = author, "ddk" = ddk, "doctype" = doctype, "freetext" = freetext, "from_timestamp" = from_timestamp, "from_year" = from_year, "fulltext" = fulltext, "lang" = lang, "limit" = limit, "order_and_limit_by_rank" = order_and_limit_by_rank, "publisher" = publisher, "subject" = subject, "title" = title, "to_timestamp" = to_timestamp, "to_year" = to_year)

    # query <- POST(url, body = params, encode = "json")
    query <- api_call_wrapper(url, body = params, encode = "json")

    if  (is.null(query)) {
      return(NULL)
    }


    return(as.data.frame(do.call(cbind, content(query))))

}
