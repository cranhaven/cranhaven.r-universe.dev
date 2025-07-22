#' Europeana Search API
#'
#' @description The Search API allows to search the Europeana repository for
#' metadata records and media. The Search API is the most straightforward to
#' use. It works in a similar fashion to the Europeana website when it comes
#' to interacting with the data. You can use the API to search for keywords,
#' and it will return any entries that contain those words. You can refine your
#' search with more advanced queries like Boolean Searches, or you can filter
#' out parts of the results advanced filtering.
#'
#' @param query (character) string with the search term(s)
#' @param rows (numeric) that indicates the number of records to return
#' @param profile (character) Profile parameter controls the format and richness
#'  of the response.
#' @param qf (character) Facet filtering query. This parameter can be defined
#'  more than once.
#' @param reusability (character) Filter by copyright status.
#' @param media (character) Filter by records where an URL to the full media
#'  file is present in the edm:isShownBy or edm:hasView metadata and
#'  is resolvable.
#' @param thumbnail (character) Filter by records where a thumbnail image has
#'  been generated for any of the WebResource media resources (thumbnail
#'  available in the edmPreview field).
#' @param landingpage (character) Filter by records where the link to the
#'  original object on the providers website (edm:isShownAt) is present and
#'  verified to be working.
#' @param colourpalette (character) Filter by images where one of the colours
#'  of an image matches the provided colour code. You can provide this parameter
#'  multiple times, the search will then do an 'AND' search on all the provided
#'  colours. See colour palette.
#' @param facet (character) Name of an individual facet. See individual facets.
#' @param limit (numeric) The number of records to return. Maximum is 100. Default: 10
#' @param start (numeric) The item in the search results to start with. The first item is 1.
#' Default: 1
#' @param sort (character) Sort by a field, e.g., \code{timestamp_update+desc}
#' @param ... parameters passed in get request
#' @param path (character) URL signature with the API version
#'
#' @returns S3 object of class `europeana_search_api`. Contains the parsed content,
#' the path, and the API response compatible with `httr` methods.
#'
#' @details In the `query` parameter the Apache Lucene Query Syntax is inheritly
#' supported by the Search API. Users can use Lucene and Apache SOLR guides to
#' get the most out of the Europeana repository.
#'
#' The response is always formatted in JSON and will contain a
#' number of fields that present information about the handling of the
#' request, while the concrete information about the record is presented in
#' the "items" field (see Metadata Sets). The parsed information is stored in
#' the `content` field of the S3 object returned.
#'
#' @source https://pro.europeana.eu/page/search
#'
#' @importFrom Rdpack reprompt
#'
#' @references
#' \insertRef{doerr2010europeana}{europeanaR}
#'
#' \insertRef{httr}{europeanaR}
#'
#' \insertRef{jsonlite}{europeanaR}
#'
#'
#' @examplesIf Sys.getenv("EUROPEANA_KEY") != ""
#' \donttest{
#' #set your API key with set_key(api_key = "XXXX")
#' #query search API
#' res <- query_search_api("arioch", qf = "1712", media = TRUE)
#' }
#'
#' @export
query_search_api <- function(query = NULL,
                             rows = NULL,
                             profile = NULL,
                             qf = NULL,
                             reusability = NULL,
                             media = NULL,
                             thumbnail = NULL,
                             landingpage = NULL,
                             colourpalette = NULL,
                             facet = NULL,
                             limit = NULL,
                             start = NULL,
                             sort = NULL,
                             path = "/record/v2/search.json",
                             ...) {

  stopifnot(`Max rows are 100` = rows <= 100)

  ua <- httr::user_agent("europeanaR (https://github.com/AleKoure/europeanaR)")

  wskey <- get_key()

  url <- httr::modify_url("https://api.europeana.eu",
                    path = path)

  resp <- httr::RETRY("GET",
                      url,
                      ua,
                      query = list(query = query,
                                   rows = rows,
                                   wskey = wskey,
                                   profile = profile,
                                   qf = qf,
                                   reusability = reusability,
                                   media = media,
                                   thumbnail = thumbnail,
                                   landingpage = landingpage,
                                   colourpalette = colourpalette,
                                   facet = facet,
                                   limit = limit,
                                   start = start,
                                   sort = sort,
                                   ...))

  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(resp, "text"),
                               simplifyVector = FALSE)

  if (httr::http_error(resp)) {
    stop(
      sprintf(
        "Europeana search API request failed [%s]\n%s\n<%s>",
        httr::status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }

  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "europeana_search_api"
  )

}
