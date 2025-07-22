#' Query Record API
#'
#' @description The Record API provides direct access to the Europeana data,
#' which is modeled using the Europeana Data Model (EDM). While EDM is an open
#' flexible data model featuring various kind of resources and relations
#' between them, the Record API (and the Europeana Collections Portal) supports
#' the retrieval of a segment of EDM for practical purposes.
#'
#' These "atomic" EDM segments typically contain one Cultural Heritage Object
#' (CHO), aggregation information that connects the metadata and digital
#' representations, and a number of contextual resources related to the CHO,
#' such as agents, locations, concepts, and time.
#'
#' @param id string with the `RECORD_ID` in the form of `/DATASET_ID/LOCAL_ID`
#' @param path string that indicates version of the API
#' @param ... other parameters passed as query parameters
#'
#' @returns S3 object of class `europeana_record_api`. Contains the parsed content,
#' the path, and the API response compatible with `httr` methods.
#'
#' @examplesIf Sys.getenv("EUROPEANA_KEY") != ""
#' \donttest{
#' #set your API key with set_key(api_key = "XXXX")
#' #query search API
#' res <- query_search_api("arioch", qf = "1712", media = TRUE)
#' #get results in tidy format
#' dat <- tidy_search_items(res)
#' #query records API for each item
#' lapply(dat$id, query_record_api)
#' }
#'
#' @importFrom Rdpack reprompt
#'
#' @source https://pro.europeana.eu/page/record
#'
#' @references
#' \insertRef{doerr2010europeana}{europeanaR}
#'
#' \insertRef{httr}{europeanaR}
#'
#' \insertRef{jsonlite}{europeanaR}
#'
#' @export
query_record_api <- function(id, path = "/record/v2", ...) {

  stopifnot(is.character(id))
  stopifnot(is.character(path))

  ua <- httr::user_agent("euRopeana (http://my.package.web.site)")

  wskey <- get_key()

  url <- httr::modify_url("https://api.europeana.eu",
                          path = paste0(path, id,  ".json"))

  resp <- httr::RETRY("GET", url, ua, query = list(wskey = wskey, ...))

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
    class = "europeana_record_api"
  )

}
