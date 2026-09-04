#' Get Localized Meta Information
#'
#' Download metadata given a time series key.
#' @param ts_keys A vector of time series keys.
#' @param locale character ISO-2-digit language of requested metadata. Meta data
#' are for survey related typically available in English, German, French and Italian.
#' Non survey data are not provided with meta information, yet.
#' @return A named list of lists containing metadata.
#' @import httr
#' @import jsonlite
#' @examples
#' try(get_metadata("kofbarometer","en"))
#' @export
get_metadata <- function(ts_keys, locale=c("en", "de", "fr", "it")) {

  locale <- match.arg(locale)

  url <- "https://datenservice.kof.ethz.ch/api/v1/public/metadata"

  query = list(
    locale = locale,
    keys = paste(ts_keys, collapse = ",")
  )

  response <- GET(url, query = query)
  data <- fromJSON(content(response, as="text"))

  # Set empty lists to NA
  data[sapply(data, length) == 0] <- NA

  # Set metadata for missing keys to NA
  data[setdiff(ts_keys, names(data))] <- NA

  data
}
