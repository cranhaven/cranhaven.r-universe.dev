
mnis_query <- function(query) {
  got <- httr::GET(query, httr::accept_json())

  if (httr::http_type(got) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  got <- tidy_bom(got)

  got <- jsonlite::fromJSON(got, flatten = TRUE)

  got
}
