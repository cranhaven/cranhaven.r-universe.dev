## negative of purr::keep("name")

toss <- function(.x, .p) {
  pat <- paste(paste0("^", .p, "$"), collapse = "|")
  return(.x[!grepl(pat, names(.x))])
}

fetch <- function(path) {
  if(!curl::has_internet()) {
    message("Internet connection is down")
    return(NULL)
  }
  r <- httr::GET(path)
  if(httr::http_error(r)) {
    message("Internal error or data source has moved. Please try again.")
    return(NULL)
  } else {
    return(XML::xmlParse(httr::content(r)))
  }
}
