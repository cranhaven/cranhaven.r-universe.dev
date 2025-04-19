#' Search for the n most popular places in a city according to Tripadvisor
#'
#' @param ciudad City or area where you want the places' information
#' @param n.resultados Maximum number of results to retrieve. If not specified, all results will be retrieved
#'
#' @return Character vector with the names of the most popular places of the searched city or area according to Tripadvisor
#' @export
#'
#' @examples
#'
#' \dontrun{
#' tripadvisor_places("Pinamar, Argentina", 10)
#' }
tripadvisor_places <- function(ciudad, n.resultados = Inf) {
  if (missing(ciudad)) {
    stop('"ciudad" must be specified')
  }
  remDr$open(silent = TRUE) # abre firefox
  remDr$navigate("https://www.google.com.ar")
  webElem <- remDr$findElement(using = "name", value = "q")
  webElem$sendKeysToElement(list(paste("restaurantes tripadvisor", ciudad, sep = " ")))
  webElem <- remDr$findElement(using = "css selector", value = "div.tfB0Bf:nth-child(7) > center:nth-child(2) > input:nth-child(2)")
  Sys.sleep(1)
  webElem$clickElement()
  tripadvisor <- as.character(remDr$getCurrentUrl()[[1]])
  n <- gregexpr(pattern = "-", tripadvisor)[[1]][2]
  restaurantes <- c()
  rest.num <- 30
  x <- 1
  while ((remDr$getCurrentUrl()[[1]] != tripadvisor | x == 1) & length(restaurantes) < n.resultados) {
    source <- c(1, 2)
    while (length(source) == 2) {
      source <- remDr$getPageSource()[[1]]
    }
    restaurantes <- c(restaurantes, qdapRegex::ex_between(source, "<!-- -->. <!-- -->", "</a>")[[1]])
    remDr$navigate(paste(substr(tripadvisor, 1, n - 1), "-oa", rest.num, substr(tripadvisor, n, nchar(tripadvisor)), sep = ""))
    x <- x + 1
    rest.num <- rest.num + 30
  }
  remDr$close()
  if (n.resultados < Inf) {
    restaurantes[1:n.resultados]
  } else {
    restaurantes
  }
}
