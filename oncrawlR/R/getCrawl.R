#' Get a crawl
#'
#' @param crawlId Id of your crawl
#'
#' @details
#' <http://developer.oncrawl.com/#Get-a-crawl>
#'
#' ResCode
#' 400 : Returned when the request has incompatible values or does not match the API specification.
#' 401 : Returned when the request is not authenticated.
#' 403 : Returned the current quota does not allow the action to be performed.
#' 404 : Returned when any of resources referred in the request is not found.
#' 403 : Returned when the request is authenticated but the action is not allowed.
#' 409 : Returned when the requested operation is not allowed for current state of the resource.
#' 500 : Internal error
#'
#' The HTTP response is JSON object with a single crawl key containing the crawl’s data
#'
#' @examples
#' \dontrun{
#' initAPI()
#' project <- getCrawl("YOURCRAWLID")
#' }
#'
#' @return Json
#' @author Vincent Terrasi
#' @export
getCrawl <- function(crawlId) {

  KEY <- getOption('oncrawl_token')
  DEBUG <- getOption('oncrawl_debug')
  API <- getOption('oncrawl_api')

  if(nchar(KEY)<=10) {
    testConf <- initAPI()
    if(testConf!="ok") stop("No API Key detected")
  }

  curl <- RCurl::getCurlHandle()

  hdr  <- c(Accept="application/json"
            ,Authorization=paste("Bearer",KEY)
  )

  crawlAPI <- paste0(API,"crawls/",crawlId)

  reply <- RCurl::getURL(crawlAPI,
                  httpheader = hdr,
                  curl = curl,
                  verbose = DEBUG)

  info <- RCurl::getCurlInfo(curl)

  if (info$response.code==200) {
    # return ok if response.code==200
    res <- jsonlite::fromJSON(reply)
  } else {
    # return error if response.code!=200
    warning(reply)
  }

  return(res)
}
