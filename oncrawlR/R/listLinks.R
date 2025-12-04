#' List all links from a crawl
#'
#' @param crawlId ID of your crawl
#' @param originFilter select a specific source
#' @param targetFilter select a specific target
#'
#' @details
#' <http://developer.oncrawl.com/#Data-types>
#'
#' ResCode
#' 400 : Returned when the request has incompatible values or does not match the API specification.
#' 401 : Returned when the request is not authenticated.
#' 403 : Returned the current quota does not allow the action to be performed.
#' 404 : Returned when any of resource(s) referred in the request is not found.
#' 403 : Returned when the request is authenticated but the action is not allowed.
#' 409 : Returned when the requested operation is not allowed for current state of the resource.
#' 500 : Internal error
#'
#' @examples
#' \dontrun{
#' links <- listLinks(YOURCRAWLID)
#' }
#'
#' @return Json
#' @author Vincent Terrasi
#' @export
listLinks <- function(crawlId, originFilter="", targetFilter="") {

  KEY <- getOption('oncrawl_token')
  DEBUG <- getOption('oncrawl_debug')
  API <- getOption('oncrawl_api')

  if(nchar(KEY)<=10) {
    testConf <- initAPI()
    if(testConf!="ok") stop("No API Key detected")
  }

  curl <- RCurl::getCurlHandle()

  pageAPI <- paste0(API,"data/crawl/", crawlId,"/links", sep = "")

  hdr  <- c('Content-Type'="application/json"
            ,Authorization=paste("Bearer",KEY)
  )


  listJSON <- list("fields"=c(
    "origin",
    #"origin_ext",
    #"origin_first_path",
    #"origin_has_params",
    #"origin_host",
    #"origin_path",
    #"origin_querystring_key",
    #"origin_querystring_keyvalue",
    "target",
    #"target_ext",
    "target_fetched",
    #"target_first_path",
    #"target_has_params",
    #"target_host",
    #"target_path",
    #"target_querystring_key",
    #"target_querystring_keyvalue",
    "target_status",
    "target_status_range",
    "anchor",
    "follow",
    "intern",
    "juice"
  ),
  export="true")

  if (originFilter!="" && targetFilter=="") {
    listJSON <- c(listJSON, list(oql=list(field= c("origin","contains",originFilter))))
  }
  else if (originFilter=="" && targetFilter!="") {
    listJSON <- c(listJSON, list(oql=list(field= c("target","contains",targetFilter))))
  }
  else if (originFilter!="" && targetFilter!="") {
    #NOT IMPLEMENTED
    #listFilters <- list(field= c("origin","equals",originFilter))
    #listFilters <- c(listFilters, list(field= c("target","equals",targetFilter)))
    #listJSON <- c(listJSON, list(oql=list(and=listFilters)))
  }

  jsonbody <- jsonlite::toJSON(listJSON)

  reply <- RCurl::postForm(pageAPI,
                           .opts=list(httpheader=hdr, postfields=jsonbody),
                           curl = curl,
                           style = "POST"
  )

  info <- RCurl::getCurlInfo(curl)

  if (info$response.code==200) {
    # return ok if response.code==200
    csv <- read.csv(text = readLines(textConnection(reply)), sep = ";", header = TRUE)
  } else {
    # return error if response.code!=200
    warning(reply)
  }

  return(csv)
}
