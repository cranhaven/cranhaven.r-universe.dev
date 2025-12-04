#' List all pages from a crawl
#'
#' @param crawlId ID of your crawl
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
#' pages <- listPages(YOURCRAWLID)
#' }
#'
#' @return Json
#' @author Vincent Terrasi
#' @export
listPages <- function(crawlId) {

  KEY <- getOption('oncrawl_token')
  DEBUG <- getOption('oncrawl_debug')
  API <- getOption('oncrawl_api')

  if(nchar(KEY)<=10) {
    testConf <- initAPI()
    if(testConf!="ok") stop("No API Key detected")
  }

  curl <- RCurl::getCurlHandle()

  # get fields
  fieldsList <- getPageFields(crawlId)

  pageAPI <- paste0(API,"data/crawl/", crawlId,"/pages", sep = "")

  hdr  <- c('Content-Type'="application/json"
            ,Authorization=paste("Bearer",KEY)
  )

  jsonbody <- jsonlite::toJSON(list("fields"=fieldsList,export=TRUE))

  reply <- RCurl::postForm(pageAPI,
                    .opts=list(httpheader=hdr, postfields=jsonbody),
                    curl = curl,
                    style = "POST"
  )

  info <- RCurl::getCurlInfo(curl)

  if (info$response.code==200) {
    # return ok if response.code==200
    csv <- read.csv(text = readLines(textConnection(reply)), sep = ";", header = TRUE)

    if ( any(grepl("adobe_analytics_entrances_seo_google",names(csv))) ) {
      csv$analytics_entrances_seo_google <- 0
      csv$analytics_entrances_seo_google <- csv$adobe_analytics_entrances_seo_google
    }

    if ( any(grepl("ati_entrances_seo_google",names(csv))) ) {
      csv$analytics_entrances_seo_google <- 0
      csv$analytics_entrances_seo_google <- csv$ati_entrances_seo_google
    }

    if ( any(grepl("google_analytics_entrances_seo_google",names(csv))) ) {
      csv$analytics_entrances_seo_google <- 0
      csv$analytics_entrances_seo_google <- csv$google_analytics_entrances_seo_google
    }

  } else {
    # return error if response.code!=200
    warning(reply)
  }

  return(csv)
}

#' List all available fields from a crawl
#'
#' @param crawlId ID of your crawl
#'
#' @details
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
#' pages <- getFields(YOURCRAWLID)
#' }
#'
#' @return Character Array
#' @author Vincent Terrasi
getPageFields <- function(crawlId) {

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

  pageAPI <- paste0(API,"data/crawl/", crawlId,"/pages/fields", sep = "")

  reply <- RCurl::getURL(pageAPI,
                         httpheader = hdr,
                         curl = curl,
                         verbose = DEBUG)

  info <- RCurl::getCurlInfo(curl)

  if (info$response.code==200) {
    # return ok if response.code==200
    res <- jsonlite::fromJSON(reply)

    # disable some fields : high volume, ..
    fieldsDF <- data.frame(name=res$fields$name
                           ,type=res$fields$type
                           ,can_display=res$fields$can_display
                           ,can_filter=res$fields$can_filter
                           ,arity=res$fields$arity
                           ,stringsAsFactors=FALSE)

    fieldsDF <- dplyr::filter(fieldsDF
                              , .data$can_display == TRUE
                              , .data$can_filter == TRUE
                              , .data$type != "object"
                              , .data$arity != "many"
                              )

    fieldsDF <- dplyr::select(fieldsDF,.data$name)

    return(fieldsDF$name)

  } else {
    # return error if response.code!=200
    warning(reply)
  }


}


#' Get Aggregate Queries for a specific OQL
#'
#' @param crawlId ID of your crawl
#' @param oqlList json of your OQL
#'
#' @details
#' <http://developer.oncrawl.com/#OnCrawl-Query-Language>
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
#' agg <- listPagesAggs(YOURCRAWLID, YOURJSON)
#' page_crawled = agg[[1]]$rows
#' }
#'
#' @return Json
#' @author Vincent Terrasi
#' @export
listPagesAggs <- function(crawlId,oqlList) {

  KEY <- getOption('oncrawl_token')
  DEBUG <- getOption('oncrawl_debug')
  API <- getOption('oncrawl_api')

  if(nchar(KEY)<=10) {
    testConf <- initAPI()
    if(testConf!="ok") stop("No API Key detected")
  }

  curl <- RCurl::getCurlHandle()

  pageAPI <- paste0(API,"data/crawl/", crawlId,"/pages/aggs", sep = "")

  hdr  <- c('Content-Type'="application/json"
            ,Authorization=paste("Bearer",KEY)
  )

  reply <- RCurl::postForm(pageAPI,
                           .opts=list(httpheader=hdr, postfields=oqlList),
                           curl = curl,
                           style = "POST"
  )

  info <- RCurl::getCurlInfo(curl)

  if (info$response.code==200) {
    # return ok if response.code==200
    json <- rjson::fromJSON(reply)
    data <- json$aggs

  } else {
    # return error if response.code!=200
    warning(reply)
  }

  return(data)
}

