#' List all pages from logs monitoring
#'
#' @param projectId ID of your project
#'
#' @details
#' <http://developer.oncrawl.com/#Data-types>
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
#' @examples
#' \dontrun{
#' pages <- listLogs(YOURPROJECTID)
#' }
#' @return Json
#' @author Vincent Terrasi
#' @export
listLogs <- function(projectId) {

  KEY <- getOption('oncrawl_token')
  DEBUG <- getOption('oncrawl_debug')
  API <- getOption('oncrawl_api')

  if(nchar(KEY)<=10) {
    testConf <- initAPI()
    if(testConf!="ok") stop("No API Key detected")
  }

  curl <- RCurl::getCurlHandle()

  # get fields
  fieldsList <- getPageFieldsLogs(projectId)

  pageAPI <- paste0(API,"data/project/", projectId,"/log_monitoring/events", sep = "")

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
  } else {
    # return error if response.code!=200
    warning(reply)
  }

  return(csv)
}


#' List all available fields from logs
#'
#' @param projectId ID of your project
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
#' logsFields <- getFieldsLogs(YOURPROJECTID)
#' }
#'
#' @return Character Array
#' @author Vincent Terrasi
getPageFieldsLogs <- function(projectId) {

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

  #data/project/", projectId,"/log_monitoring/pages
  pageAPI <- paste0(API,"data/project/", projectId,"/log_monitoring/events/fields", sep = "")

  reply <- RCurl::getURL(pageAPI,
                         httpheader = hdr,
                         curl = curl,
                         verbose = DEBUG)

  info <- RCurl::getCurlInfo(curl)

  if (info$response.code==200) {
    # return ok if response.code==200
    res <- jsonlite::fromJSON(reply)

    # TODO :regex_to_hide_fields = "(_hash|google|gsc_|ati_|adobe|seo_)"

    fieldsDF <- data.frame(name=res$fields$name
                           ,type=res$fields$type
                           ,can_display=res$fields$can_display
                           ,can_filter=res$fields$can_filter
                           ,arity=res$fields$arity
                           ,stringsAsFactors=FALSE)

    # rmove : filter not found: inactive_indexable_pages_in_structure_logs']}

    fieldsDF <- dplyr::filter(fieldsDF
                              , .data$can_display == TRUE
    )

    fieldsDF <- dplyr::select(fieldsDF,.data$name)

    return(fieldsDF$name)

  } else {
    # return error if response.code!=200
    warning(reply)
  }

}
