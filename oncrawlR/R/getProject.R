#' Get a project
#'
#' @param projectId Id of your project
#'
#' @details
#' <http://developer.oncrawl.com/#Get-a-project>
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
#'The HTTP response is JSON object with three keys:
#'- A project key with the project’s data
#'- A crawl_configs key with a list of all the project’s crawl configurations.
#'- A crawls key with a list of all project’s crawl.
#'
#' @examples
#' \dontrun{
#' initAPI()
#' project <- getProject(YOURPROJECTID)
#' }
#' @return Json
#' @author Vincent Terrasi
#' @export
getProject <- function(projectId) {

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

  projectAPI <- paste0(API,"projects/",projectId)

  reply <- RCurl::getURL(projectAPI,
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
